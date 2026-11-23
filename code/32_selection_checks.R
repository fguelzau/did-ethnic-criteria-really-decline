# Robustness: selection and clustering (Tables A1d-e, A2f, A12b). 2023 arms,
# German citizens:
# (a) respondent composition by arm and the descent contrasts with the
#     self-administered arms raked to the CAPI arm's composition;
# (b) the design effect by birth cohort;
# (c) a bootstrap that resamples interviewer-connected clusters of points,
#     because 55 CAPI interviewers work in more than one point;
# (d) correlations of 'born' with other items by arm.
# Municipality size (gkpol) is not released for 2023, so balance uses age, sex,
# education and region.

source(here::here("code", "helpers.R"))

d23 <- read_rds(clean("allbus_mn.rds")) |>
  filter(year == 2023) |>
  mutate(cohort = cut(2023 - age, c(-Inf, 1964, 1984, Inf),
                      labels = c("Born before 1965", "Born 1965-1984", "Born 1985 or later")))

w <- boot_weights(d23, "region")
capi <- d23$arm == "CAPI"
sa <- d23$self_admin
share <- \(item) as.numeric(d23[[item]] >= 5)

# (a) Balance and reweighting ---------------------------------------------------

balance_vars <- c("age_group", "female", "educ3", "region")

balance <- map(balance_vars, \(v) {
  x <- as.character(d23[[v]])
  tibble(variable = v, level = sort(unique(na.omit(x)))) |>
    mutate(res = map(level, \(l) {
      y <- as.numeric(x == l)
      m <- map(set_names(arm_levels), \(a) boot_mean(y, d23$arm == a, w)[1])
      boot_summary(boot_mean(y, sa, w) - boot_mean(y, capi, w)) |>
        mutate(CAPI = m$CAPI, Sequential = m$Sequential, Concurrent = m$Concurrent, .before = 1)
    }))
}) |>
  list_rbind() |>
  unnest(res)

# Rake the self-administered arms to the CAPI arm's weighted margins (iterative
# proportional fitting), separately in every replicate, so the uncertainty of
# the targets enters the intervals. Complete cases on the raking variables.
complete <- d23 |> select(all_of(balance_vars)) |> complete.cases()
rake_vars <- map(balance_vars, \(v) as.character(d23[[v]]))

rake_sa <- function(wj, iterations = 25) {
  wj[!complete] <- 0
  for (k in seq_len(iterations)) {
    for (x in rake_vars) {
      target <- tapply(wj[capi], x[capi], sum) / sum(wj[capi])
      current <- tapply(wj[sa], x[sa], sum) / sum(wj[sa])
      factor <- (target / current)[x]
      wj[sa] <- wj[sa] * replace_na(factor[sa], 1)
    }
  }
  wj
}

w_raked <- apply(w, 2, rake_sa)
w_complete <- w * complete

reweighted <- expand_grid(item = c("mn01", "mn02"),
                          weights = c("Unadjusted (complete cases)", "Raked to CAPI composition")) |>
  mutate(res = map2(item, weights, \(i, wt) {
    wm <- if (str_starts(wt, "Raked")) w_raked else w_complete
    boot_summary(boot_mean(share(i), sa, wm) - boot_mean(share(i), capi, wm))
  })) |>
  unnest(res)

# Check: the raked arms reproduce the CAPI margins
walk(rake_vars, \(x) {
  gap <- max(abs(tapply(w_raked[sa, 1], x[sa], sum) / sum(w_raked[sa, 1]) -
                   tapply(w_raked[capi, 1], x[capi], sum) / sum(w_raked[capi, 1])))
  check_close(gap, 0, tol = 1e-4, what = "raked margin vs CAPI margin")
})

# (b) Birth cohort --------------------------------------------------------------
# Adult before the 2000 reform vs socialised after it. Interaction: gap in the
# youngest minus gap in the oldest cohort.
cohort_gap <- \(i, c) boot_mean(share(i), sa & d23$cohort %in% c, w) -
  boot_mean(share(i), capi & d23$cohort %in% c, w)

cohorts <- expand_grid(item = c("mn01", "mn02"), cohort = levels(d23$cohort)) |>
  mutate(res = map2(item, cohort, \(i, c) {
    boot_summary(cohort_gap(i, c)) |>
      mutate(capi = boot_mean(share(i), capi & d23$cohort %in% c, w)[1],
             n = sum(d23$cohort %in% c & !is.na(d23[[i]])))
  })) |>
  unnest(res) |>
  bind_rows(
    tibble(item = c("mn01", "mn02"), cohort = "Difference: 1985 or later minus before 1965") |>
      mutate(res = map(item, \(i) boot_summary(cohort_gap(i, "Born 1985 or later") -
                                                 cohort_gap(i, "Born before 1965")))) |>
      unnest(res)
  )

# (c) Interviewer-connected clusters --------------------------------------------
# Points that share a CAPI interviewer are merged into one cluster (connected
# components of the point-interviewer graph). Resampling these clusters keeps
# every interviewer's respondents together.
links <- d23 |> filter(capi, !is.na(interviewer)) |> distinct(point, interviewer)
cluster_of <- set_names(seq_along(unique(d23$point)), unique(d23$point))
repeat {
  merged <- links |>
    mutate(cl = cluster_of[as.character(point)]) |>
    mutate(cl_min = min(cl), .by = interviewer) |>
    summarise(cl_new = min(cl_min), .by = point)
  updated <- cluster_of
  updated[as.character(merged$point)] <- merged$cl_new
  # propagate: every point takes the minimum label of its cluster
  updated <- set_names(ave(updated, updated, FUN = min), names(updated))
  if (identical(updated, cluster_of)) break
  cluster_of <- updated
}
cluster_of <- set_names(match(cluster_of, unique(cluster_of)), names(cluster_of))
d23 <- d23 |> mutate(cluster = cluster_of[as.character(point)])

cluster_sizes <- d23 |>
  summarise(points = n_distinct(point), n = n(), regions = n_distinct(region), .by = cluster)
interviewer_points <- links |> count(interviewer, name = "points") |> count(points, name = "interviewers")

# Clusters can span East and West, so the cluster bootstrap is unstratified
w_cluster <- d23 |>
  mutate(psu = str_c("cl_", cluster), all = "all") |>
  boot_weights("all")

clustered <- expand_grid(item = c("mn01", "mn02"),
                         bootstrap = c("Sample points within region", "Interviewer-connected clusters")) |>
  mutate(res = map2(item, bootstrap, \(i, b) {
    wm <- if (str_starts(b, "Interviewer")) w_cluster else w
    boot_summary(boot_mean(share(i), sa, wm) - boot_mean(share(i), capi, wm))
  })) |>
  unnest(res)

# (d) Correlations of 'born' by arm ----------------------------------------------
corr_items <- c("ma04", "mp02")
d23 <- d23 |>
  left_join(read_dta(raw("ZA8833_v1-0-0.dta"), col_select = c(respid, all_of(corr_items))) |>
              mutate(across(all_of(corr_items), as_item)), by = "respid")

wcor <- function(x, y, wt) {
  ok <- !is.na(x) & !is.na(y)
  cov.wt(cbind(x[ok], y[ok]), wt = wt[ok], cor = TRUE)$cor[1, 2]
}

correlations <- expand_grid(with = c("mn02", setdiff(mn_items, c("mn01", "mn02")), corr_items),
                            group = c("CAPI", "Self-administered")) |>
  mutate(r = map2_dbl(with, group, \(v, g) {
    keep <- if (g == "CAPI") capi else sa
    wcor(d23$mn01[keep], d23[[v]][keep], d23$weight[keep])
  })) |>
  pivot_wider(names_from = group, values_from = r)

save_tab(balance, "tableA1d_balance")
save_tab(reweighted, "tableA1e_reweighted")
save_tab(cohorts, "tableA12b_cohorts")
save_tab(clustered, "tableA1e_cluster_bootstrap")
save_tab(correlations, "tableA2f_born_correlations")

print(balance |> mutate(across(where(is.numeric), \(x) round(100 * x, 1))), n = Inf)
print(reweighted |> mutate(across(where(is.numeric), \(x) round(100 * x, 1))))
print(cohorts |> mutate(across(c(estimate:upper, capi), \(x) round(100 * x, 1))))
print(interviewer_points); print(arrange(cluster_sizes, desc(n)) |> head(5)); print(nrow(cluster_sizes))
print(clustered |> mutate(across(where(is.numeric), \(x) round(100 * x, 1))))
print(correlations |> mutate(across(where(is.numeric), \(x) round(x, 2))))
