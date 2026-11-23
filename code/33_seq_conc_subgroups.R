# Robustness (Tables A8, A9, A12):
# (a) sequential vs concurrent arm as internal replication;
# (b) the self-administered - CAPI gap on mn01/mn02 by East/West and education;
# (c) straightlining by arm, the layout objection: CAPI read the items out in
#     rotated order, web and paper showed a fixed-order matrix, and the
#     rotation order is not recorded.
# Subgroup results are underpowered and exploratory, not nulls.

source(here::here("code", "helpers.R"))

d23 <- read_rds(clean("allbus_mn.rds")) |> filter(year == 2023)
w <- boot_weights(d23, "region")

share <- \(item) as.numeric(d23[[item]] >= 5)

# (a) Sequential - concurrent, all items
seq_conc <- tibble(item = mn_items) |>
  mutate(res = map(item, \(i) {
    boot_summary(boot_mean(share(i), d23$arm == "Sequential", w) -
                   boot_mean(share(i), d23$arm == "Concurrent", w))
  })) |>
  unnest(res)

save_tab(seq_conc, "tableA8_seq_vs_conc")

# (b) Mode gap by subgroup
subgroups <- bind_rows(
  tibble(dimension = "Region", level = c("West", "East")),
  tibble(dimension = "Education", level = levels(d23$educ3))
) |>
  expand_grid(item = c("mn01", "mn02")) |>
  mutate(res = pmap(list(dimension, level, item), \(dim, lev, i) {
    in_group <- if (dim == "Region") d23$region == lev else d23$educ3 %in% lev
    boot_summary(boot_mean(share(i), in_group & d23$self_admin, w) -
                   boot_mean(share(i), in_group & d23$arm == "CAPI", w)) |>
      mutate(n = sum(in_group & !is.na(d23[[i]])))
  })) |>
  unnest(res)

save_tab(subgroups, "tableA12_subgroups")

# (c) Straightlining: same answer on all nine items; within-respondent SD across them
answers <- d23 |> select(all_of(mn_items))
complete <- complete.cases(answers)
item_sd <- pmap_dbl(answers, \(...) sd(c(...)))
straight <- as.numeric(item_sd == 0)

straightlining <- expand_grid(arm = arm_levels, outcome = c("straightlining", "within_sd")) |>
  mutate(res = map2(arm, outcome, \(a, o) {
    y <- if (o == "straightlining") straight else item_sd
    boot_summary(boot_mean(y, complete & d23$arm == a, w)) |>
      mutate(n = sum(complete & d23$arm == a))
  })) |>
  unnest(res)

save_tab(straightlining, "tableA9_straightlining")

list(seq_conc, subgroups, straightlining) |>
  map(\(x) mutate(x, across(c(estimate, lower, upper), \(v) round(100 * v, 1))) |>
        select(-se)) |>
  walk(print)
