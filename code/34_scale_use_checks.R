# Robustness: typology, scale use and specificity (Tables A2d-e, A3a-c, A4, A8b).
# 2023 arms, German citizens, ITT contrasts pooled self-administered minus CAPI,
# bootstrap over sample points.
# (a) Typology and scale use: does the neither -> both shift survive
#     when the civic threshold moves with the arm's civic scale use? Run for the
#     Diehl-Tucci ethnic index (born + descent) and for descent alone, since
#     'born in Germany' also admits a jus soli reading.

source(here::here("code", "helpers.R"))

d23 <- read_rds(clean("allbus_mn.rds")) |>
  filter(year == 2023) |>
  mutate(
    descent_mean = (mn01 + mn02) / 2,
    civic_mean = rowMeans(pick(all_of(typology_civic))),
    civic_hi = civic_mean >= dt_cutoff
  ) |>
  filter(!is.na(descent_mean), !is.na(civic_mean))

ethnic_indices <- list(
  `Born + descent` = d23$descent_mean >= dt_cutoff,
  `Descent only` = d23$mn02 >= dt_cutoff
)

w <- boot_weights(d23, "region")
capi <- d23$arm == "CAPI"
sa <- d23$self_admin

# Apply a statistic to the full-sample weights and each replicate
boot_stat <- function(f) map_dbl(seq_len(ncol(w)), \(j) f(w[, j]))
wmean <- function(y, keep, wj) sum(wj[keep] * y[keep]) / sum(wj[keep])

# Civic threshold set per arm so that the self-administered arms have the
# CAPI arm's civic share (weighted quantile; cases tied at the cut-off get a
# fractional membership so the share matches exactly). The descent threshold
# stays at 6. If the extra descent-minded respondents are still mostly "both",
# the pattern is not produced by civic answers drifting across the threshold.
civic_membership <- function(x, keep, target, wj) {
  vals <- sort(unique(x[keep]), decreasing = TRUE)
  above <- map_dbl(vals, \(v) sum(wj[keep & x > v])) / sum(wj[keep])
  at <- map_dbl(vals, \(v) sum(wj[keep & x == v])) / sum(wj[keep])
  i <- which(above + at >= target)[1]
  frac <- (target - above[i]) / at[i]
  case_when(x > vals[i] ~ 1, x == vals[i] ~ frac, .default = 0)
}

type_shares <- function(ethnic_hi, wj, matched = TRUE) {
  civ <- as.numeric(d23$civic_hi)
  if (matched) {
    target <- wmean(d23$civic_hi, capi, wj)
    civ <- if_else(capi, civ, civic_membership(d23$civic_mean, sa, target, wj))
  }
  eth <- as.numeric(ethnic_hi)
  types <- list(`Mainly ethnic` = eth * (1 - civ), Both = eth * civ,
                `Mainly civic` = (1 - eth) * civ, Neither = (1 - eth) * (1 - civ))
  map_dbl(types, \(y) wmean(y, sa, wj) - wmean(y, capi, wj))
}

typology_checks_for <- function(ethnic_hi) {
  # (a1) Strong descent endorsement within civic strata. Civic endorsement is
  # itself affected by mode, so this conditions on a post-treatment variable and
  # is read as descriptive, not causal.
  conditional <- expand_grid(civic = c(TRUE, FALSE)) |>
    mutate(res = map(civic, \(cv) {
      keep <- d23$civic_hi == cv
      boot_summary(boot_stat(\(wj) wmean(ethnic_hi, sa & keep, wj) -
                               wmean(ethnic_hi, capi & keep, wj))) |>
        mutate(capi = wmean(ethnic_hi, capi & keep, w[, 1]),
               sa = wmean(ethnic_hi, sa & keep, w[, 1]), .before = 1)
    })) |>
    unnest(res) |>
    mutate(check = "Strong descent share, within civic stratum",
           civic = if_else(civic, "Civic mean >= 6", "Civic mean < 6"))

  # (a2) Typology shift, fixed and matched civic threshold
  shifts <- c(`Typology, fixed threshold` = FALSE,
              `Typology, civic threshold matched to CAPI share` = TRUE) |>
    imap(\(m, check) {
      map(seq_len(ncol(w)), \(j) type_shares(ethnic_hi, w[, j], matched = m)) |>
        list_transpose() |>
        imap(\(x, type) boot_summary(unlist(x)) |> mutate(type = type, .before = 1)) |>
        list_rbind() |>
        mutate(check = check)
    }) |>
    list_rbind()

  # (a3) Within-respondent descent minus civic mean (scale points; born + descent
  # only), and the share of strong descent endorsers who are also strongly civic
  # ("four out of five")
  within <- list(
    `Descent minus civic mean (scale points)` = \(wj) {
      y <- d23$descent_mean - d23$civic_mean
      wmean(y, sa, wj) - wmean(y, capi, wj)
    },
    `Share of strong descent endorsers also civic, SA` = \(wj) {
      wmean(d23$civic_hi, sa & ethnic_hi, wj)
    },
    `Share of strong descent endorsers also civic, CAPI` = \(wj) {
      wmean(d23$civic_hi, capi & ethnic_hi, wj)
    }
  ) |>
    imap(\(f, check) boot_summary(boot_stat(f)) |> mutate(check = check, .before = 1)) |>
    list_rbind()

  bind_rows(conditional, shifts, within)
}

typology_checks <- ethnic_indices |>
  imap(\(ethnic_hi, index) typology_checks_for(ethnic_hi) |> mutate(ethnic_index = index)) |>
  list_rbind() |>
  filter(!(ethnic_index == "Descent only" & str_starts(check, "Descent minus"))) |>
  relocate(ethnic_index, check, civic, type)

save_tab(typology_checks, "tableA4_typology_checks")
print(typology_checks |> mutate(across(c(estimate:upper, capi, sa), \(x) round(x, 3))), n = Inf, width = 200)

# (b) Specificity tests --------------------------------------------------------
# All 2023 citizens (not only those with both typology indices), as in Table A2.
# Items of the item gradient (22_item_gradient.R) for the difference-in-differences
conduct_items <- c("ma01b", "ma02", "ma03", "ma04", "mp02", "mp04", "mp06", "mp07")
gradient_items <- names(placebo_labels)

d23 <- read_rds(clean("allbus_mn.rds")) |>
  filter(year == 2023) |>
  left_join(
    read_dta(raw("ZA8833_v1-0-0.dta"), col_select = c(respid, all_of(gradient_items))) |>
      mutate(across(all_of(gradient_items), as_item)),
    by = "respid"
  )

w <- boot_weights(d23, "region")
capi <- d23$arm == "CAPI"

cutpoints <- list(
  share_ge2 = \(x) as.numeric(x >= 2), share_ge5 = \(x) as.numeric(x >= 5),
  share_ge6 = \(x) as.numeric(x >= 6), share_7 = \(x) as.numeric(x == 7),
  mean = \(x) x
)

# Self-administered minus CAPI, full sample and replicates, per item and outcome
gap <- \(item, outcome) {
  y <- cutpoints[[outcome]](d23[[item]])
  boot_mean(y, d23$self_admin, w) - boot_mean(y, capi, w)
}

# (b1) mn items under alternative cut points and the mean
cutpoint_gaps <- expand_grid(item = mn_items, outcome = names(cutpoints)) |>
  mutate(res = map2(item, outcome, \(i, o) boot_summary(gap(i, o)))) |>
  unnest(res)

# Same sample as Table A2: the 5-7 contrasts must match it
read_csv(here(path_tab, "tableA2_design_effects.csv"), show_col_types = FALSE) |>
  filter(outcome == "share_ge5", contrast == "Self-administered") |>
  select(item, expected = estimate) |>
  inner_join(cutpoint_gaps |> filter(outcome == "share_ge5"), by = "item") |>
  pwalk(\(item, expected, estimate, ...) {
    check_close(estimate, expected, 1e-8, glue::glue("cut point 5-7 matches Table A2 ({item})"))
  })

# (b2) Differences in design effects: descent item minus the mean effect on the
# civic typology items, on all other mn items, and on the conduct-related
# migrant items (demands on foreigners and negative statements)
comparison_sets <- list(
  `Civic (typology items)` = typology_civic,
  `Other criteria (mn03-mn09)` = setdiff(mn_items, c("mn01", "mn02")),
  `Conduct-related migrant items` = conduct_items
)

did <- expand_grid(item = c("mn01", "mn02"), comparison = names(comparison_sets),
                   outcome = c("share_ge5", "share_7", "mean")) |>
  filter(!(comparison == "Conduct-related migrant items" & outcome == "share_7")) |>
  mutate(res = pmap(list(item, comparison, outcome), \(i, cmp, o) {
    reference <- map(comparison_sets[[cmp]], \(j) gap(j, o)) |> reduce(`+`)
    boot_summary(gap(i, o) - reference / length(comparison_sets[[cmp]]))
  })) |>
  unnest(res)

# (b3) Multiplicity: Holm and BH over the questionnaire-wide gradient (Table A3,
# share 5-7) and over the nine sequential-concurrent contrasts (Table A8).
# p-values from the bootstrap SE (normal approximation).
adjust <- \(df) df |>
  mutate(p = 2 * pnorm(-abs(estimate / se)),
         p_holm = p.adjust(p, "holm"), p_bh = p.adjust(p, "BH"))

gradient_adjusted <- read_csv(here(path_tab, "tableA3_gradient.csv"), show_col_types = FALSE) |>
  filter(outcome == "share_ge5") |>
  adjust() |>
  select(set, item, estimate, se, p, p_holm, p_bh)

seq_conc_adjusted <- read_csv(here(path_tab, "tableA8_seq_vs_conc.csv"), show_col_types = FALSE) |>
  adjust() |>
  select(item, estimate, se, p, p_holm, p_bh)

# (b4) Cumulative logit on the stacked battery: item-specific thresholds, one
# common self-administration shift, and the excess for born and descent. Model 2
# adds a scale (dispersion) effect for self-administration, the extremity
# pattern of the civic items. Refits are slow (~4 s), so the model bootstrap
# uses the first n_boot_clm replicates, in parallel.
n_boot_clm <- 199

long <- d23 |>
  mutate(row = row_number()) |>
  select(row, sa = self_admin, all_of(mn_items)) |>
  pivot_longer(all_of(mn_items), names_to = "item", values_to = "y") |>
  filter(!is.na(y)) |>
  mutate(y = factor(y, levels = 1:7, ordered = TRUE), item = factor(item),
         sa = as.numeric(sa), sa_born = sa * (item == "mn01"),
         sa_descent = sa * (item == "mn02"))

clm_specs <- list(
  `Common shift + descent excess` = list(scale = NULL),
  `... + scale effect` = list(scale = ~ sa)
)

fit_clm <- function(spec, wj, start = NULL) {
  long$wt <- wj[long$row]
  fit <- ordinal::clm(y ~ sa + sa_born + sa_descent, nominal = ~ item, scale = spec$scale,
                      weights = wt, data = long[long$wt > 0, ], start = start)
  c(fit$beta, scale = fit$zeta)
}

clm_results <- imap(clm_specs, \(spec, model) {
  full <- fit_clm(spec, w[, 1])
  start <- ordinal::clm(y ~ sa + sa_born + sa_descent, nominal = ~ item, scale = spec$scale,
                        weights = w[long$row, 1], data = long) |> coef()
  reps <- parallel::mclapply(seq_len(n_boot_clm) + 1, \(j) fit_clm(spec, w[, j], start = start),
                             mc.cores = max(1, parallel::detectCores() - 1))
  terms <- intersect(c("sa", "sa_born", "sa_descent", "scale.sa"), names(full))
  map(terms, \(term) {
    boot_summary(c(full[[term]], map_dbl(reps, \(r) r[[term]]))) |>
      mutate(model = model, term = term, .before = 1)
  }) |> list_rbind()
}) |>
  list_rbind() |>
  mutate(term = recode(term, sa = "Common shift (logit)", sa_born = "Excess: born",
                       sa_descent = "Excess: descent", scale.sa = "Scale (log SD ratio)"))

save_tab(cutpoint_gaps, "tableA2d_cutpoints")
save_tab(did, "tableA3b_did")
save_tab(gradient_adjusted, "tableA3a_gradient_adjusted")
save_tab(seq_conc_adjusted, "tableA8b_seq_conc_adjusted")
save_tab(clm_results, "tableA2e_cumulative_logit")

print(did |> mutate(across(estimate:upper, \(x) round(x, 3))), n = Inf)
print(clm_results |> mutate(across(estimate:upper, \(x) round(x, 3))))
print(seq_conc_adjusted |> mutate(across(where(is.numeric), \(x) round(x, 3))))
print(gradient_adjusted |> filter(p_holm < 0.05) |> mutate(across(where(is.numeric), \(x) round(x, 3))), n = Inf)

# (c) Acquiescence --------------------------------------------------------------
# The mp battery mixes negative (mp02/04/06/07) and positive (mp05/10/12)
# statements on one agree-disagree scale (1 completely disagree - 7 completely
# agree). If the interviewer raises yea-saying, removing it lowers agreement on
# both poles: the balanced index falls and so does agreement with contradictory
# pairs. If it raises only what sounds acceptable, the positive pole falls and
# the negative pole does not: the content index (negative minus positive) rises.
# Respondents with all seven statements answered.
mp_negative <- c("mp02", "mp04", "mp06", "mp07")
mp_positive <- c("mp05", "mp10", "mp12")
contradictory_pairs <- list(c("mp06", "mp12"), c("mp02", "mp05"))

agree <- \(items) d23 |> select(all_of(items)) |> mutate(across(everything(), \(x) x >= 5))
complete_mp <- d23 |> select(all_of(c(mp_negative, mp_positive))) |> complete.cases()
neg <- rowMeans(agree(mp_negative))
pos <- rowMeans(agree(mp_positive))
both_poles <- contradictory_pairs |>
  map(\(p) agree(p) |> pull(1) & agree(p) |> pull(2)) |>
  reduce(`|`)

polarity_measures <- list(
  `Negative statements, share agreeing` = neg,
  `Positive statements, share agreeing` = pos,
  `Balanced agreement (mean of both poles)` = (neg + pos) / 2,
  `Content (negative minus positive)` = neg - pos,
  `Agrees with both sides of a contradictory pair` = as.numeric(both_poles)
) |>
  map(\(y) replace(y, !complete_mp, NA))

polarity <- polarity_measures |>
  imap(\(y, measure) {
    boot_summary(boot_mean(y, d23$self_admin, w) - boot_mean(y, capi, w)) |>
      mutate(measure = measure, capi = boot_mean(y, capi, w)[1],
             sa = boot_mean(y, d23$self_admin, w)[1], .before = 1)
  }) |>
  list_rbind() |>
  mutate(n_capi = sum(complete_mp & d23$arm == "CAPI"), n_sa = sum(complete_mp & d23$self_admin))

save_tab(polarity, "tableA3c_polarity")
print(polarity |> mutate(across(c(capi, sa, estimate:upper), \(x) round(x, 3))), width = 200)
