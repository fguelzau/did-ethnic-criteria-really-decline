# Robustness (Table A10): interviewer ICCs in the 2023 CAPI arm (and 2016 for
# comparison), mn items vs the life-domain placebo items. Random-intercept models
# with lme4.
# The interviewer-only ICC confounds interviewer and area variance (an upper
# bound). Interviewers partly cross sample points (xs11a), so a cross-classified
# interviewer x point model separates the two.

source(here::here("code", "helpers.R"))
suppressPackageStartupMessages(library(lme4))

placebo_items <- c("li01", "li02", "li03", "li04", "li05", "li07", "li08")

capi_2023 <- read_rds(clean("allbus_mn.rds")) |>
  filter(year == 2023, arm == "CAPI") |>
  left_join(
    read_dta(raw("ZA8833_v1-0-0.dta"), col_select = c(respid, all_of(placebo_items))) |>
      mutate(across(all_of(placebo_items), as_item)),
    by = "respid"
  )

capi_2016 <- read_rds(clean("allbus_mn.rds")) |> filter(year == 2016)

# 95% intervals from a parametric bootstrap of the variance shares
n_sim_icc <- 200

icc_share <- function(fit, group) {
  vc <- as.data.frame(VarCorr(fit))
  vc$vcov[vc$grp == group] / sum(vc$vcov)
}

icc_ci <- function(fit, group) {
  sims <- bootMer(fit, \(f) icc_share(f, group), nsim = n_sim_icc, seed = 20230101,
                  parallel = "multicore", ncpus = max(1, parallel::detectCores() - 1))$t
  quantile(sims, c(0.025, 0.975), names = FALSE, na.rm = TRUE)
}

icc <- function(df, item) {
  df <- filter(df, !is.na(interviewer))
  fit <- lmer(reformulate("1 + (1 | interviewer)", response = item), data = df, REML = TRUE)
  fit_x <- lmer(reformulate("1 + (1 | interviewer) + (1 | point)", response = item),
                data = df, REML = TRUE)
  vc <- as.data.frame(VarCorr(fit))
  vc_x <- as.data.frame(VarCorr(fit_x))
  ci <- icc_ci(fit, "interviewer")
  ci_x <- icc_ci(fit_x, "interviewer")
  tibble(
    item = item,
    n = nobs(fit),
    n_interviewers = ngrps(fit)[["interviewer"]],
    n_points = ngrps(fit_x)[["point"]],
    icc = vc$vcov[vc$grp == "interviewer"] / sum(vc$vcov),
    icc_interviewer_x = vc_x$vcov[vc_x$grp == "interviewer"] / sum(vc_x$vcov),
    icc_point_x = vc_x$vcov[vc_x$grp == "point"] / sum(vc_x$vcov),
    icc_lower = ci[1], icc_upper = ci[2],
    icc_interviewer_x_lower = ci_x[1], icc_interviewer_x_upper = ci_x[2]
  )
}

iccs <- bind_rows(
  map(c(mn_items, placebo_items), \(i) icc(capi_2023, i)) |> list_rbind() |> mutate(year = 2023),
  map(mn_items, \(i) icc(capi_2016, i)) |> list_rbind() |> mutate(year = 2016)
) |>
  mutate(set = case_when(
    item %in% c("mn01", "mn02") ~ "Descent",
    item %in% mn_items ~ "Other naturalisation",
    .default = "Placebo: life domains"
  ))

save_tab(iccs, "tableA10_interviewer_icc")

iccs |>
  summarise(across(starts_with("icc"), mean), max_icc = max(icc), .by = c(year, set)) |>
  mutate(across(where(is.double), \(x) round(x, 3))) |>
  print()

# Interviewer characteristics and presence of others ---------------------------
# Logit for rating born / descent important (5-7) in the 2023 CAPI arm, with
# random intercepts for interviewers and points. Exploratory: interviewer
# characteristics are not randomised to respondents, and presence of others is
# self-selected, so coefficients are associations given respondent covariates.
interviewer_vars <- read_dta(raw("ZA8839_v1-0-0.dta"),
                             col_select = c(year, respid, xi02, xi03, xi04, xi05)) |>
  filter(year == 2023) |>
  transmute(respid,
            int_female = as_item(xi02) == 2,
            int_age10 = as_item(xi03) / 10,
            int_tertiary = as_item(xi04) >= 4,
            int_tenure5 = as_item(xi05) / 5)

capi_model <- capi_2023 |>
  left_join(interviewer_vars, by = "respid") |>
  mutate(age10 = age / 10) |>
  filter(!is.na(interviewer))

interviewer_models <- map(c("mn01", "mn02"), \(item) {
  df <- capi_model |> mutate(y = as.numeric(.data[[item]] >= 5))
  fit <- glmer(y ~ int_female + int_age10 + int_tertiary + int_tenure5 + others_present +
                 age10 + female + educ3 + region + (1 | interviewer) + (1 | point),
               data = df, family = binomial, control = glmerControl(optimizer = "bobyqa"))
  est <- fixef(fit)
  se <- sqrt(diag(as.matrix(vcov(fit))))
  vc <- as.data.frame(VarCorr(fit))
  tibble(item = item, term = names(est), log_odds = est, se = se,
         lower = est - 1.96 * se, upper = est + 1.96 * se, n = nobs(fit),
         var_interviewer = vc$vcov[vc$grp == "interviewer"], var_point = vc$vcov[vc$grp == "point"])
}) |>
  list_rbind()

save_tab(interviewer_models, "tableA10b_interviewer_models")

interviewer_models |>
  mutate(across(log_odds:upper, \(x) round(x, 2))) |>
  print(n = Inf)
