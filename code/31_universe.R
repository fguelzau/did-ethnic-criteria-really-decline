# Robustness (Table A1a-c): the Figure 1 / Table A2 contrasts under two analysis
# universes for 2023.
#   - Main: German citizens (as in the rest of the pipeline)
#   - Asimov (2026): all citizenships, age <= 80 (the sequential sample stopped at
#     80), sex and education codable for her raking (education 1-5)
# Her Ns (Table A1: mn01 1,819 / 679 / 756) are approached within ~1-3%; the rest
# comes from covariate completeness rules in her selection models that cannot be
# recovered from the data. She rakes to Microcensus 2024; here both universes use
# wghtpew.

source(here::here("code", "helpers.R"))

za8833 <- read_dta(
  raw("ZA8833_v1-0-0.dta"),
  col_select = c(respid, substudy, mode, age, sex, educ, german, eastwest, wghtpew, all_of(mn_items))
) |>
  mutate(
    arm = case_when(
      substudy == 1 ~ "Concurrent",
      substudy == 2 ~ "Sequential",
      as_item(mode) == 2 ~ "CAPI"
    ) |> factor(levels = arm_levels),
    self_admin = arm != "CAPI",
    region = if_else(as_item(eastwest) == 2, "East", "West"),
    weight = as.numeric(wghtpew),
    across(all_of(mn_items), as_item),
    age = as_item(age)
  ) |>
  filter(!is.na(arm), !is.na(mn01)) |>
  left_join(
    read_dta(raw("ZA8839_v1-0-0.dta"), col_select = c(year, respid, xs11a)) |>
      filter(year == 2023) |>
      transmute(respid, psu = str_c(year, "_", as_item(xs11a))),
    by = "respid", relationship = "one-to-one"
  )

stopifnot(all(!is.na(za8833$psu)))

universes <- list(
  `German citizens` = za8833 |> filter(as_item(german) %in% 1:2),
  `Asimov (2026)` = za8833 |> filter(age <= 80, as_item(sex) %in% 1:2, as_item(educ) %in% 1:5)
)

contrast <- function(d23) {
  w <- boot_weights(d23, "region")
  capi <- d23$arm == "CAPI"
  expand_grid(item = mn_items, outcome = c("share_ge5", str_c("cat", 1:7))) |>
    mutate(res = map2(item, outcome, \(i, o) {
      x <- d23[[i]]
      y <- if (o == "share_ge5") as.numeric(x >= 5) else as.numeric(x == parse_number(o))
      map(c("Sequential", "Concurrent", "Self-administered"), \(a) {
        group <- if (a == "Self-administered") d23$self_admin else d23$arm == a
        boot_summary(boot_mean(y, group, w) - boot_mean(y, capi, w)) |> mutate(contrast = a)
      }) |> list_rbind()
    })) |>
    unnest(res)
}

results <- universes |>
  map(contrast) |>
  list_rbind(names_to = "universe")

n_by_arm <- universes |>
  map(\(d) count(d, arm)) |>
  list_rbind(names_to = "universe") |>
  pivot_wider(names_from = arm, values_from = n)

aamb <- results |>
  filter(str_starts(outcome, "cat"), contrast != "Self-administered") |>
  summarise(aamb = 100 * mean(abs(estimate)), .by = c(universe, item, contrast))

save_tab(results, "tableA1b_universe_contrasts")
save_tab(aamb, "tableA1c_universe_aamb")
save_tab(n_by_arm, "tableA1a_universe_n")

print(n_by_arm)
results |>
  filter(outcome == "share_ge5", contrast == "Self-administered") |>
  select(universe, item, estimate) |>
  mutate(estimate = round(100 * estimate, 1)) |>
  pivot_wider(names_from = universe, values_from = estimate) |>
  print()
