# Figure 1 and Table A2: signed category-wise design effects, mn01-mn09, 2023.
# Each self-administered arm (and both pooled) minus the CAPI arm, by randomised
# design (ITT), never by realised mode. Weighted with wghtpew; bootstrap CIs
# resample sample points within East/West.

source(here::here("code", "helpers.R"))

d23 <- read_rds(clean("allbus_mn.rds")) |> filter(year == 2023)

w <- boot_weights(d23, "region")

groups <- list(
  CAPI = d23$arm == "CAPI",
  Sequential = d23$arm == "Sequential",
  Concurrent = d23$arm == "Concurrent",
  `Self-administered` = d23$self_admin
)

contrasts <- c("Sequential", "Concurrent", "Self-administered")

summarise_contrasts <- function(y) {
  m <- map(groups, \(g) boot_mean(y, g, w))
  map(contrasts, \(arm) {
    boot_summary(m[[arm]] - m$CAPI) |>
      mutate(contrast = arm, capi = m$CAPI[1], arm_value = m[[arm]][1], .before = 1)
  }) |> list_rbind()
}

# Outcomes per item: share in each category 1-7, share >= 5, mean
outcomes <- expand_grid(item = mn_items, outcome = c(str_c("cat", 1:7), "share_ge5", "mean")) |>
  mutate(y = map2(item, outcome, \(item, outcome) {
    x <- d23[[item]]
    switch(outcome,
      mean = x,
      share_ge5 = as.numeric(x >= 5),
      as.numeric(x == parse_number(outcome))
    )
  }))

effects <- outcomes |>
  mutate(res = map(y, summarise_contrasts)) |>
  select(-y) |>
  unnest(res) |>
  mutate(
    contrast = factor(contrast, levels = contrasts),
    item_label = factor(mn_labels[item], levels = mn_labels),
    block = if_else(item %in% c("mn01", "mn02"), "Descent", "Other")
  )

save_tab(effects, "tableA2_design_effects")

# Average absolute measurement bias (Asimov 2026): mean absolute difference in
# category shares, in percentage points
aamb <- effects |>
  filter(str_starts(outcome, "cat")) |>
  summarise(aamb = 100 * mean(abs(estimate)), .by = c(item, contrast))

save_tab(aamb, "aamb")

# Reproduction check against Asimov (2026, Table A1). Her sample (age <= 80,
# all citizenships, raked to Microcensus 2024) differs from ours, hence the
# tolerance of 1 pp.
tribble(
  ~item, ~contrast, ~expected,
  "mn01", "Sequential", 6.30,
  "mn01", "Concurrent", 7.44,
  "mn02", "Sequential", 4.56,
  "mn02", "Concurrent", 6.52
) |>
  left_join(aamb, by = c("item", "contrast")) |>
  pwalk(\(item, contrast, expected, aamb) {
    check_close(aamb, expected, tol = 1, what = glue::glue("AAMB {item} {contrast}"))
  })

# Figure 1 -----------------------------------------------------------------------

fig1 <- effects |>
  filter(str_starts(outcome, "cat"), contrast != "Self-administered") |>
  mutate(category = parse_number(outcome)) |>
  ggplot(aes(category, 100 * estimate, colour = contrast)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_pointrange(
    aes(ymin = 100 * lower, ymax = 100 * upper),
    position = position_dodge(width = 0.5), size = 0.2
  ) +
  facet_wrap(vars(item_label), ncol = 3) +
  scale_x_continuous(breaks = 1:7) +
  scale_colour_manual(values = arm_colours, name = "Design arm minus CAPI") +
  labs(
    x = "Response category (1 = not at all important, 7 = very important)",
    y = "Difference in share (percentage points)"
  ) +
  theme_paper()

save_fig(fig1, "fig1_design_effects", height = 150)

# Summary panel: share rating 5-7, all items
fig1b <- effects |>
  filter(outcome == "share_ge5") |>
  ggplot(aes(100 * estimate, fct_rev(item_label), colour = contrast)) +
  geom_vline(xintercept = 0, colour = "grey60") +
  geom_pointrange(
    aes(xmin = 100 * lower, xmax = 100 * upper),
    position = position_dodge(width = 0.6), size = 0.2
  ) +
  scale_colour_manual(values = c(arm_colours, `Self-administered` = "black"), name = NULL) +
  labs(x = "Difference in share rating 5-7 vs CAPI (percentage points)", y = NULL) +
  theme_paper()

save_fig(fig1b, "fig1b_share_important", height = 110)

effects |>
  filter(outcome %in% c("share_ge5", "mean"), contrast == "Self-administered") |>
  select(item, outcome, capi, arm_value, estimate, lower, upper) |>
  mutate(across(where(is.numeric), \(x) round(x, 3))) |>
  print(n = 20)
