# Figure 3: Diehl & Tucci four-type typology by 2023 design arm, next
# to the historical waves. First reproduces the published 1996/2006 shares.

source(here::here("code", "helpers.R"))

allbus <- read_rds(clean("allbus_mn.rds")) |>
  mutate(type = dt_typology(pick(everything()))) |>
  filter(!is.na(type))

# Reproduction check: Diehl & Tucci (2011) "mainly ethnic", 1996 -> 2006
published <- tribble(
  ~year, ~region, ~expected,
  1996, "West", 12,
  2006, "West", 4,
  1996, "East", 17,
  2006, "East", 3
)

allbus |>
  filter(year %in% c(1996, 2006)) |>
  summarise(share = 100 * weighted.mean(type == "Mainly ethnic", weight), .by = c(year, region)) |>
  inner_join(published, by = c("year", "region")) |>
  pwalk(\(year, region, share, expected) {
    check_close(share, expected, tol = 2.5, what = glue::glue("mainly ethnic {region} {year} (%)"))
  })

# Shares by wave (pre-2023) and by arm (2023), with bootstrap CIs
allbus <- allbus |>
  mutate(group = if_else(year == 2023, str_c("2023 ", arm), str_c(year, " ", mode)))

w <- boot_weights(allbus, c("wave", "region"))

shares <- expand_grid(group = unique(allbus$group), type = typology_levels) |>
  mutate(res = map2(group, type, \(g, t) {
    boot_summary(boot_mean(as.numeric(allbus$type == t), allbus$group == g, w))
  })) |>
  unnest(res) |>
  mutate(
    type = factor(type, levels = typology_levels),
    group = factor(group, levels = c("1996 PAPI", "2006 CAPI", "2016 CAPI",
                                     str_c("2023 ", arm_levels)))
  )

# Difference self-administered arm - CAPI arm, 2023
arm_diffs <- expand_grid(arm = c("Sequential", "Concurrent"), type = typology_levels) |>
  mutate(res = map2(arm, type, \(a, t) {
    y <- as.numeric(allbus$type == t)
    boot_summary(boot_mean(y, allbus$group == str_c("2023 ", a), w) -
                   boot_mean(y, allbus$group == "2023 CAPI", w))
  })) |>
  unnest(res)

save_tab(shares, "typology_shares")
save_tab(arm_diffs, "typology_arm_differences")

fig3 <- shares |>
  ggplot(aes(group, 100 * estimate, colour = str_detect(group, "Sequential|Concurrent"))) +
  geom_pointrange(aes(ymin = 100 * lower, ymax = 100 * upper), size = 0.25) +
  facet_wrap(vars(type), nrow = 1) +
  scale_colour_manual(values = c(`FALSE` = "grey20", `TRUE` = arm_colours[["Sequential"]]),
                      guide = "none") +
  labs(x = NULL, y = "Share of respondents (%)") +
  theme_paper() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_fig(fig3, "fig3_typology", height = 95)

print(arm_diffs |> mutate(across(where(is.numeric), \(x) round(100 * x, 1))))

# Alternative codings (Table A4e) ---------------------------------------------------
# The original coding is not identified by the published texts (see helpers.R).
# For each rule in `typology_rules`: fit to the 16 shares printed in Diehl &
# Tucci (2010, Fig. 1), on their universe (German since birth), and the 2023
# shift, pooled self-administered minus CAPI.

published_all <- tribble(
  ~type,           ~West_1996, ~East_1996, ~West_2006, ~East_2006,
  "Mainly ethnic",         12,         17,          4,          3,
  "Mainly civic",          21,         15,         57,         54,
  "Both",                  26,         22,         14,         15,
  "Neither",               41,         46,         25,         28
) |>
  pivot_longer(-type, names_to = c("region", "year"), names_sep = "_",
               values_to = "published", names_transform = list(year = as.numeric))

all_cases <- read_rds(clean("allbus_mn.rds"))
typed <- typology_rules |>
  map(\(rule) all_cases |> mutate(type = rule(pick(everything()))))

rule_fit <- typed |>
  imap(\(d, rule) {
    d |>
      filter(year %in% c(1996, 2006), german_birth, !is.na(type)) |>
      count(year, region, type, wt = weight) |>
      mutate(share = 100 * n / sum(n), .by = c(year, region)) |>
      inner_join(published_all, by = c("year", "region", "type")) |>
      summarise(rule = rule, fit_mean = mean(abs(share - published)),
                fit_max = max(abs(share - published)))
  }) |>
  list_rbind()

d23 <- all_cases |> filter(year == 2023)
w23 <- boot_weights(d23, "region")
universes <- list(`German citizens` = rep(TRUE, nrow(d23)), `German since birth` = d23$german_birth)

rule_shifts <- expand_grid(rule = names(typology_rules), universe = names(universes),
                           type = typology_levels) |>
  mutate(res = pmap(list(rule, universe, type), \(rule, universe, type) {
    y <- as.numeric(typed[[rule]]$type[typed[[rule]]$year == 2023] == type)
    keep <- universes[[universe]]
    capi <- boot_mean(y, keep & d23$arm == "CAPI", w23)
    sa <- boot_mean(y, keep & d23$self_admin, w23)
    boot_summary(sa - capi) |> mutate(capi = capi[1], sa = sa[1], .before = 1)
  })) |>
  unnest(res)

typology_rule_table <- rule_shifts |> left_join(rule_fit, by = "rule")
save_tab(typology_rule_table, "tableA4e_typology_rules")

typology_rule_table |>
  mutate(across(capi:upper, \(x) round(100 * x, 1)), across(starts_with("fit"), \(x) round(x, 1))) |>
  print(n = Inf, width = 200)
