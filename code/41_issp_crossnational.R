# Triangulation: ISSP 2013 -> 2023 change in the importance of being born in the
# country and of ancestry, by country, comparing countries that switched from
# interviewer- to self-administration with mode-stable countries. Descriptive;
# identification is weak (switching is not random and coincides with COVID-era
# fieldwork changes). Citizens only; national weights.

source(here::here("code", "helpers.R"))

interviewer_modes <- c("PAPI", "CAPI", "CATI")
self_modes <- c("SC, paper", "SC, interviewer present", "CASI", "CAWI")

issp <- read_rds(clean("issp_ni.rds")) |>
  filter(year %in% c(2013, 2023), citizen %in% TRUE) |>
  mutate(
    admin = case_when(
      mode %in% interviewer_modes ~ "Interviewer",
      mode %in% self_modes ~ "Self",
      .default = "Mixed/unknown"
    ),
    weight = replace_na(weight, 1)
  )

# The 2023 file carries no country labels; take them from the cumulation
labels_2013 <- issp |>
  filter(year == 2013) |>
  distinct(country, country_label)

# Dominant administration per country and year (share of cases)
country_mode <- issp |>
  count(country, year, admin) |>
  mutate(share = n / sum(n), .by = c(country, year)) |>
  slice_max(share, n = 1, by = c(country, year), with_ties = FALSE) |>
  select(country, year, admin, share) |>
  pivot_wider(names_from = year, values_from = c(admin, share))

change <- issp |>
  summarise(
    across(c(born, ancestry), \(x) weighted.mean(x >= 3, weight, na.rm = TRUE)),
    n = n(),
    .by = c(country, year)
  ) |>
  pivot_wider(names_from = year, values_from = c(born, ancestry, n)) |>
  filter(!is.na(born_2013), !is.na(born_2023)) |>
  left_join(labels_2013, by = "country") |>
  left_join(country_mode, by = "country") |>
  mutate(
    switch = case_when(
      admin_2013 == "Interviewer" & admin_2023 == "Self" ~ "Interviewer -> self",
      admin_2013 == admin_2023 ~ "Mode-stable",
      .default = "Other change"
    ),
    d_born = born_2023 - born_2013,
    d_ancestry = ancestry_2023 - ancestry_2013
  )

save_tab(change, "tableA13_issp_crossnational")

change |>
  summarise(n_countries = n(), across(c(d_born, d_ancestry), \(x) round(100 * mean(x, na.rm = TRUE), 1)),
            .by = switch) |>
  print()

fig_cn <- change |>
  select(country_label, switch, d_born, d_ancestry) |>
  pivot_longer(c(d_born, d_ancestry), names_to = "item", values_to = "change") |>
  mutate(item = fct_recode(item, Born = "d_born", Ancestry = "d_ancestry"),
         country_label = str_remove(country_label, "^\\d+\\.\\s*")) |>
  ggplot(aes(100 * change, fct_reorder(country_label, change), colour = switch)) +
  geom_vline(xintercept = 0, colour = "grey60") +
  geom_point() +
  facet_wrap(vars(item)) +
  scale_colour_manual(values = c(`Interviewer -> self` = arm_colours[["Concurrent"]],
                                 `Mode-stable` = arm_colours[["CAPI"]],
                                 `Other change` = "grey70"), name = NULL) +
  labs(x = "Change 2013-2023 in share rating the criterion very/fairly important (pp)", y = NULL) +
  theme_paper()

save_fig(fig_cn, "figA3_issp_crossnational", height = 150)
