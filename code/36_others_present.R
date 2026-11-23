# Robustness (Table A11): third-party presence (xs01 "interview conducted alone";
# xs02 partner present). Share rating mn01/mn02 5-7 by presence, within the CAPI
# arm and within the self-administered arms. Presence is self-selected, so read
# the comparison as interviewer-specific vs general social presence, not as a
# falsification test.

source(here::here("code", "helpers.R"))

d23 <- read_rds(clean("allbus_mn.rds")) |>
  filter(year == 2023, !is.na(others_present)) |>
  mutate(setting = if_else(self_admin, "Self-administered", "CAPI"))

w <- boot_weights(d23, "region")

presence <- expand_grid(
  setting = c("CAPI", "Self-administered"),
  item = c("mn01", "mn02"),
  indicator = c("others_present", "partner_present")
) |>
  mutate(res = pmap(list(setting, item, indicator), \(s, i, ind) {
    y <- as.numeric(d23[[i]] >= 5)
    in_setting <- d23$setting == s
    present <- in_setting & d23[[ind]] %in% TRUE
    alone <- in_setting & !d23$others_present
    boot_summary(boot_mean(y, present, w) - boot_mean(y, alone, w)) |>
      mutate(n_present = sum(present & !is.na(y)), n_alone = sum(alone & !is.na(y)))
  })) |>
  unnest(res)

save_tab(presence, "tableA11_third_party")

presence |>
  mutate(across(c(estimate, lower, upper), \(x) round(100 * x, 1))) |>
  select(setting, item, indicator, n_present, n_alone, estimate, lower, upper) |>
  print()
