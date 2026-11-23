# Data preparation: the analysis file for the naturalisation battery (mn01-mn09).
# Waves with mn: 1996 (PAPI), 2006 and 2016 (CAPI), 2023 (randomised CAPI vs
# self-administered). The 2023 design arm (substudy) comes from ZA8833. The
# cumulation release with sample points (xs11a filled for 2023) is ZA8839_v1-0-0.dta.

source(here::here("code", "helpers.R"))

cum_vars <- c(
  "year", "respid", "mode", "german", "dn05", "sex", "age", "educ", "eastwest",
  "xi01", "xs11a", str_c("xs0", 1:7), "wghtpew", "splt16_4", "splt23_1", mn_items
)

cumulation <- read_dta(raw("ZA8839_v1-0-0.dta"), col_select = all_of(cum_vars)) |>
  filter(year %in% c(1996, 2006, 2016, 2023))

arms_2023 <- read_dta(raw("ZA8833_v1-0-0.dta"), col_select = c(respid, substudy)) |>
  mutate(year = 2023)

allbus <- cumulation |>
  left_join(arms_2023, by = c("year", "respid"), relationship = "one-to-one") |>
  mutate(
    across(all_of(mn_items), as_item),
    mode = as_factor(mode) |> fct_drop(),
    arm = case_when(
      year < 2023 ~ NA_character_,
      substudy == 1 ~ "Concurrent",
      substudy == 2 ~ "Sequential",
      mode == "CAPI" ~ "CAPI"
    ) |> factor(levels = arm_levels),
    # Self-administered arms pooled, for contrasts against CAPI
    self_admin = arm %in% c("Sequential", "Concurrent"),
    german_citizen = as_item(german) %in% c(1, 2),
    # German since birth (dn05 == 1): the universe of Diehl & Tucci (2011, fn 10),
    # used only to compare typology rules with their published shares
    german_birth = as_item(dn05) %in% 1,
    east = as_item(eastwest) == 2,
    region = if_else(east, "East", "West"),
    female = as_item(sex) == 2,
    age = as_item(age),
    age_group = cut(age, c(17, 29, 39, 49, 59, 69, Inf),
                    labels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70+")),
    # educ: 1 none, 2 lower, 3 intermediate, 4 applied-sciences entrance, 5 Abitur;
    # 6 other and 7 still at school are left out of the three-way grouping
    educ3 = case_when(
      as_item(educ) %in% 1:2 ~ "Low",
      as_item(educ) == 3 ~ "Intermediate",
      as_item(educ) %in% 4:5 ~ "High"
    ) |> factor(levels = c("Low", "Intermediate", "High")),
    interviewer = as_item(xi01),
    # Sample point (PSU); point numbers are reused across waves
    point = as_item(xs11a),
    psu = str_c(year, "_", point),
    # xs01: interview conducted alone with respondent (1 yes, 0 no)
    others_present = as_item(xs01) == 0,
    partner_present = as_item(xs02) == 1,
    weight = as.numeric(wghtpew),
    wave = factor(year)
  ) |>
  # Respondents who received the battery: 2016 split 1, 2023 CAPI + split 1
  filter(if_any(all_of(mn_items), \(x) !is.na(x)))

if (universe_german_only) allbus <- filter(allbus, german_citizen)

# Checks -----------------------------------------------------------------------

stopifnot(
  all(!is.na(allbus$arm[allbus$year == 2023])),
  all(is.na(allbus$mn09[allbus$year == 1996])),
  all(!is.na(allbus$point)),
  # PSUs nest in East/West, the sampling strata
  nrow(distinct(allbus, psu, region)) == n_distinct(allbus$psu),
  n_distinct(allbus$respid[allbus$year == 2023]) == sum(allbus$year == 2023)
)

# Asimov (2026) arm sizes for the mn battery; ours are within ~50 of hers
# (German citizens with a valid mn01; her further exclusion is not documented
# in the data)
n_arm <- allbus |> filter(year == 2023, !is.na(mn01)) |> count(arm)
walk2(n_arm$n, c(1819, 680, 756), \(obs, exp) stopifnot(abs(obs - exp) <= 50))

allbus |>
  select(year, wave, respid, mode, arm, self_admin, german_birth, region, east, female, age,
         age_group, educ3, interviewer, point, psu, others_present, partner_present,
         xs01:xs07, weight, all_of(mn_items)) |>
  write_rds(clean("allbus_mn.rds"))

message("allbus_mn.rds: ", nrow(allbus), " respondents")
print(count(allbus, year, mode, arm))
