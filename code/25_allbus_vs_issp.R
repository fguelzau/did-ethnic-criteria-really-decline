# ALLBUS and ISSP descent series compared (Table A7, Figure A2). Interviewer-
# administered ALLBUS descent items (mn01 born, mn02 descent) against the
# self-completion ISSP items (born, ancestry), German citizens. H5: the ALLBUS
# items declined faster.
# Each item is standardised within item (pooled mean and SD across its waves).
# Caveats: the ISSP series is not mode-constant either (1995 mail, 2003 interviewer
# present at completion, 2013 mostly CASI within an interview, 2023 mail); ancestry was
# not asked in Germany in 1995; question wording and scales differ, which the
# within-item standardisation handles only for level and spread.
# SEs: ALLBUS clustered on sample points (psu) within wave x region; the ISSP files
# carry no sample points, so ISSP respondents are their own clusters and its SEs
# stay too small to the extent that the German ISSP samples were clustered.

source(here::here("code", "helpers.R"))
suppressPackageStartupMessages(library(survey))

wtd_var <- function(x, w) sum(w * (x - weighted.mean(x, w))^2) / (sum(w) - 1)

allbus <- read_rds(clean("allbus_mn.rds")) |>
  filter(!self_admin) |>
  transmute(source = "ALLBUS (interviewer)", year, weight, psu = as.character(psu),
            region = as.character(region), born = mn01, descent = mn02)

issp <- read_rds(clean("issp_ni.rds")) |>
  filter(germany, citizen) |>
  transmute(source = "ISSP (self-completion)", year, weight, psu = str_c("issp_", year, "_", id),
            region = replace_na(as.character(region), "all"), born, descent = ancestry)

wedge_data <- bind_rows(allbus, issp) |>
  pivot_longer(c(born, descent), names_to = "criterion", values_to = "value") |>
  filter(!is.na(value)) |>
  mutate(
    z = (value - weighted.mean(value, weight)) / sqrt(wtd_var(value, weight)),
    .by = c(source, criterion)
  ) |>
  mutate(decade = (year - 1995) / 10, source = fct_inorder(source),
         stratum = str_c(source, year, region, sep = "_"))

# Standardised means by wave
wave_means <- wedge_data |>
  summarise(z = weighted.mean(z, weight), n = n(), .by = c(source, criterion, year))

save_tab(wave_means, "tableA7b_wedge_means")

# Slope difference: z ~ decade * source, per criterion, cluster-robust SEs
slopes <- wedge_data |>
  nest(.by = criterion) |>
  mutate(fit = map(data, \(d) {
    svyglm(z ~ decade * source, design = svydesign(ids = ~psu, strata = ~stratum, weights = ~weight,
                                         nest = TRUE, data = d))
  })) |>
  mutate(tidy = map(fit, broom::tidy, conf.int = TRUE)) |>
  select(criterion, tidy) |>
  unnest(tidy)

save_tab(slopes, "tableA7a_wedge_slopes")

# ISSP born by wave: 1995 and 2023 were both postal, so these two waves compare
# without a mode change. Respondents are their own clusters, as above.
born_shares <- read_rds(clean("issp_ni.rds")) |>
  filter(germany, citizen, !is.na(born)) |>
  mutate(important = as.numeric(born >= 3), very = as.numeric(born == 4)) |>
  srvyr::as_survey_design(weights = weight) |>
  group_by(year, mode) |>
  summarise(
    n = srvyr::unweighted(n()),
    important = srvyr::survey_mean(important, vartype = "ci"),
    very = srvyr::survey_mean(very, vartype = "ci")
  ) |>
  ungroup()

save_tab(born_shares, "tableA7c_issp_born_shares")

fig_wedge <- wave_means |>
  ggplot(aes(year, z, colour = source)) +
  geom_line() +
  geom_point() +
  facet_wrap(vars(criterion), labeller = as_labeller(c(born = "Born in Germany",
                                                       descent = "German descent / ancestry"))) +
  scale_x_continuous(breaks = c(1995, 2003, 2006, 2013, 2016, 2023)) +
  scale_colour_manual(values = c(arm_colours[["CAPI"]], arm_colours[["Concurrent"]]), name = NULL) +
  labs(x = NULL, y = "Mean importance (standardised within item)",
       caption = str_wrap(str_c("German citizens. ALLBUS 1996 PAPI, 2006-2023 CAPI arm; ISSP 1995 mail, ",
                                "2003 SC with interviewer present, 2013 mostly CASI, 2023 mail."), 120)) +
  theme_paper()

save_fig(fig_wedge, "figA2_wedge", height = 90)

slopes |>
  filter(str_detect(term, "decade")) |>
  select(criterion, term, estimate, conf.low, conf.high, p.value) |>
  mutate(across(where(is.numeric), \(x) round(x, 3))) |>
  print()
