# Figures 4, 5 and A1, Tables A5 and A6: the series 1996-2023 with a mode bridge.
#   2016 CAPI -> 2023 CAPI: attitude change at constant mode
#   2023 CAPI -> 2023 self-administered (both arms pooled): mode at constant time
# Only the 2023 mode gap is observed. The series corrected to self-administration
# is therefore shown as a band between two scenarios for earlier waves:
#   - constant gap: the 2023 logit gap applies to every wave (the decline is all
#     real; in percentage points the gap is larger at the 1996 level)
#   - growing gap: no gap in 1996, rising linearly to the 2023 gap (the widest
#     reportability share compatible with a mode effect that never shrank, the
#     assumption that follows from descent items becoming more normatively marked)
# Gaps are applied on the logit scale for shares and additively for means, per
# bootstrap replicate. The reportability share (the part of the measured
# 1996-2023 decline that the gap accounts for) depends only on the endpoints;
# the linear path affects the intermediate waves only and makes the growing-gap
# series rise from 2016 to 2023, which is an artefact of the path, not a finding.
# Because the share depends only on the endpoints, the two scenarios are the ends
# of a single assumption: how large the 1996 gap was. The sensitivity section
# below sweeps that one number and reports the share as a function of it, so the
# reader supplies the assumption instead of inheriting ours.

source(here::here("code", "helpers.R"))

allbus <- read_rds(clean("allbus_mn.rds")) |>
  mutate(
    series = case_when(
      year < 2023 ~ "Interviewer-administered",
      self_admin ~ "Self-administered",
      .default = "Interviewer-administered"
    ),
    group = str_c(year, "_", series)
  )

groups <- distinct(allbus, year, series, group)
w <- boot_weights(allbus, c("wave", "region"))

outcome_y <- function(item, outcome) {
  x <- allbus[[item]]
  if (outcome == "mean") x else as.numeric(x >= 5)
}

# Replicate vectors of every group estimate, kept for the contrasts below
estimates <- expand_grid(item = mn_items, outcome = c("share_ge5", "mean"), groups) |>
  mutate(reps = pmap(list(item, outcome, group), \(item, outcome, group) {
    boot_mean(outcome_y(item, outcome), allbus$group == group, w)
  })) |>
  filter(map_lgl(reps, \(r) !is.na(r[1])))

rep_of <- function(item_, outcome_, year_, series_) {
  estimates |>
    filter(item == item_, outcome == outcome_, year == year_, series == series_) |>
    pull(reps) |>
    pluck(1)
}

# Correction factor and decomposition per item
bridge <- estimates |>
  distinct(item, outcome) |>
  mutate(res = map2(item, outcome, \(i, o) {
    capi_23 <- rep_of(i, o, 2023, "Interviewer-administered")
    self_23 <- rep_of(i, o, 2023, "Self-administered")
    capi_16 <- rep_of(i, o, 2016, "Interviewer-administered")
    list(
      mode_gap_2023 = self_23 - capi_23,
      change_2016_2023_capi = capi_23 - capi_16,
      change_2016_2023_naive = self_23 - capi_16
    ) |>
      map(boot_summary) |>
      list_rbind(names_to = "quantity")
  })) |>
  unnest(res)

save_tab(bridge, "tableA5_correction_factors")

# Scenario bands -------------------------------------------------------------------

link <- list(share_ge5 = qlogis, mean = identity)
inverse <- list(share_ge5 = plogis, mean = identity)

first_year <- min(allbus$year)
last_year <- max(allbus$year)

scenario_weight <- list(
  `Constant gap` = \(year) rep(1, length(year)),
  `Growing gap` = \(year) (year - first_year) / (last_year - first_year)
)

# Replicate vectors of the corrected series, per scenario and wave
corrected <- estimates |>
  filter(series == "Interviewer-administered") |>
  expand_grid(scenario = names(scenario_weight)) |>
  mutate(reps = pmap(list(item, outcome, year, scenario, reps), \(i, o, y, s, r) {
    gap <- link[[o]](rep_of(i, o, last_year, "Self-administered")) -
      link[[o]](rep_of(i, o, last_year, "Interviewer-administered"))
    inverse[[o]](link[[o]](r) + scenario_weight[[s]](y) * gap)
  }))

# Reportability share: 1 - corrected decline / measured decline, 1996 -> 2023
reportability <- corrected |>
  filter(year %in% c(first_year, last_year)) |>
  select(item, outcome, scenario, year, reps) |>
  pivot_wider(names_from = year, values_from = reps, names_prefix = "y") |>
  mutate(res = pmap(list(item, outcome, .data[[str_c("y", first_year)]],
                         .data[[str_c("y", last_year)]]), \(i, o, first, last) {
    measured <- rep_of(i, o, first_year, "Interviewer-administered") -
      rep_of(i, o, last_year, "Interviewer-administered")
    corrected_decline <- first - last
    bind_rows(
      boot_summary(measured) |> mutate(quantity = "Measured decline"),
      boot_summary(corrected_decline) |> mutate(quantity = "Corrected decline"),
      boot_summary(1 - corrected_decline / measured) |> mutate(quantity = "Reportability share")
    )
  })) |>
  select(item, outcome, scenario, res) |>
  unnest(res)

save_tab(reportability, "tableA6a_reportability_scenarios")

# Gap-path sensitivity ---------------------------------------------------------------
# gap_ratio = the 1996 mode gap as a fraction of the 2023 gap, on the link scale.
# 1 reproduces the constant-gap scenario, 0 the growing-gap scenario. Reported
# against the implied 1996 gap in points (Figure 5, Table A6b).

curve_items <- c("mn01", "mn02")
gap_ratios <- seq(0, 1, by = 0.02)

sensitivity <- estimates |>
  distinct(item, outcome) |>
  filter(item %in% curve_items) |>
  expand_grid(gap_ratio = gap_ratios) |>
  mutate(res = pmap(list(item, outcome, gap_ratio), \(i, o, ratio) {
    capi_first <- rep_of(i, o, first_year, "Interviewer-administered")
    capi_last <- rep_of(i, o, last_year, "Interviewer-administered")
    gap <- link[[o]](rep_of(i, o, last_year, "Self-administered")) -
      link[[o]](capi_last)
    corrected_first <- inverse[[o]](link[[o]](capi_first) + ratio * gap)
    corrected_last <- inverse[[o]](link[[o]](capi_last) + gap)
    bind_rows(
      boot_summary(corrected_first - capi_first) |>
        mutate(quantity = "Gap in 1996"),
      boot_summary(1 - (corrected_first - corrected_last) / (capi_first - capi_last)) |>
        mutate(quantity = "Reportability share")
    )
  })) |>
  unnest(res) |>
  mutate(item_label = factor(mn_labels[item], levels = mn_labels))

save_tab(sensitivity, "reportability_sensitivity")

# The curve has to pass through both scenarios at its endpoints
expand_grid(item = curve_items, scen = c("Growing gap", "Constant gap")) |>
  pwalk(\(item, scen) {
    pick <- \(df, ...) df |>
      filter(item == !!item, outcome == "share_ge5",
             quantity == "Reportability share", ...) |>
      pull(estimate)
    observed <- pick(sensitivity, gap_ratio == if (scen == "Constant gap") 1 else 0)
    expected <- pick(reportability, scenario == scen)
    check_close(observed, round(expected, 4), 0.001,
                glue::glue("sensitivity curve endpoint, {scen} ({item})"))
  })

# Where the curve crosses a given share: "the reportability share exceeds one
# third only if the 1996 gap was at least X points"
thresholds <- sensitivity |>
  filter(outcome == "share_ge5") |>
  select(item, item_label, gap_ratio, quantity, estimate) |>
  pivot_wider(names_from = quantity, values_from = estimate) |>
  rename(gap_1996 = `Gap in 1996`, share = `Reportability share`) |>
  arrange(item, share) |>
  reframe(
    target = c(0.25, 1 / 3, 0.5),
    gap_1996 = approx(share, gap_1996, xout = target)$y,
    gap_ratio = approx(share, gap_ratio, xout = target)$y,
    .by = c(item, item_label)
  )

save_tab(thresholds, "tableA6b_reportability_thresholds")

# Partial reportability and scale ----------------------------------------------
# kappa = the part of the 2023 gap that reflects reporting (the rest being
# selection or layout); kappa = 1 is the main analysis. Both scenario ends, on
# the logit scale and on the percentage-point scale (where the constant-gap end
# is zero by construction).
kappas <- c(0.25, 0.5, 0.75, 1)
scales <- list(Logit = list(link = qlogis, inverse = plogis),
               `Percentage points` = list(link = identity, inverse = identity))

partial <- expand_grid(item = curve_items, scale = names(scales), kappa = kappas,
                       scenario = names(scenario_weight)) |>
  mutate(res = pmap(list(item, scale, kappa, scenario), \(i, s, k, scen) {
    f <- scales[[s]]
    capi_first <- rep_of(i, "share_ge5", first_year, "Interviewer-administered")
    capi_last <- rep_of(i, "share_ge5", last_year, "Interviewer-administered")
    gap <- k * (f$link(rep_of(i, "share_ge5", last_year, "Self-administered")) - f$link(capi_last))
    first <- f$inverse(f$link(capi_first) + scenario_weight[[scen]](first_year) * gap)
    last <- f$inverse(f$link(capi_last) + gap)
    bind_rows(
      boot_summary(first - last) |> mutate(quantity = "Corrected decline"),
      boot_summary(1 - (first - last) / (capi_first - capi_last)) |>
        mutate(quantity = "Reportability share")
    )
  })) |>
  unnest(res)

save_tab(partial, "tableA6c_reportability_partial")

partial |>
  filter(scale == "Logit", kappa == 1, quantity == "Reportability share") |>
  left_join(reportability |> filter(outcome == "share_ge5", quantity == "Reportability share") |>
              select(item, scenario, expected = estimate), by = c("item", "scenario")) |>
  pwalk(\(item, scenario, estimate, expected, ...) {
    check_close(estimate, expected, 1e-8, glue::glue("kappa = 1 reproduces {scenario} ({item})"))
  })

# Worked example of the bridge: a later self-administered estimate
# p_sa is put on the 2023 interview scale as plogis(qlogis(p_sa) - g). The
# interval carries the bootstrap error in g; the new wave's own sampling error
# (se_sa, independent sample) is added on the logit scale.
bridge_example <- tibble(item = curve_items, p_sa = 0.40, se_sa = 0.015) |>
  mutate(res = pmap(list(item, p_sa, se_sa), \(i, p, se) {
    g_reps <- qlogis(rep_of(i, "share_ge5", last_year, "Self-administered")) -
      qlogis(rep_of(i, "share_ge5", last_year, "Interviewer-administered"))
    se_logit <- sqrt(var(g_reps[-1]) + (se / (p * (1 - p)))^2)
    tibble(g = g_reps[1], g_se = sd(g_reps[-1]), on_interview_scale = plogis(qlogis(p) - g_reps[1]),
           lower = plogis(qlogis(p) - g_reps[1] - 1.96 * se_logit),
           upper = plogis(qlogis(p) - g_reps[1] + 1.96 * se_logit))
  })) |>
  unnest(res)

save_tab(bridge_example, "tableA5c_bridge_example")

series_all <- bind_rows(
  estimates |> mutate(scenario = NA_character_),
  corrected |> mutate(series = "Corrected to self-administration")
) |>
  mutate(summ = map(reps, boot_summary)) |>
  select(-reps) |>
  unnest(summ) |>
  mutate(item_label = factor(mn_labels[item], levels = mn_labels))

save_tab(series_all, "series_1996_2023")

# Figure 4 ---------------------------------------------------------------------------
# With annotate = TRUE (Fig 4, not the nine-panel Fig A1) the band edges are labelled
# directly, and the one observed quantity, the 2023 mode gap, is marked as a bracket
# so it reads apart from the assumed band.

scenario_lines <- c(`Constant gap` = "dashed", `Growing gap` = "dotted")

plot_series <- function(items, annotate = FALSE) {
  d <- series_all |> filter(outcome == "share_ge5", item %in% items)

  band <- d |>
    filter(series == "Corrected to self-administration") |>
    select(item_label, year, scenario, estimate) |>
    pivot_wider(names_from = scenario, values_from = estimate)

  edges <- d |> filter(series == "Corrected to self-administration")
  observed <- d |> filter(series != "Corrected to self-administration")

  p <- ggplot(mapping = aes(year, 100 * estimate)) +
    geom_ribbon(
      data = band,
      aes(x = year, ymin = 100 * pmin(`Constant gap`, `Growing gap`),
          ymax = 100 * pmax(`Constant gap`, `Growing gap`)),
      inherit.aes = FALSE, fill = arm_colours[["Sequential"]], alpha = 0.15
    ) +
    geom_line(data = edges, aes(linetype = scenario), colour = arm_colours[["Sequential"]]) +
    geom_line(data = filter(observed, series == "Interviewer-administered"),
              colour = arm_colours[["CAPI"]]) +
    geom_pointrange(data = observed, aes(ymin = 100 * lower, ymax = 100 * upper, colour = series),
                    size = 0.25) +
    facet_wrap(vars(item_label)) +
    scale_x_continuous(breaks = c(1996, 2006, 2016, 2023)) +
    scale_colour_manual(values = c(`Interviewer-administered` = arm_colours[["CAPI"]],
                                   `Self-administered` = arm_colours[["Concurrent"]]),
                        name = NULL) +
    scale_linetype_manual(values = scenario_lines, name = "Corrected to self-administration",
                          guide = if (annotate) "none" else "legend") +
    labs(
      x = NULL, y = "Rating the criterion important (5-7), %",
      caption = str_wrap(str_c(
        "1996 PAPI; 2006-2023 CAPI; 2023 self-administered = both randomised arms. ",
        "Band: interview series corrected to self-administration, between a constant 2023 ",
        "mode gap and a gap growing linearly from zero in 1996 (logit scale)."
      ), 120)
    ) +
    theme_paper() +
    theme(legend.box = "vertical", legend.title = element_text(size = rel(0.9)))

  if (!annotate) return(p)

  edge_labels <- edges |>
    filter(year %in% c(2006, 2016)) |>
    summarise(estimate = mean(estimate), .by = c(item_label, scenario)) |>
    mutate(year = 2011, label = str_to_lower(scenario),
           estimate = estimate + if_else(scenario == "Constant gap", 0.07, -0.04))

  gap <- observed |>
    filter(year == last_year) |>
    select(item_label, series, estimate) |>
    pivot_wider(names_from = series, values_from = estimate) |>
    rename(capi = `Interviewer-administered`, self = `Self-administered`) |>
    mutate(label = str_c("observed\nmode gap\n+", format(round(100 * (self - capi), 1), nsmall = 1), " pp"))

  bracket_x <- last_year + 1.2
  p +
    geom_text(data = edge_labels, aes(label = label), size = 2.7,
              colour = arm_colours[["Sequential"]]) +
    geom_segment(data = gap, aes(x = bracket_x, xend = bracket_x, y = 100 * capi, yend = 100 * self),
                 inherit.aes = FALSE, linewidth = 0.4) +
    geom_segment(data = gap, aes(x = bracket_x - 0.4, xend = bracket_x, y = 100 * capi, yend = 100 * capi),
                 inherit.aes = FALSE, linewidth = 0.4) +
    geom_segment(data = gap, aes(x = bracket_x - 0.4, xend = bracket_x, y = 100 * self, yend = 100 * self),
                 inherit.aes = FALSE, linewidth = 0.4) +
    geom_text(data = gap, aes(x = bracket_x + 0.6, y = 100 * (capi + self) / 2, label = label),
              inherit.aes = FALSE, hjust = 0, size = 2.6, lineheight = 0.9) +
    coord_cartesian(xlim = c(first_year, last_year + 7), clip = "off")
}

save_fig(plot_series(c("mn01", "mn02"), annotate = TRUE), "fig4_series", height = 100)
save_fig(plot_series(mn_items), "figA1_series_all_items", height = 175)

# Figure 5: reportability share by assumed 1996 gap -----------------------------
# Reference lines at a quarter, a third and a half, labelled where the curve crosses
# them (Table A6b), and the two Fig 4 scenarios labelled at the curve's ends.

plot_sensitivity <- function() {
  curve <- sensitivity |>
    filter(outcome == "share_ge5", quantity == "Reportability share") |>
    select(item, item_label, gap_ratio, estimate, lower, upper) |>
    left_join(
      sensitivity |>
        filter(outcome == "share_ge5", quantity == "Gap in 1996") |>
        select(item, gap_ratio, gap_1996 = estimate),
      by = c("item", "gap_ratio")
    )

  scenarios <- curve |>
    filter(gap_ratio %in% c(0, 1)) |>
    mutate(
      scenario = if_else(gap_ratio == 1, "Constant gap", "Growing gap"),
      label = if_else(gap_ratio == 1, "constant gap:\nsame logit gap\nas in 2023",
                      "growing gap: no gap in 1996"),
      x = 100 * gap_1996 + if_else(gap_ratio == 1, -0.8, 0.5),
      hjust = if_else(gap_ratio == 1, 1, 0),
      vjust = if_else(gap_ratio == 1, 1.2, -0.8)
    )

  # Threshold labels sit at the right end of each reference line, clear of the curve
  target_names <- c(`0.25` = "a quarter", `0.333` = "a third", `0.5` = "a half")
  crossings <- thresholds |>
    filter(!is.na(gap_1996)) |>
    mutate(label = str_c(target_names[as.character(round(target, 3))], " if the 1996 gap was <= ",
                         format(round(100 * gap_1996, 1), nsmall = 1), " pp"))

  ggplot(curve, aes(100 * gap_1996, 100 * estimate)) +
    geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey70") +
    geom_hline(yintercept = 100 * c(0.25, 1 / 3, 0.5), linewidth = 0.3, colour = "grey60",
               linetype = "dashed") +
    geom_ribbon(aes(ymin = 100 * lower, ymax = 100 * upper),
                fill = arm_colours[["Sequential"]], alpha = 0.15) +
    geom_line(colour = arm_colours[["Sequential"]]) +
    geom_point(data = crossings, aes(100 * gap_1996, 100 * target), size = 1.3) +
    geom_text(data = crossings, aes(x = Inf, y = 100 * target, label = label),
              hjust = 1, vjust = -0.4, size = 2.5) +
    geom_point(data = scenarios, aes(shape = scenario), size = 2) +
    geom_text(data = scenarios, aes(x = x, label = label, hjust = hjust, vjust = vjust),
              size = 2.5, lineheight = 0.9, colour = arm_colours[["Sequential"]]) +
    facet_wrap(vars(item_label)) +
    scale_shape_manual(values = c(`Constant gap` = 16, `Growing gap` = 17), guide = "none") +
    scale_y_continuous(breaks = c(0, 25, 33, 50), labels = c("0", "25", "33", "50"),
                       expand = expansion(mult = c(0.2, 0.08))) +
    labs(
      x = "Assumed mode gap in 1996, percentage points\n(0 = no interviewer effect; right end = same logit gap as in 2023)",
      y = "Share of the 1996-2023 decline\ndue to reporting, %",
      caption = str_wrap(str_c(
        "Read: choose an assumed 1996 gap on the x-axis; the curve gives the share of the measured ",
        "1996-2023 decline that reporting accounts for. Ribbon: 95% bootstrap CI."
      ), 120)
    ) +
    theme_paper()
}

save_fig(plot_sensitivity(), "fig5_sensitivity", height = 95)

reportability |>
  filter(outcome == "share_ge5", item %in% c("mn01", "mn02")) |>
  mutate(across(c(estimate, lower, upper),
                \(x) if_else(quantity == "Reportability share", round(100 * x), round(100 * x, 1)))) |>
  select(item, scenario, quantity, estimate, lower, upper) |>
  print(n = 20)

# Crossings of the curve (Table A6b), printed to the log
thresholds |>
  mutate(across(c(target, gap_ratio), \(x) round(100 * x)),
         gap_1996 = round(100 * gap_1996, 1)) |>
  print(n = 20)
