# Triangulation: ESS switch (and revert). Nine countries fielded R10 (2020-22)
# by self-completion (ESS10SC: AT, CY, DE, ES, IL, LV, PL, RS, SE) after a
# face-to-face R9 (2018/19); R11 (2023/24) returned to face-to-face. A period
# effect would not reverse in step with mode. Estimate: country and round fixed
# effects plus a dummy for self-completion country-rounds, on the core
# immigration items. With R9-R10 only this is a two-period DiD (switchers vs
# countries that stayed face-to-face); adding R11 turns it into switch-and-revert.
#
# Input: ESS data-portal Stata files in data/raw/ whose names start with "ESS"
# (one combined subset file or one file per round). The design is taken from the
# source file (`name`): ESS10SC = self-completion, all other = face-to-face.
# Realised mode (`mode`) is kept for description; R10 face-to-face countries
# include some video interviews.

source(here::here("code", "helpers.R"))
suppressPackageStartupMessages(library(survey))

# ESS_DIR can point the script to another input folder
ess_dir <- Sys.getenv("ESS_DIR", unset = path_raw)
ess_files <- fs::dir_ls(ess_dir, regexp = "/ESS[^/]*\\.dta$")

ess_items <- c(
  imsmetn = "Allow: same race/ethnic group",
  imdfetn = "Allow: different race/ethnic group",
  impcntr = "Allow: poorer countries outside Europe",
  imbgeco = "Good for the economy",
  imueclt = "Enriches cultural life",
  imwbcnt = "Makes country a better place"
)

# 1-4 items (1 allow many ... 4 allow none) and 0-10 items (0 bad ... 10 good);
# ESS codes refusals/don't know/no answer as 7-9 and 77-99
allow_items <- c("imsmetn", "imdfetn", "impcntr")
valid_range <- \(item) if (item %in% allow_items) 1:4 else 0:10

read_ess <- function(file) {
  read_dta(file, col_select = c(name, cntry, essround, idno, anweight,
                                any_of(c("psu", "mode")), any_of(names(ess_items)))) |>
    mutate(across(where(is.labelled), zap_labels))
}

ess <- map(ess_files, read_ess) |> list_rbind()
items_present <- intersect(names(ess_items), names(ess))

if (nrow(ess) == 0 || length(items_present) == 0) {
  message("ESS triangulation skipped: no ESS file in data/raw/ or none of the items ",
          str_flatten(names(ess_items), ", "), " in it. Re-download with these variables.")
} else {
  if (!all(names(ess_items) %in% items_present)) {
    message("ESS items missing: ", str_flatten(setdiff(names(ess_items), items_present), ", "))
  }
  if (!11 %in% ess$essround) message("ESS R11 not included: estimating the R9-R10 switch only.")

  ess <- ess |>
    filter(essround %in% 9:11) |>
    mutate(
      across(all_of(items_present), \(x) if_else(x %in% valid_range(cur_column()), x, NA)),
      # Restrictive attitude on every item: 1 = most restrictive end, 0 = most open
      across(any_of(allow_items), \(x) (x - 1) / 3, .names = "{.col}_r"),
      across(any_of(setdiff(items_present, allow_items)), \(x) (10 - x) / 10, .names = "{.col}_r"),
      design = if_else(str_detect(name, "SC"), "Self-completion", "Face-to-face"),
      self_completion = design == "Self-completion",
      psu_id = str_c(cntry, essround, coalesce(as.character(psu), as.character(idno)), sep = "_")
    )

  pattern <- ess |>
    distinct(cntry, essround, design) |>
    pivot_wider(names_from = essround, values_from = design, names_prefix = "r")

  # Countries face-to-face in R9 (and in R11 when available) and observed in R10
  sample_countries <- pattern |>
    filter(r9 == "Face-to-face", !is.na(r10)) |>
    filter(if (has_name(pattern, "r11")) r11 %in% "Face-to-face" else TRUE) |>
    mutate(group = if_else(r10 == "Self-completion", "Switched to self-completion",
                           "Face-to-face throughout"))

  save_tab(pattern, "tableA14c_ess_mode_pattern")

  panel <- ess |> inner_join(select(sample_countries, cntry, group), by = "cntry")

  means <- panel |>
    pivot_longer(ends_with("_r"), names_to = "item", values_to = "value") |>
    filter(!is.na(value)) |>
    summarise(value = weighted.mean(value, anweight), n = n(),
              .by = c(cntry, group, essround, design, item)) |>
    mutate(item = str_remove(item, "_r$"), item_label = ess_items[item])

  save_tab(means, "tableA14d_ess_means")

  # Mode effect: two-way fixed effects, design-based SEs clustered on PSU
  design <- svydesign(ids = ~psu_id, weights = ~anweight, data = panel)
  mode_effects <- items_present |>
    map(\(i) {
      fit <- svyglm(reformulate(c("self_completion", "factor(cntry)", "factor(essround)"),
                                response = str_c(i, "_r")), design = design)
      broom::tidy(fit, conf.int = TRUE) |>
        filter(term == "self_completionTRUE") |>
        mutate(item = i, item_label = ess_items[i])
    }) |>
    list_rbind()

  save_tab(mode_effects, "tableA14a_ess_mode_effects")

  # Country-specific switch-and-revert contrast: each switcher's R10 deviation
  # from the mean of its R9 and R11, minus the same deviation averaged over the
  # face-to-face countries (unweighted across countries). Descriptive, no SEs.
  country_effects <- NULL
  if (all(9:11 %in% means$essround)) {
    deviation <- means |>
      select(cntry, group, essround, item, value) |>
      pivot_wider(names_from = essround, values_from = value, names_prefix = "r") |>
      mutate(dev = r10 - (r9 + r11) / 2)

    country_effects <- deviation |>
      filter(group == "Switched to self-completion") |>
      left_join(
        deviation |>
          filter(group == "Face-to-face throughout") |>
          summarise(dev_stayers = mean(dev, na.rm = TRUE), .by = item),
        by = "item"
      ) |>
      mutate(effect = dev - dev_stayers, item_label = ess_items[item])

    save_tab(country_effects, "tableA14b_ess_country_effects")

    country_effects |>
      select(cntry, item, effect) |>
      mutate(effect = round(100 * effect, 1)) |>
      pivot_wider(names_from = item, values_from = effect) |>
      print()
  }

  # Figure: country switch-and-revert effects (descriptive) and the pooled
  # fixed-effects estimate with 95% CI, in points on 0-100 (restrictive = right)
  item_short <- c(
    imsmetn = "Allow: same ethnic group",
    imdfetn = "Allow: different ethnic group",
    impcntr = "Allow: poorer non-European",
    imbgeco = "Good for the economy",
    imueclt = "Enriches cultural life",
    imwbcnt = "Makes country a better place"
  )
  pooled_label <- "Pooled (FE)"

  fig_data <- bind_rows(
    mode_effects |>
      transmute(row = pooled_label, item, effect = estimate, conf.low, conf.high,
                type = "pooled"),
    country_effects |>
      transmute(row = cntry, item, effect, type = "country")
  ) |>
    mutate(across(c(effect, conf.low, conf.high), \(x) 100 * x))

  country_order <- fig_data |>
    filter(type == "country") |>
    summarise(mean_effect = mean(effect, na.rm = TRUE), .by = row) |>
    arrange(mean_effect) |>
    pull(row)

  fig_data <- fig_data |>
    mutate(
      row = factor(row, levels = c(country_order, pooled_label)),
      item = factor(item_short[item], levels = item_short[names(ess_items)])
    )

  fig_ess <- fig_data |>
    ggplot(aes(effect, row)) +
    geom_vline(xintercept = 0, colour = "grey60", linewidth = 0.3) +
    { if (!is.null(country_effects)) geom_hline(yintercept = length(country_order) + 0.5,
                                                colour = "grey80", linewidth = 0.3) } +
    geom_linerange(aes(xmin = conf.low, xmax = conf.high),
                   data = \(d) filter(d, type == "pooled"), colour = arm_colours[["CAPI"]]) +
    geom_point(data = \(d) filter(d, type == "pooled"), shape = 18, size = 2.6,
               colour = arm_colours[["CAPI"]]) +
    geom_point(data = \(d) filter(d, type == "country"), size = 1.8,
               colour = arm_colours[["Concurrent"]]) +
    facet_wrap(vars(item), ncol = 3) +
    # Fix the row order: layers are trained one at a time, which would put the
    # pooled row (first layer) at the bottom
    scale_y_discrete(limits = levels(fig_data$row)) +
    labs(x = "Change under self-completion (points on 0-100, positive = more restrictive)", y = NULL) +
    theme_paper()

  save_fig(fig_ess, "figA4_ess_switch", height = 110)

  print(count(sample_countries, group))
  print(mode_effects |> select(item, estimate, conf.low, conf.high, p.value) |>
          mutate(across(where(is.numeric), \(x) round(x, 3))))
}
