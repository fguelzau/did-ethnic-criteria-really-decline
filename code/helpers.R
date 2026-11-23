# Shared paths, constants and helpers. Sourced at the top of every script.

suppressPackageStartupMessages({
  library(tidyverse)
  library(haven)
  library(here)
})

# Paths ------------------------------------------------------------------------

path_raw   <- here("data", "raw")
path_clean <- here("data", "clean")
path_fig   <- here("output", "figures")
path_tab   <- here("output", "tables")
path_text  <- here("output", "text")

raw <- function(file) here(path_raw, file)
clean <- function(file) here(path_clean, file)

# Constants --------------------------------------------------------------------

mn_items <- str_c("mn0", 1:9)

mn_labels <- c(
  mn01 = "Born in Germany",
  mn02 = "German descent",
  mn03 = "Speaks German",
  mn04 = "Lived here long",
  mn05 = "Adapts to way of life",
  mn06 = "Christian",
  mn07 = "No crimes",
  mn08 = "Self-supporting",
  mn09 = "Accepts Basic Law"
)

# Items of the 2023 item gradient outside the mn battery (22_item_gradient.R, Table A3)
placebo_labels <- c(
  ma01b = "Should adapt their lifestyle", ma02 = "Should leave when jobs are scarce",
  ma03 = "No political activity", ma04 = "Should marry among themselves",
  mp02 = "Burden on social welfare", mp04 = "Cause housing problems",
  mp06 = "Take jobs away", mp07 = "Commit crimes more often",
  mp05 = "Help secure pensions", mp10 = "Make Germany more tolerant",
  mp12 = "Fill skilled-labour gap",
  pt01 = "Health service", pt02 = "Constitutional Court", pt03 = "Bundestag",
  pt04 = "Municipal administration", pt06 = "Catholic Church", pt07 = "Protestant Church",
  pt08 = "Judicial system", pt09 = "Television",
  pt10 = "Newspapers", pt11 = "Universities", pt12 = "Federal government",
  pt14 = "Police", pt15 = "Political parties", pt19 = "European Commission",
  pt20 = "European Parliament",
  li01 = "Family and children", li02 = "Job and work", li03 = "Leisure time",
  li04 = "Friends", li05 = "Relatives", li07 = "Politics", li08 = "Neighbours",
  li09 = "Church", li10 = "Religion",
  mg01 = "Italians", mg02 = "Ethnic Germans from Eastern Europe", mg03 = "Asylum seekers",
  mg04 = "Turks", mg05 = "Jews", mg06 = "Poles"
)

# Diehl & Tucci (2011) two-factor structure (mn09 not fielded in 1996)
ethnic_items <- c("mn01", "mn02", "mn04")
civic_items  <- c("mn03", "mn05", "mn06", "mn07", "mn08")

# Typology. Diehl & Tucci (2010, 2011) sort respondents into four types by whether
# they rate ethnic and civic-cultural criteria as important. Their texts do not
# identify the coding (index or item level, cut-off, item set) and no rule
# reproduces all 16 published shares, so the rule below is our operationalisation:
# mean of mn01 + mn02 and mean of four civic-cultural items, each >= 6 on the
# stored 1-7 scale. It comes within ~2 pp of their published 1996/2006 "mainly
# ethnic" shares (12 -> 4 West, 17 -> 3 East). `typology_rules` holds the other
# codings their texts admit; 23_typology.R reruns the 2023 contrast under each.
dt_cutoff <- 6

typology_ethnic <- c("mn01", "mn02")
typology_civic  <- c("mn03", "mn05", "mn07", "mn08")
typology_levels <- c("Mainly ethnic", "Both", "Mainly civic", "Neither")

dt_typology <- function(df) {
  ethnic <- rowMeans(select(df, all_of(typology_ethnic))) >= dt_cutoff
  civic <- rowMeans(select(df, all_of(typology_civic))) >= dt_cutoff
  case_when(
    ethnic & !civic ~ "Mainly ethnic",
    ethnic & civic ~ "Both",
    !ethnic & civic ~ "Mainly civic",
    !ethnic & !civic ~ "Neither"
  ) |> factor(levels = typology_levels)
}

# Alternative codings of the typology (Table A4e). Each returns the two
# indicators; rows with a missing item in either set are NA.
typology_type <- function(ethnic, civic) {
  case_when(
    ethnic & !civic ~ "Mainly ethnic",
    ethnic & civic ~ "Both",
    !ethnic & civic ~ "Mainly civic",
    !ethnic & !civic ~ "Neither"
  ) |> factor(levels = typology_levels)
}

index_rule <- function(ethnic, civic, cutoff) {
  \(df) typology_type(rowMeans(select(df, all_of(ethnic))) >= cutoff,
                      rowMeans(select(df, all_of(civic))) >= cutoff)
}

# Item level: every ethnic item and at least `civic_min` civic items >= cutoff
item_rule <- function(ethnic, civic, cutoff, civic_min = length(civic)) {
  \(df) typology_type(rowSums(select(df, all_of(ethnic)) >= cutoff) == length(ethnic),
                      rowSums(select(df, all_of(civic)) >= cutoff) >= civic_min)
}

typology_rules <- list(
  `Main rule: mean of born, descent >= 6; mean of four civic items >= 6` =
    index_rule(typology_ethnic, typology_civic, dt_cutoff),
  `Index, three ethnic items: adds long residence to the ethnic mean` =
    index_rule(ethnic_items, typology_civic, dt_cutoff),
  `Index, full item lists: adds long residence and church membership` =
    index_rule(ethnic_items, civic_items, dt_cutoff),
  `Item level: all three ethnic items >= 5; at least four of five civic items >= 5` =
    item_rule(ethnic_items, civic_items, 5, civic_min = 4),
  `Closest fit: all three ethnic items >= 5; language, adapts, self-supporting all >= 5` =
    item_rule(ethnic_items, c("mn03", "mn05", "mn08"), 5)
)

arm_levels <- c("CAPI", "Sequential", "Concurrent")

# Analysis universe: German citizens (the population whose views on
# naturalisation criteria the series is read as describing)
universe_german_only <- TRUE

n_boot <- 999
set.seed(20230101)

# Recoding ---------------------------------------------------------------------

# ALLBUS and ISSP code missing values as negative numbers; turn them into NA
na_negative <- function(x) {
  if (is.numeric(x)) replace(x, x < 0, NA) else x
}

# Drop value labels on numeric attitude items once negative codes are NA
as_item <- function(x) as.numeric(zap_labels(na_negative(x)))

# Bootstrap --------------------------------------------------------------------

# Weight matrix: full-sample weight in column 1, bootstrap replicate weights
# after it. Resamples sample points (psu) within strata, so strata must be ones
# the points nest in (region, wave x region), not the 2023 design arm: arms were
# randomised within points, and resampling points keeps the arms together.
boot_weights <- function(df, strata) {
  design <- df |>
    srvyr::as_survey_design(ids = psu, weights = weight, strata = all_of(strata)) |>
    srvyr::as_survey_rep(type = "bootstrap", replicates = n_boot)
  cbind(df$weight, weights(design, type = "analysis"))
}

# Weighted mean of y within a logical group, for the full sample and each replicate
boot_mean <- function(y, group, w) {
  keep <- group & !is.na(y)
  colSums(w[keep, , drop = FALSE] * y[keep]) / colSums(w[keep, , drop = FALSE])
}

# Summarise a vector of (full-sample, replicate...) estimates
boot_summary <- function(x) {
  tibble(
    estimate = x[1],
    se = sd(x[-1]),
    lower = quantile(x[-1], 0.025, names = FALSE),
    upper = quantile(x[-1], 0.975, names = FALSE)
  )
}

# Output -----------------------------------------------------------------------

theme_paper <- function(base_size = 10) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold", hjust = 0),
      legend.position = "bottom",
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.caption = element_text(hjust = 0, size = rel(0.8))
    )
}

arm_colours <- c(CAPI = "#4a4a4a", Sequential = "#1b7fb3", Concurrent = "#d9731a")

save_fig <- function(plot, name, width = 170, height = 120) {
  fs::dir_create(path_fig)
  walk(c("pdf", "png"), \(ext) ggsave(
    here(path_fig, str_c(name, ".", ext)), plot,
    width = width, height = height, units = "mm", dpi = 300
  ))
  invisible(plot)
}

save_tab <- function(df, name) {
  fs::dir_create(path_tab)
  write_csv(df, here(path_tab, str_c(name, ".csv")))
  invisible(df)
}

save_text <- function(lines, name) {
  fs::dir_create(path_text)
  write_lines(lines, here(path_text, name))
  invisible(lines)
}

# Stop the pipeline when a reproduction check fails
check_close <- function(observed, expected, tol, what) {
  if (abs(observed - expected) > tol) {
    stop(glue::glue("Check failed for {what}: observed {round(observed, 3)}, ",
                    "expected {expected} (tolerance {tol})"))
  }
  message(glue::glue("OK {what}: {round(observed, 3)} (expected {expected})"))
  invisible(TRUE)
}
