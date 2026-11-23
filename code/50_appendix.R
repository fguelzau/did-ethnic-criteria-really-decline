# Online appendix: assembles Tables A1-A14, Figures A1-A4 and the cross-national text
# and output/figures into output/text/appendix.docx, with a list of contents.
# Numbering follows the order of first mention in the manuscript (map in
# README.md). Run after the analysis scripts. Styles come from pandoc's default
# template with smaller table text (the manuscript's Word template breaks pandoc
# tables).

source(here::here("code", "helpers.R"))

tab <- function(name) read_csv(here(path_tab, str_c(name, ".csv")), show_col_types = FALSE)

# Formatting -------------------------------------------------------------------

# Number with a typographic minus; scale = 100 turns shares into percentages
num <- function(x, digits = 1, scale = 1) {
  if_else(is.na(x), "—", sprintf("%.*f", as.integer(digits), scale * x)) |>
    str_replace("^-", "−")
}
ci <- function(est, lower, upper, digits = 1, scale = 100) {
  # Non-breaking space inside the brackets, so a narrow cell wraps before "[" only
  if_else(is.na(est), "—", str_c(num(est, digits, scale), " [", num(lower, digits, scale), ",\u00a0",
                                 num(upper, digits, scale), "]"))
}
est_se <- function(est, se, digits = 2) str_c(num(est, digits), " (", num(se, digits), ")")

item_label <- \(item) coalesce(mn_labels[item], placebo_labels[item], item) |> unname()

# Show a grouping column only on the first row of each group
blank_repeats <- \(x) if_else(x == lag(x, default = ""), "", x)

# Pipe table whose separator dashes set the relative column widths: the longest
# unbreakable token of a column, or its longest cell up to 28 characters. Rows
# longer than pandoc's line width make the table span the full text width.
md_table <- function(df) {
  df <- mutate(df, across(everything(), \(x) coalesce(as.character(x), "")))
  widths <- map2_int(names(df), df, \(name, x) {
    cells <- c(name, x)
    max(nchar(unlist(str_split(cells, " "))), min(max(nchar(cells)), 28L))
  })
  row <- \(x) str_c("| ", str_flatten(x, " | "), " |")
  c(row(names(df)),
    str_c("|", str_flatten(map_chr(widths, \(w) strrep("-", w + 2)), "|"), "|"),
    pmap_chr(df, \(...) row(c(...))),
    "")
}

# Each element: heading, optional panel title, table, note
block <- function(title, ..., note = NULL) {
  c(str_c("## ", title), "", unlist(list(...)),
    if (!is.null(note)) c(str_c("*Note:* ", note), ""), "")
}
panel <- function(title, df) c(str_c("**", title, "**"), "", md_table(df), "")
figure <- function(title, file, note = NULL) {
  block(title, str_c("![](", here(path_fig, str_c(file, ".png")), "){width=16cm}"), "", note = note)
}

ci_note <- str_c("95% confidence intervals in brackets, from ", n_boot, " bootstrap replicates ",
                 "that resample sample points within East/West (× wave for the series). ")
pop_note <- "German citizens, weighted with the ALLBUS East/West person weight. "
arm_note <- str_c("Differences are intention-to-treat contrasts of the randomised design arm with ",
                  "the CAPI arm, in percentage points unless stated otherwise. ")

# Table A1: analysis universe ----------------------------------------------------

a1a <- tab("tableA1a_universe_n") |> rename(Universe = universe)

a1b <- tab("tableA1b_universe_contrasts") |>
  filter(outcome == "share_ge5", contrast == "Self-administered") |>
  mutate(value = ci(estimate, lower, upper)) |>
  select(item, universe, value) |>
  pivot_wider(names_from = universe, values_from = value) |>
  mutate(item = item_label(item)) |>
  rename(Criterion = item)

a1c <- tab("tableA1c_universe_aamb") |>
  mutate(value = num(aamb), col = str_c(universe, ": ", contrast)) |>
  select(item, col, value) |>
  pivot_wider(names_from = col, values_from = value) |>
  mutate(item = item_label(item)) |>
  rename(Criterion = item)

a1d <- tab("tableA1d_balance") |>
  mutate(variable = recode(variable, age_group = "Age", female = "Sex", educ3 = "Education",
                           region = "Region"),
         level = case_when(variable == "Sex" & level == "TRUE" ~ "Female",
                           variable == "Sex" ~ "Male", .default = level)) |>
  transmute(Variable = blank_repeats(variable), Group = level,
            CAPI = num(CAPI, 1, 100), Sequential = num(Sequential, 1, 100),
            Concurrent = num(Concurrent, 1, 100),
            `Self-administered − CAPI` = ci(estimate, lower, upper))

a1e <- bind_rows(
  tab("tableA1e_reweighted") |> rename(Weights = weights),
  tab("tableA1e_cluster_bootstrap") |>
    mutate(Weights = str_c("East/West weight; bootstrap over ", str_to_lower(bootstrap)))
) |>
  transmute(Criterion = item_label(item), Specification = Weights, `Design effect` = ci(estimate, lower, upper)) |>
  arrange(Criterion) |>
  mutate(Criterion = blank_repeats(Criterion))

table_a1 <- block(
  "Table A1. Analysis universe, composition of the design arms and alternative inference",
  panel("(a) Respondents answering the battery, by design arm", a1a),
  panel("(b) Design effect on the share rating a criterion important (5–7), self-administered arms pooled", a1b),
  panel("(c) Average absolute marginal bias (AAMB) against CAPI", a1c),
  panel("(d) Composition of the responding German citizens by design arm, per cent", a1d),
  panel("(e) Design effect on born in Germany and German descent (5–7) under alternative weights and bootstrap clusters", a1e),
  note = str_c("Asimov's universe: all citizenships, aged 80 or younger, with sex and education codable. ",
               "She rakes to the Microcensus; both universes here use the ALLBUS East/West person weight. ",
               "(b) in percentage points; ", ci_note, "(c) The pipeline checks the values for born in ",
               "Germany and German descent against Asimov's Table A1 (21_design_effects.R). (b) and (c) are ",
               "estimated from the 2023 compact file (ZA8833) for both universes, so the citizen values can ",
               "differ from Table A2 by 0.1 point. (d) Weighted shares; municipality size is not released for ",
               "2023. (e) Raked: the self-administered arms raked to the CAPI arm's weighted margins of age group, ",
               "sex, education and region, re-estimated in every bootstrap replicate; complete cases on these ",
               "variables. Interviewer-connected clusters: sample points that share a CAPI interviewer are merged ",
               "(106 clusters; 91 of 146 interviewers work in a single point), and the clusters are resampled ",
               "without regional strata because some span East and West.")
)

# Table A2: design effects by item and arm -----------------------------------------

a2 <- tab("tableA2_design_effects")

a2_by_arm <- function(out, digits, scale) {
  a2 |>
    filter(outcome == out) |>
    mutate(value = ci(estimate, lower, upper, digits, scale)) |>
    select(item, CAPI = capi, contrast, value) |>
    pivot_wider(names_from = contrast, values_from = value) |>
    mutate(CAPI = num(CAPI, digits, scale), item = item_label(item)) |>
    select(Criterion = item, CAPI, Sequential, Concurrent, `Both self-administered` = `Self-administered`)
}

a2c <- a2 |>
  filter(str_starts(outcome, "cat"), contrast == "Self-administered") |>
  mutate(value = num(estimate, 1, 100), outcome = str_replace(outcome, "cat", "")) |>
  select(item, outcome, value) |>
  pivot_wider(names_from = outcome, values_from = value) |>
  mutate(item = item_label(item)) |>
  rename(Criterion = item)

a2d <- tab("tableA2d_cutpoints") |>
  mutate(value = if_else(outcome == "mean", ci(estimate, lower, upper, 2, 1), ci(estimate, lower, upper))) |>
  select(item, outcome, value) |>
  pivot_wider(names_from = outcome, values_from = value) |>
  transmute(Criterion = item_label(item), `2–7` = share_ge2, `5–7` = share_ge5, `6–7` = share_ge6,
            `7` = share_7, Mean = mean)

a2e <- tab("tableA2e_cumulative_logit") |>
  transmute(Model = blank_repeats(model), Term = term, Estimate = ci(estimate, lower, upper, 2, 1))

a2f <- tab("tableA2f_born_correlations") |>
  transmute(`Born in Germany with` = item_label(with), CAPI = num(CAPI, 2),
            `Self-administered` = num(`Self-administered`, 2))

table_a2 <- block(
  "Table A2. Design effects on the naturalisation criteria, by item and arm",
  panel("(a) Share rating the criterion important (5–7), per cent; differences in percentage points",
        a2_by_arm("share_ge5", 1, 100)),
  panel("(b) Mean on the 1–7 scale; differences in scale points", a2_by_arm("mean", 2, 1)),
  panel("(c) Difference in each response category (1 = not at all important, 7 = very important), both self-administered arms pooled",
        a2c),
  panel("(d) Alternative cut points: difference in the share rating 2–7, 5–7, 6–7 and 7, and in the mean, both self-administered arms pooled",
        a2d),
  panel("(e) Cumulative logit model of the stacked battery: common shift under self-administration and the excess on the descent items",
        a2e),
  panel("(f) Correlation of born in Germany with the other items, by design", a2f),
  note = str_c(pop_note, arm_note, ci_note, "CAPI columns give the level in the interview arm. ",
               "(e) All nine criteria stacked, with item-specific thresholds; the common shift applies to every ",
               "item, the excess terms are added for born in Germany and German descent. The second model ",
               "lets the latent dispersion differ under self-administration (scale term, log ratio), which ",
               "absorbs more emphatic answers at both ends. Logit scale; intervals from 199 bootstrap replicates. ",
               "(f) Weighted Pearson correlations; marry among themselves and burden on social welfare are from ",
               "the attitudes-to-foreigners batteries.")
)

# Table A3: item gradient -----------------------------------------------------------

gradient_group <- function(item) {
  case_when(
    item %in% c("mn01", "mn02") ~ "Naturalisation: descent",
    str_starts(item, "mn") ~ "Naturalisation: other criteria",
    str_starts(item, "ma") ~ "Demands on foreigners",
    item %in% c("mp02", "mp04", "mp06", "mp07") ~ "Foreigners: negative statements",
    str_starts(item, "mp") ~ "Foreigners: positive statements",
    str_starts(item, "pt") ~ "Institutional trust",
    str_starts(item, "li") ~ "Life domains",
    str_starts(item, "mg") ~ "Perceived lifestyle differences"
  )
}

a3 <- tab("tableA3_gradient") |>
  mutate(
    digits = if_else(outcome == "mean", 2, 1),
    scale = if_else(outcome == "mean", 1, 100),
    value = ci(estimate, lower, upper, digits, scale),
    capi = num(capi, 1, 100)
  )

a3_adjusted <- tab("tableA3a_gradient_adjusted") |> select(item, p_holm)

a3b <- tab("tableA3b_did") |>
  mutate(value = if_else(outcome == "mean", ci(estimate, lower, upper, 2, 1), ci(estimate, lower, upper)),
         outcome = recode(outcome, share_ge5 = "Share 5–7", share_7 = "Share 7", mean = "Mean")) |>
  select(item, comparison, outcome, value) |>
  pivot_wider(names_from = outcome, values_from = value, values_fill = "—") |>
  transmute(Criterion = blank_repeats(item_label(item)), `Compared with` = comparison,
            `Share 5–7`, `Share 7`, Mean)

a3c <- tab("tableA3c_polarity") |>
  transmute(Measure = measure, CAPI = num(capi, 1, 100), `Self-administered` = num(sa, 1, 100),
            Difference = ci(estimate, lower, upper))

table_a3 <- block(
  "Table A3. The item gradient: design effects across the 2023 instrument",
  "**(a) Design effect per item**", "",
  md_table(
    a3 |>
      mutate(capi = if_else(outcome == "share_ge5", capi, NA)) |>
      group_by(item) |>
      mutate(capi = first(na.omit(capi))) |>
      ungroup() |>
      select(item, capi, outcome, value) |>
      pivot_wider(names_from = outcome, values_from = value) |>
      mutate(
        group = gradient_group(item),
        order = match(group, unique(gradient_group(c("mn01", "mn03", "ma01b", "mp02", "mp05", "pt01", "li01", "mg01"))))
      ) |>
      arrange(order) |>
      left_join(a3_adjusted, by = "item") |>
      transmute(
        Block = blank_repeats(group), Item = item_label(item), `CAPI 5–7 (%)` = capi,
        `Δ share 5–7` = share_ge5, `Holm p` = if_else(p_holm < 0.001, "<0.001", num(p_holm, 3)),
        `Δ share 7` = share_7, `Δ mean` = mean
      )
  ),
  "",
  panel("(b) Difference in design effects: descent item minus the mean design effect of a comparison set", a3b),
  panel("(c) Acquiescence: agreement with negative and positive statements about foreigners, per cent", a3c),
  note = str_c(pop_note, "Both self-administered arms pooled minus CAPI. Shares in percentage points, ",
               "means in scale points (all items 1–7). ", ci_note,
               "Life domains, trust and perceived lifestyle differences are asked in all CAPI cases and in split A of the ",
               "self-administered arms, like the naturalisation battery. Perceived lifestyle differences: how ",
               "strongly each group differs in its way of life from Germans (1 = not at all, 7 = very strongly); ",
               "descriptive items, not shown in Figure 2. (a) Holm p: two-sided p-value of ",
               "the share 5–7 difference (bootstrap standard error, normal approximation), Holm-adjusted ",
               "over all items of the table. (b) Civic (typology items): speaks German, adapts, no crimes, ",
               "self-supporting. Conduct-related migrant items: the four demands on foreigners and the four ",
               "negative statements; the share 7 is not shown for them because their scales are agreement scales. ",
               "(c) Respondents who answered all seven statements (scores 5–7 = agree). Negative: burden on ",
               "welfare, housing problems, take jobs, crime; positive: pensions, tolerance, skilled labour. ",
               "Balanced agreement: mean of the two poles' agreement shares, a measure of yea-saying; content: ",
               "negative minus positive. Contradictory pair: agrees both that foreigners take jobs and that they ",
               "fill the skilled-labour gap, or both that they burden welfare and that they secure pensions.")
)

# Table A4: typology and scale use --------------------------------------------------

a4_all <- tab("tableA4_typology_checks")
a4_typ <- a4_all |> filter(ethnic_index == "Born + descent")

a4t_a <- a4_typ |>
  filter(str_starts(check, "Strong descent")) |>
  transmute(`Civic stratum` = civic, CAPI = num(capi, 1, 100), `Self-administered` = num(sa, 1, 100),
            Difference = ci(estimate, lower, upper))

a4t_b <- a4_typ |>
  filter(check == "Typology, civic threshold matched to CAPI share") |>
  left_join(tab("typology_arm_differences") |>
              summarise(fixed = mean(estimate), .by = type), by = "type") |>
  transmute(Type = type, `Fixed threshold (mean of both arms)` = num(fixed, 1, 100),
            `Civic threshold matched to CAPI` = ci(estimate, lower, upper))

a4t_c <- a4_typ |>
  filter(is.na(civic), is.na(type)) |>
  transmute(Quantity = check,
            Estimate = if_else(str_starts(check, "Descent minus"), ci(estimate, lower, upper, 2, 1),
                               ci(estimate, lower, upper)))

a4t_d <- a4_all |>
  filter(ethnic_index == "Descent only", !is.na(type)) |>
  mutate(check = if_else(str_detect(check, "matched"), "Civic threshold matched to CAPI", "Fixed threshold")) |>
  transmute(Type = type, check, value = ci(estimate, lower, upper)) |>
  pivot_wider(names_from = check, values_from = value) |>
  bind_rows(
    a4_all |>
      filter(ethnic_index == "Descent only", str_starts(check, "Share of strong")) |>
      transmute(Type = str_replace(check, "Share of strong descent endorsers also civic", "Share of strong endorsers also civic"),
                `Fixed threshold` = ci(estimate, lower, upper), `Civic threshold matched to CAPI` = "")
  )

a4t_e <- tab("tableA4e_typology_rules") |>
  mutate(value = ci(estimate, lower, upper),
         column = if_else(universe == "German citizens", type, str_c(type, ", German since birth"))) |>
  filter(universe == "German citizens" | type == "Both") |>
  mutate(Rule = str_replace_all(rule, ">=", "≥"), `Deviation from published shares, mean (max)` =
           str_c(num(fit_mean), " (", num(fit_max), ")")) |>
  select(Rule, `Deviation from published shares, mean (max)`, column, value) |>
  pivot_wider(names_from = column, values_from = value)

table_a4 <- block(
  "Table A4. The typology and general scale use",
  panel("(a) Share strongly endorsing descent (mean of born and descent ≥ 6), within civic strata, per cent", a4t_a),
  panel("(b) Typology shift, self-administered minus CAPI, with the civic threshold of the self-administered arms set to reproduce the CAPI civic share", a4t_b),
  panel("(c) Within-respondent comparison", a4t_c),
  panel("(d) Descent alone as the ethnic index ('German descent' ≥ 6): typology shift, self-administered minus CAPI", a4t_d),
  panel("(e) Alternative codings of the typology: shift in each type, self-administered minus CAPI", a4t_e),
  note = str_c(pop_note, ci_note, "Strong civic endorsement: mean of speaks German, adapts, no crimes and ",
               "self-supporting ≥ 6, as in the typology. (a) conditions on civic endorsement, which is itself ",
               "affected by the design, and is therefore descriptive. (b) The fixed-threshold column averages the ",
               "two arm-specific differences of Figure 3. Matched threshold: a weighted quantile in each replicate; ",
               "respondents tied at the cut-off receive fractional membership, so the civic share equals the CAPI ",
               "share exactly. (c) Descent minus civic: the difference in the within-respondent gap between the two ",
               "indices, scale points; the shares are the proportion of strong descent endorsers who are also ",
               "strongly civic, by design. (d) Repeats (b) and (c) without 'born in Germany', which can also be ",
               "read as jus soli; the fixed-threshold shift pools both self-administered arms. ",
               "(e) Diehl and Tucci (2010, 2011) name the items of each dimension (ethnic: born, descent, long ",
               "residence; civic-cultural: language, adapts, church, no crimes, self-supporting) and a threshold ",
               "of 5 on a scale from 0 to 6, but not whether the threshold applies to an index or to single items, ",
               "nor how items are combined; no rule we tried reproduces all 16 shares they print. The panel repeats ",
               "the pooled contrast under the codings their description admits. Thresholds refer to the stored 1–7 ",
               "scale. Deviation: absolute difference, in percentage points, between each rule's shares and the 16 ",
               "shares in Diehl and Tucci (2010, Figure 1; four types, East and West, 1996 and 2006), computed for ",
               "respondents who are German since birth, as in their analysis. The rule with the full item lists ",
               "puts 16 per cent of West Germans in the mainly civic type in 2006, against a published 57 per ",
               "cent, so it cannot be the original coding; it is shown for completeness.")
)

# Table A5: correction factors ------------------------------------------------------

a5_panel <- function(out, digits, scale) {
  tab("tableA5_correction_factors") |>
    filter(outcome == out) |>
    mutate(value = ci(estimate, lower, upper, digits, scale)) |>
    select(item, quantity, value) |>
    pivot_wider(names_from = quantity, values_from = value) |>
    transmute(
      Criterion = item_label(item),
      `Mode gap 2023` = mode_gap_2023,
      `Change 2016–2023, interview only` = change_2016_2023_capi,
      `Change 2016–2023, interview → self-administered` = change_2016_2023_naive
    )
}

a5c <- tab("tableA5c_bridge_example") |>
  transmute(Criterion = item_label(item), `Hypothetical self-administered estimate` = num(p_sa, 1, 100),
            `Logit gap g (SE)` = est_se(g, g_se),
            `On the 2023 interview scale` = ci(on_interview_scale, lower, upper))

table_a5 <- block(
  "Table A5. Correction factors for reading the series across the mode change",
  panel("(a) Share rating the criterion important (5–7), percentage points", a5_panel("share_ge5", 1, 100)),
  panel("(b) Mean, scale points", a5_panel("mean", 2, 1)),
  panel("(c) Worked example: a later self-administered estimate placed on the interview scale", a5c),
  note = str_c(pop_note, "The mode gap is the 2023 difference between the pooled self-administered arms ",
               "and CAPI. The last column compares the 2016 interview with the 2023 self-administered arms, ",
               "the comparison a continued series would make without correction. ", ci_note,
               "Because the series bootstrap also stratifies by wave, the 2023 intervals can differ from ",
               "Table A2 in the last digit. (c) A hypothetical later wave with 40.0 per cent (standard error 1.5 ",
               "points) is moved to the interview scale as logit⁻¹(logit(p) − g), with g from Equation 1. The ",
               "interval adds the bootstrap variance of g and the sampling variance of the new estimate on the ",
               "logit scale (independent samples). The bridge assumes the same universe and a mix of web and ",
               "paper similar to 2023 (71 per cent web in the sequential, 75 per cent paper in the concurrent ",
               "design), and that the gap has not changed since 2023 on the logit scale. It should not be used ",
               "where the mode mix differs markedly, for a different population, or across a period in which the ",
               "norm itself may have moved; the further from 2023, the weaker the last assumption.")
)

# Table A6: reportability -----------------------------------------------------------

a6a <- tab("tableA6a_reportability_scenarios") |>
  filter(outcome == "share_ge5", item != "mn09") |>
  mutate(key = case_when(
    quantity == "Measured decline" ~ "measured",
    quantity == "Corrected decline" ~ str_c("corrected_", scenario),
    quantity == "Reportability share" ~ str_c("share_", scenario)
  )) |>
  filter(!(quantity == "Measured decline" & scenario == "Growing gap")) |>
  mutate(value = ci(estimate, lower, upper)) |>
  select(item, key, value) |>
  pivot_wider(names_from = key, values_from = value)

# The share is a ratio to the measured decline: only meaningful where that is sizeable
measured_size <- tab("tableA6a_reportability_scenarios") |>
  filter(outcome == "share_ge5", quantity == "Measured decline", scenario == "Constant gap") |>
  select(item, measured_est = estimate)

a6a <- a6a |>
  left_join(measured_size, by = "item") |>
  mutate(across(starts_with("share_"), \(x) if_else(abs(measured_est) >= 0.1, x, "—"))) |>
  transmute(
    Criterion = item_label(item),
    `Measured decline 1996–2023` = measured,
    `Corrected, constant gap` = `corrected_Constant gap`,
    `Reportability share, constant gap (%)` = `share_Constant gap`,
    `Corrected, growing gap` = `corrected_Growing gap`,
    `Reportability share, growing gap (%)` = `share_Growing gap`
  )

a6b <- tab("tableA6b_reportability_thresholds") |>
  transmute(
    Criterion = blank_repeats(item_label),
    `Reportability share` = str_c(num(target, 0, 100), "%"),
    `Largest 1996 gap (pp)` = if_else(is.na(gap_1996), "not reachable", num(gap_1996, 1, 100)),
    `As share of the 2023 gap (%)` = if_else(is.na(gap_ratio), "—", num(gap_ratio, 0, 100))
  )

a6c <- tab("tableA6c_reportability_partial") |>
  filter(quantity == "Reportability share") |>
  mutate(value = ci(estimate, lower, upper, 0), col = str_c(scale, ", ", str_to_lower(scenario))) |>
  select(item, kappa, col, value) |>
  pivot_wider(names_from = col, values_from = value) |>
  transmute(Criterion = blank_repeats(item_label(item)),
            `Part of the 2023 gap due to reporting` = str_c(num(kappa, 0, 100), "%"),
            `Logit, constant gap`, `Logit, growing gap`,
            `Percentage points, constant gap`, `Percentage points, growing gap`)

table_a6 <- block(
  "Table A6. Reportability share of the 1996–2023 decline",
  panel("(a) Two scenarios for the 1996 mode gap, share rating the criterion important (5–7)", a6a),
  panel("(b) Largest 1996 mode gap compatible with a given reportability share", a6b),
  panel("(c) Reportability share (%) if only part of the 2023 gap reflects reporting, on the logit and the percentage-point scale", a6c),
  note = str_c(pop_note, "Declines in percentage points (negative = increase). Constant gap: the 2023 gap ",
               "applies in 1996 on the logit scale. Growing gap: no gap in 1996, growing linearly to its ",
               "2023 size. The reportability share is the part of the measured decline removed by the ",
               "correction; it is shown only where the measured change exceeds 10 points, because it is a ",
               "ratio to that change. Accepts the Basic Law was not asked in 1996. ", ci_note,
               "(b) is read off the sensitivity curve in Figure 5. (c) The remainder of the 2023 gap is treated ",
               "as selection or layout and is not carried back to earlier waves. 100% reproduces (a). On the ",
               "percentage-point scale a constant gap leaves the measured decline unchanged by construction.")
)

# ALLBUS and ISSP series compared: appendix text --------------------------------

text_wedge <- c(
  "## The ALLBUS and ISSP series compared", "",
  str_c("If reportability fell over time, the interview series should have declined faster than a ",
        "comparable series answered without an interviewer (H5). The closest German comparison is the ISSP, ",
        "which asks how important being born in Germany and having German ancestry are for being 'truly ",
        "German'. Because questions and scales differ, each item is standardised within its series and the ",
        "trends are compared, regressing the standardised item on time, series and their interaction ",
        "(Figure A2, Table A7)."), "",
  str_c("The ALLBUS items fell by 0.36 and 0.37 standard deviations per decade between 1996 and 2023. The ",
        "ISSP birth item fell by 0.02 per decade and the ancestry item, available from 2003, by 0.22. The ",
        "direction matches H5: the interview series fell faster. The comparison cannot say more than that. ",
        "The ISSP was not self-administered in a constant mode (the 1995 survey was by post, an interviewer ",
        "was present in 2003, most respondents used CASI within a personal interview in 2013, and the 2023 ",
        "survey was again by post), so mode and period are not separable within it across all waves. Only ",
        "the two postal waves compare without a mode change, and only for birth (Table A7c). Its question, scale ",
        "and sample also differ from the ALLBUS battery. Taken literally, the flat birth trend would make ",
        "the whole interview decline a matter of reporting, more than even the growing-gap scenario allows, ",
        "which points to differences between the series other than the interviewer. The intervals in Table A7 ",
        "reflect sampling error only. The comparison is therefore descriptive context for the direction of ",
        "the gap, not evidence of its size, and it does not locate the 1996 gap on Figure 5."), ""
)

# Table A7: ALLBUS and ISSP series --------------------------------------------------------

a7a <- tab("tableA7a_wedge_slopes") |>
  transmute(
    Criterion = blank_repeats(if_else(criterion == "born", "Born in Germany", "Ancestry / descent")),
    Term = c(
      "(Intercept)" = "Intercept (ALLBUS)",
      "decade" = "Decade (ALLBUS slope)",
      "sourceISSP (self-completion)" = "ISSP",
      "decade:sourceISSP (self-completion)" = "Decade × ISSP (difference in slope)"
    )[term],
    Estimate = ci(estimate, conf.low, conf.high, 2, 1)
  )

a7c <- tab("tableA7c_issp_born_shares") |>
  transmute(
    Year = as.character(year), Mode = mode,
    `Important (%)` = ci(important, important_low, important_upp),
    `Very important (%)` = ci(very, very_low, very_upp),
    N = formatC(n, format = "d", big.mark = ",")
  )

a7b <- tab("tableA7b_wedge_means") |>
  mutate(value = str_c(num(z, 2), " (", formatC(n, format = "d", big.mark = ","), ")")) |>
  select(source, criterion, year, value) |>
  arrange(year) |>
  pivot_wider(names_from = year, values_from = value, values_fill = "—") |>
  mutate(criterion = if_else(criterion == "born", "Born in Germany", "Ancestry / descent")) |>
  arrange(criterion, source) |>
  rename(Criterion = criterion, Series = source) |>
  mutate(Criterion = blank_repeats(Criterion))

table_a7 <- block(
  "Table A7. ALLBUS and ISSP series compared",
  panel("(a) Linear trends in standard deviations per decade", a7a),
  panel("(b) Standardised means by year (N in parentheses)", a7b),
  panel("(c) ISSP: importance of being born in Germany, share of German citizens", a7c),
  note = str_c("German citizens, weighted. Each item standardised within its series (pooled mean and SD ",
               "across waves). Weighted least squares; 95% confidence intervals in brackets, clustered on ",
               "ALLBUS sample points within wave and region. The ISSP files have no sample points, so ISSP ",
               "respondents are their own clusters and its intervals are too narrow if its samples were ",
               "clustered. ALLBUS: importance as a naturalisation criterion; 1996 PAPI, 2006–2023 CAPI ",
               "arm. ISSP: importance for being truly German; 1995 by post, 2003 ",
               "self-completion with an interviewer present, 2013 mostly CASI within CAPI, 2023 by post. ",
               "Ancestry not asked in the ISSP 1995.")
)

# Tables A8-A12: robustness ---------------------------------------------------------------

table_a8 <- block(
  "Table A8. Sequential compared with concurrent design",
  md_table(tab("tableA8_seq_vs_conc") |>
             left_join(tab("tableA8b_seq_conc_adjusted") |> select(item, p, p_holm, p_bh), by = "item") |>
             transmute(Criterion = item_label(item), `Sequential − concurrent` = ci(estimate, lower, upper),
                       p = num(p, 3), `Holm p` = num(p_holm, 3), `BH p` = num(p_bh, 3))),
  "",
  note = str_c(pop_note, "Difference in the share rating the criterion important (5–7), percentage points. ",
               ci_note, "p: two-sided, bootstrap standard error, normal approximation; Holm and ",
               "Benjamini–Hochberg adjustments over the nine criteria.")
)

table_a9 <- block(
  "Table A9. Straightlining and response variation by design arm",
  md_table(tab("tableA9_straightlining") |>
             mutate(value = if_else(outcome == "straightlining",
                                    ci(estimate, lower, upper, 1, 100), ci(estimate, lower, upper, 2, 1))) |>
             select(arm, outcome, value, n) |>
             pivot_wider(names_from = outcome, values_from = value) |>
             transmute(Arm = arm, `Same answer on all items (%)` = straightlining,
                       `Within-respondent SD` = within_sd, N = as.character(n))),
  "",
  note = str_c(pop_note, "Computed over the nine criteria. ", ci_note)
)

model_terms <- c(
  int_femaleTRUE = "Interviewer female", int_age10 = "Interviewer age (10 years)",
  int_tertiaryTRUE = "Interviewer tertiary education", int_tenure5 = "Interviewer tenure (5 years)",
  others_presentTRUE = "Others present", age10 = "Respondent age (10 years)",
  femaleTRUE = "Respondent female", educ3Intermediate = "Education intermediate (ref. low)",
  educ3High = "Education high (ref. low)", regionWest = "West (ref. East)"
)

a10b <- tab("tableA10b_interviewer_models") |>
  filter(term != "(Intercept)") |>
  mutate(value = ci(log_odds, lower, upper, 2, 1), term = model_terms[term]) |>
  select(term, item, value) |>
  pivot_wider(names_from = item, values_from = value) |>
  transmute(Term = term, `Born in Germany` = mn01, `German descent` = mn02)

table_a10 <- block(
  "Table A10. Interviewer variance and interviewer characteristics in the interview arm",
  "**(a) Intraclass correlations**", "",
  md_table(tab("tableA10_interviewer_icc") |>
             arrange(desc(year)) |>
             transmute(
               Year = blank_repeats(as.character(year)),
               Set = blank_repeats(str_replace(set, "Placebo: life domains", "Life domains")),
               Item = item_label(item), N = as.character(n),
               `Interviewers / points` = str_c(n_interviewers, " / ", n_points),
               `Interviewer ICC` = ci(icc, icc_lower, icc_upper, 3, 1),
               `Interviewer ICC, crossed` = ci(icc_interviewer_x, icc_interviewer_x_lower,
                                               icc_interviewer_x_upper, 3, 1),
               `Point ICC, crossed` = num(icc_point_x, 3)
             )),
  "",
  panel("(b) Logit model of rating the criterion important (5–7), 2023 interview arm", a10b),
  note = str_c("German citizens, CAPI arm (2023) and the 2016 interview. (a) Linear random-intercept models ",
               "(lme4). Crossed: interviewers and sample points as crossed random effects. Most interviewers ",
               "worked in a single sample point, so the interviewer-only ICC also contains area variance. ",
               "95% intervals from 200 parametric bootstrap draws. (b) Log-odds with 95% Wald intervals; ",
               "random intercepts for interviewers and sample points; unweighted. Interviewers were not ",
               "assigned at random to respondents and the presence of others is self-selected, so the ",
               "coefficients are associations.")
)

table_a11 <- block(
  "Table A11. Presence of others during the interview",
  md_table(tab("tableA11_third_party") |>
             transmute(
               Setting = blank_repeats(setting),
               Criterion = item_label(item),
               Indicator = if_else(indicator == "others_present", "Anyone present", "Partner present"),
               Difference = ci(estimate, lower, upper),
               `N present / alone` = str_c(n_present, " / ", n_alone)
             )),
  "",
  note = str_c(pop_note, "Difference in the share rating the criterion important (5–7) between respondents ",
               "with and without others present, percentage points. Recorded by the interviewer in CAPI, ",
               "self-reported in the self-administered arms. Not randomised. ", ci_note)
)

table_a12 <- block(
  "Table A12. Design effect by region, education and birth cohort",
  md_table(tab("tableA12_subgroups") |>
             bind_rows(tab("tableA12b_cohorts") |>
                         mutate(dimension = "Birth cohort", level = cohort,
                                n = replace_na(n, 0L))) |>
             mutate(value = ci(estimate, lower, upper)) |>
             select(dimension, level, item, value, n) |>
             mutate(n = max(n), .by = c(dimension, level)) |>
             pivot_wider(names_from = item, values_from = value) |>
             transmute(Dimension = blank_repeats(dimension), Group = level,
                       `Born in Germany` = mn01, `German descent` = mn02,
                       N = if_else(n == 0, "", as.character(n)))),
  "",
  note = str_c(pop_note, arm_note, "Both self-administered arms pooled; share rating the criterion ",
               "important (5–7). ", ci_note, "Birth cohorts: adult before the 2000 reform (born before ",
               "1965), in between, and socialised after it (born 1985 or later). The last row is the difference ",
               "between the youngest and the oldest cohort.")
)

# Cross-national comparisons: appendix text -----------------------------------------------

text_crossnational <- c(
  "## Cross-national comparisons", "",
  str_c("The 2023 experiment is randomised, but is limited to one country and one year. To understand the ",
        "broader implications of a mode effect of this kind, we draw on two non-randomised external sources, ",
        "using them for context rather than as definitive tests."), "",
  "### Countries that changed mode", "",
  str_c("Between 2013 and 2023, four countries in the ISSP National Identity module moved from interviewer- to ",
        "self-administered modes (Israel, Slovenia, Switzerland, and the United States). During the same ",
        "period, ten countries maintained their original mode. The share of respondents rating “birth in the ",
        "country” as important decreased by 4.2 percentage points in the switching countries, compared to ",
        "3.8 percentage points in the mode-stable countries. For “ancestry,” the figures were 3.1 and 4.5 ",
        "percentage points, respectively (Table A13, Figure A3). This comparison does not indicate a mode ",
        "effect. However, its explanatory power is limited: the four countries made independent decisions ",
        "to switch modes, the transitions coincided with fieldwork during and after the pandemic, and in the ",
        "United States, only about half of the 2023 interviews were self-administered."), "",
  "### Switch and revert in the ESS", "",
  str_c("The European Social Survey (ESS; ESS ERIC 2025a, 2025b, 2026a, 2026b) provides a more direct ",
        "comparison. Eight countries that conducted ",
        "face-to-face interviews in Round 9 (2018/19) switched to self-completion in Round 10 (2020–22) and ",
        "then reverted to face-to-face interviews in Round 11 (2023/24). A genuine change in attitudes should ",
        "not reverse when the mode reverts; a mode effect should."), "",
  str_c("Compared to nineteen countries that consistently used face-to-face interviews, self-completion ",
        "shifted answers in a more restrictive direction on the three items evaluating immigration: by 1.4 ",
        "points (95% CI: 0.2 to 2.6 points) on a 0–100 scale for the economy, 2.3 points (95% CI: 1.1 to 3.6 ",
        "points) for cultural life, and 2.5 points (95% CI: 1.3 to 3.7 points) for the country as a place to ",
        "live. On the three items concerning whom to admit, self-completion made no clear difference (−1.0 to ",
        "+0.8 points) (Table A14a)."), "",
  str_c("The country-specific effects varied considerably (Table A14b, Figure A4). In Germany, self-completion ",
        "shifted answers by 1.4 to 5.7 points in the restrictive direction, and these shifts reversed when ",
        "interviewer administration resumed. In Sweden, the shift was more pronounced, ranging from 10 to 13.5 ",
        "points. Conversely, Poland showed a shift in the opposite direction, by 3 to 13 points, primarily on ",
        "the admission items. Spain and Cyprus also showed slight shifts in the opposite direction."), "",
  str_c("The ESS items do not address criteria for belonging, so they cannot determine whether ascription, ",
        "specifically, is withheld from interviewers. Nonetheless, they demonstrate that self-completion ",
        "influences answers on immigration in many countries, that the magnitude and direction of this effect ",
        "are country-dependent, and that the observed shift in Germany aligns with the finding of the ALLBUS ",
        "experiment. While the German case is not unique in Europe, the specific size of its “descent effect” ",
        "cannot be extrapolated to other countries or other items."), ""
)

# Table A13: ISSP cross-national --------------------------------------------------------

table_a13 <- block(
  "Table A13. ISSP National Identity: change 2013–2023 by change in mode",
  md_table(tab("tableA13_issp_crossnational") |>
             mutate(switch = factor(switch, c("Interviewer -> self", "Mode-stable", "Other change"))) |>
             arrange(switch, d_born) |>
             transmute(
               Group = blank_repeats(str_replace(as.character(switch), "->", "→")),
               Country = str_remove(country_label, "^\\d+\\. [A-Z]{2}-"),
               `Mode 2013` = str_c(admin_2013, " (", num(share_2013, 0, 100), "%)"),
               `Mode 2023` = str_c(admin_2023, " (", num(share_2023, 0, 100), "%)"),
               `Born 2013` = num(born_2013, 1, 100), `Born 2023` = num(born_2023, 1, 100),
               `Δ born` = num(d_born, 1, 100),
               `Ancestry 2013` = num(ancestry_2013, 1, 100), `Ancestry 2023` = num(ancestry_2023, 1, 100),
               `Δ ancestry` = num(d_ancestry, 1, 100)
             )),
  "",
  note = str_c("ISSP 2013 (ZA5960) and 2023 (ZA10010), countries in both waves with a recorded mode. ",
               "Share rating the criterion very or fairly important for being truly [nationality], per cent; ",
               "changes in percentage points. Mode is the dominant administration (share of cases in ",
               "parentheses). Citizens only, national weights.")
)

# Table A14: ESS switch-and-revert ------------------------------------------------------

ess_items <- c(
  imsmetn = "Same race/ethnic group", imdfetn = "Different race/ethnic group",
  impcntr = "Poorer countries outside Europe", imbgeco = "Bad for the economy",
  imueclt = "Undermines cultural life", imwbcnt = "Worse place to live"
)

a14a <- tab("tableA14a_ess_mode_effects") |>
  transmute(Item = ess_items[item], `Self-completion effect` = ci(estimate, conf.low, conf.high),
            p = if_else(p.value < 0.001, "<0.001", num(p.value, 3)))

a14b <- tab("tableA14b_ess_country_effects") |>
  mutate(item = factor(item, names(ess_items))) |>
  arrange(item) |>
  transmute(cntry, item = ess_items[as.character(item)], value = num(effect, 1, 100)) |>
  pivot_wider(names_from = item, values_from = value) |>
  rename(Country = cntry)

a14c <- tab("tableA14c_ess_mode_pattern") |>
  mutate(across(r9:r11, \(x) coalesce(x, "not fielded"))) |>
  rename(Country = cntry, R9 = r9, R10 = r10, R11 = r11)

a14d <- tab("tableA14d_ess_means") |>
  mutate(item = factor(item, names(ess_items))) |>
  arrange(group, cntry, essround, item) |>
  transmute(group, cntry, round = str_c("R", essround, if_else(design == "Self-completion", " (SC)", "")),
            item = ess_items[as.character(item)], value = num(value, 1, 100)) |>
  pivot_wider(names_from = item, values_from = value) |>
  mutate(group = fct_rev(group)) |>
  arrange(group, cntry) |>
  mutate(Group = blank_repeats(as.character(group)), Country = blank_repeats(cntry)) |>
  select(Group, Country, Round = round, all_of(unname(ess_items)))

ess_note <- str_c("ESS rounds 9 (2018/19), 10 (2020–22) and 11 (2023/24). All items scored 0–100 in the ",
                  "restrictive direction: allow few or none (first three items) and the negative end of the ",
                  "0–10 evaluations. Weighted with anweight. ")

table_a14 <- block(
  "Table A14. ESS: switch to self-completion and back",
  panel("(a) Pooled effect of self-completion (two-way fixed effects)", a14a),
  panel("(b) Effect by switching country", a14b),
  panel("(c) Design by country and round", a14c),
  panel("(d) Means by country and round", a14d),
  note = str_c(ess_note, "(a) Country and round fixed effects, 8 switching and 19 face-to-face countries, ",
               "standard errors clustered on PSUs, 95% confidence intervals in brackets. (b) Each switcher's ",
               "R10 deviation from the mean of its R9 and R11, minus the average deviation of the face-to-face ",
               "countries; descriptive, no standard errors. Israel was self-completion in R10 but has no R9 ",
               "and is not in (a), (b) or (d).")
)

# Figures ------------------------------------------------------------------------------

figures <- c(
  figure("Figure A1. The series 1996–2023 with the scenario band, all nine criteria", "figA1_series_all_items",
         note = str_c(pop_note, "Share rating the criterion important (5–7) in the interview series, and the ",
                      "series corrected to self-administration under a constant and a growing mode gap. ",
                      "2023 points show both arms. ", ci_note)),
  figure("Figure A2. ALLBUS and ISSP series compared", "figA2_wedge",
         note = "Standardised means and linear trends (Table A7). The ISSP series is not mode-constant."),
  figure("Figure A3. ISSP National Identity: change 2013–2023 by change in mode", "figA3_issp_crossnational",
         note = "Change in the share rating the criterion very or fairly important (Table A13)."),
  figure("Figure A4. ESS: change under self-completion, by country", "figA4_ess_switch",
         note = str_c(ess_note, "Dots: each switching country's round 10 deviation from the mean of its ",
                      "rounds 9 and 11, minus the same deviation averaged over the 19 face-to-face ",
                      "countries (Table A14b; descriptive). Pooled: two-way fixed-effects estimate with ",
                      "95% confidence interval clustered on PSU (Table A14a). Positive values: more ",
                      "restrictive under self-completion."))
)

# Works cited in the appendix notes
references <- c(
  "## References", "",
  str_c("Asimov, Alexandra. 2026. \"Comparing Self-Administered Mixed-Mode and Face-to-Face Designs in a ",
        "Repeated Cross-Sectional Survey.\" *International Journal of Social Research Methodology* 29 (3): ",
        "381–98. https://doi.org/10.1080/13645579.2025.2545974."), "",
  str_c("Diehl, Claudia, and Ingrid Tucci. 2010. \"Ethnische Grenzziehungen in Ost- und Westdeutschland: ",
        "Konvergenz und Kulturalisierung.\" In *Leben in Ost- und Westdeutschland: Eine sozialwissenschaftliche ",
        "Bilanz der deutschen Einheit 1990–2010*, edited by Peter Krause and Ilona Ostner. Campus."), "",
  str_c("Diehl, Claudia, and Ingrid Tucci. 2011. \"Who Can Become German? Xenophobia and Attitudes towards ",
        "Naturalization.\" *DIW Economic Bulletin* 1 (3): 3–8."), "",
  str_c("European Social Survey European Research Infrastructure (ESS ERIC). 2025a. *ESS10 – Integrated File, Edition 3.3* [Data set]. ",
        "Sikt – Norwegian Agency for Shared Services in Education and Research. ",
        "https://doi.org/10.21338/ess10e03_3."), "",
  str_c("European Social Survey European Research Infrastructure (ESS ERIC). 2025b. *ESS10 Self-Completion – Integrated File, Edition 3.2* [Data set]. ",
        "Sikt – Norwegian Agency for Shared Services in Education and Research. ",
        "https://doi.org/10.21338/ess10sce03_2."), "",
  str_c("European Social Survey European Research Infrastructure (ESS ERIC). 2026a. *ESS11 – Integrated File, Edition 4.2* [Data set]. ",
        "Sikt – Norwegian Agency for Shared Services in Education and Research. ",
        "https://doi.org/10.21338/ess11e04_2."), "",
  str_c("European Social Survey European Research Infrastructure (ESS ERIC). 2026b. *ESS9 – Integrated File, Edition 3.3* [Data set]. ",
        "Sikt – Norwegian Agency for Shared Services in Education and Research. ",
        "https://doi.org/10.21338/ess9e03_3."), ""
)

# Assemble ---------------------------------------------------------------------------------

tables <- list(table_a1, table_a2, table_a3, table_a4, table_a5, table_a6, text_wedge, table_a7,
               table_a8, table_a9, table_a10, table_a11, table_a12, text_crossnational,
               table_a13, table_a14)

# Tables too wide for a portrait page get a landscape section of their own
landscape <- c("Table A1", "Table A3", "Table A6", "Table A10", "Table A13", "Table A14")

# A4 section break; the section it closes takes this orientation
section_break <- function(wide = FALSE) {
  size <- if (wide) '<w:pgSz w:w="16838" w:h="11906" w:orient="landscape"/>' else '<w:pgSz w:w="11906" w:h="16838"/>'
  c("```{=openxml}",
    str_c('<w:p><w:pPr><w:sectPr>', size,
          '<w:pgMar w:top="1134" w:right="1134" w:bottom="1134" w:left="1134" w:header="709" ',
          'w:footer="709" w:gutter="0"/></w:sectPr></w:pPr></w:p>'),
    "```", "")
}

# List of contents, built from the headings so it cannot drift from them
headings <- c(unlist(tables), figures) |>
  str_subset("^## ") |>
  str_remove("^## ")

contents <- c(
  "## Contents", "",
  "**Text**", "",
  str_c("- ", str_subset(headings, "^(Table|Figure)", negate = TRUE)), "",
  "**Tables**", "",
  str_c("- ", str_subset(headings, "^Table")), "",
  "**Figures**", "",
  str_c("- ", str_subset(headings, "^Figure")), "",
  section_break()
)

is_wide <- \(t) str_detect(t[1], str_c("^## (", str_flatten(landscape, "|"), ")\\."))

md <- c(
  "---",
  "title: \"Online appendix\"",
  "subtitle: \"Civic demands, hidden descent: survey mode and the measurement of naturalisation attitudes in Germany, 1996–2023\"",
  "author: \"Fabian Gülzau\"",
  "---", "",
  contents,
  unlist(map(tables, \(t) c(t, section_break(is_wide(t))))),
  figures,
  references
)

md_file <- fs::file_temp(ext = "md")
write_lines(md, md_file)

# Template: pandoc's default, body 11 pt, table cells ("Compact") 8.5 pt
ref_dir <- fs::path_temp("appendix_ref")
ref_docx <- fs::path_temp("appendix_ref.docx")
system2(rmarkdown::pandoc_exec(), c("-o", ref_docx, "--print-default-data-file", "reference.docx"))
unzip(ref_docx, exdir = ref_dir)
styles <- fs::path(ref_dir, "word", "styles.xml")
read_file(styles) |>
  str_replace('(<w:rPrDefault>.*?)<w:sz w:val="24" />\\s*<w:szCs w:val="24" />',
              '\\1<w:sz w:val="22" /><w:szCs w:val="22" />') |>
  str_replace('(w:styleId="Compact">.*?<w:spacing w:before="36" w:after="36" />\\s*</w:pPr>)',
              '\\1<w:rPr><w:sz w:val="17" /><w:szCs w:val="17" /></w:rPr>') |>
  write_file(styles)
# Final section (the figures): A4 portrait, as in the section breaks above
document <- fs::path(ref_dir, "word", "document.xml")
read_file(document) |>
  str_replace("</w:body>", str_c(str_remove_all(section_break()[2], "</?w:p>|</?w:pPr>"), "</w:body>")) |>
  write_file(document)
fs::file_delete(ref_docx)
withr::with_dir(ref_dir, zip(ref_docx, fs::dir_ls(recurse = TRUE, type = "file"), flags = "-q"))

rmarkdown::pandoc_convert(
  md_file, to = "docx", output = here(path_text, "appendix.docx"),
  options = c("--from=markdown+pipe_tables", str_c("--reference-doc=", ref_docx))
)

message("Written ", here(path_text, "appendix.docx"), ": ", sum(str_detect(headings, "^(Table|Figure)")), " tables and figures")
