# Runs the whole pipeline in order. From the project root:
#   Rscript code/99_run_all.R

scripts <- c(
  "01_prepare_allbus.R",
  "02_prepare_issp.R",
  "10_design_facts.R",
  "11_table1_design.R",
  "21_design_effects.R",
  "22_item_gradient.R",
  "23_typology.R",
  "24_bridge.R",
  "25_allbus_vs_issp.R",
  "31_universe.R",
  "32_selection_checks.R",
  "33_seq_conc_subgroups.R",
  "34_scale_use_checks.R",
  "35_interviewer_icc.R",
  "36_others_present.R",
  "41_issp_crossnational.R",
  "42_ess.R",
  "50_appendix.R"
)

for (script in scripts) {
  message("\n== ", script)
  source(here::here("code", script), local = new.env())
}

# Record the R and package versions of this run
writeLines(capture.output(sessionInfo()), here::here("session_info.txt"))
