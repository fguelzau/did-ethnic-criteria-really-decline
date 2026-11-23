# Table 1: design, mode and Ns of the mn battery and the German ISSP NI items.
# Response rates are not in the data files; the text reports them from the method reports.

source(here::here("code", "helpers.R"))

allbus <- read_rds(clean("allbus_mn.rds"))
issp <- read_rds(clean("issp_ni.rds"))

allbus_rows <- allbus |>
  mutate(design = if_else(year == 2023, str_c(arm, " arm"), "Single mode")) |>
  summarise(
    n = sum(!is.na(mn01)),
    items = if_else(any(!is.na(mn09)), "mn01-mn09", "mn01-mn08"),
    modes = str_flatten(sort(unique(as.character(mode))), ", "),
    .by = c(year, design)
  ) |>
  mutate(source = "ALLBUS mn battery", .before = 1)

issp_rows <- issp |>
  filter(germany) |>
  summarise(
    n = sum(!is.na(born)),
    modes = str_flatten(sort(unique(replace_na(mode, "not recorded"))), ", "),
    .by = year
  ) |>
  mutate(source = "ISSP NI born/ancestry", items = "born, ancestry", design = "Single survey", .before = 1)

table1 <- bind_rows(allbus_rows, issp_rows) |>
  arrange(source, year, design) |>
  select(source, year, design, items, modes, n)

save_tab(table1, "table1_design")

table1 |>
  gt::gt(groupname_col = "source") |>
  gt::cols_label(year = "Year", design = "Design", items = "Items", modes = "Realised mode(s)", n = "N") |>
  gt::fmt_integer(n) |>
  gt::tab_source_note("German citizens; ALLBUS Ns are respondents with a valid mn01.") |>
  gt::gtsave(here(path_tab, "table1_design.html"))

print(table1)
