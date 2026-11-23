# Data preparation: ISSP National Identity "truly [NAT]" items, 1995-2023.
# ZA5960 = NI cumulation 1995/2003/2013; ZA10010 = ISSP 2023 NI & Citizenship.
# Items are recoded so that higher = more important (1-4), matching the ALLBUS
# direction. "Can't choose" (-8) is kept as a separate flag.

source(here::here("code", "helpers.R"))

# Item names shared across modules
truly_items <- c("born", "citizenship", "lived", "speak", "religion",
                 "institutions", "feel", "ancestry")

importance <- function(x) 5 - as_item(x)
cant_choose <- function(x) as.numeric(x) == -8

cumulation <- read_dta(
  raw("ZA5960_v1-0-0.dta"),
  col_select = c(YEAR_SDNO, COUNTRY, C_SAMPLE, CASEID, MODE, WEIGHT, v5:v12, v50)
) |>
  transmute(
    year = as.numeric(str_sub(as.character(as_factor(YEAR_SDNO)), 1, 4)),
    country = as.numeric(COUNTRY),
    c_sample = as.numeric(C_SAMPLE),
    country_label = as.character(as_factor(COUNTRY)),
    id = as.character(CASEID),
    mode_code = as_item(MODE),
    weight = as.numeric(WEIGHT),
    citizen = as_item(v50) == 1,
    born = v5, citizenship = v6, lived = v7, speak = v8, religion = v9,
    institutions = v10, feel = v11, ancestry = v12
  )

issp_2023 <- read_dta(
  raw("ZA10010_v1-0-0.dta"),
  col_select = c(country, c_sample, CASEID = matches("^CASEID$"), MODE, WEIGHT_COM, v1:v6, v60)
) |>
  transmute(
    year = 2023,
    country = as.numeric(country),
    c_sample = as.numeric(c_sample),
    country_label = as.character(as_factor(country)),
    id = as.character(row_number()),
    mode_code = as_item(MODE),
    weight = as.numeric(WEIGHT_COM),
    citizen = as_item(v60) == 1,
    born = v1, citizenship = v2, religion = v3, institutions = v4,
    feel = v5, ancestry = v6
  )

issp <- list(cumulation, issp_2023) |>
  map(\(d) mutate(d, across(where(is.labelled), zap_labels))) |>
  bind_rows() |>
  mutate(
    across(any_of(truly_items), cant_choose, .names = "{.col}_dk"),
    across(any_of(truly_items), importance),
    # Collapse the ISSP mode codes into the categories the paper compares
    mode = case_when(
      mode_code %in% 10:14 ~ "PAPI",
      mode_code %in% c(20:24, 52) ~ "CAPI",
      mode_code == 30 ~ "SC, interviewer present",
      mode_code %in% 31:34 ~ "SC, paper",
      mode_code == 40 ~ "CASI",
      mode_code == 50 ~ "CAWI",
      mode_code == 60 ~ "CATI",
      mode_code >= 70 | mode_code %in% c(35:39, 41:49, 51, 53:59) ~ "Mixed/other",
      .default = NA_character_
    ),
    germany = country == 276,
    region = case_when(c_sample == 27601 ~ "West", c_sample == 27602 ~ "East"),
    # The cumulation has no mode for 1995. Germany was self-completion by post
    # (ISSP Study Monitoring, ZA2880, Park and Jowell, findings chart p. 6)
    mode = if_else(germany & year == 1995, "SC, paper", mode)
  )

# Checks -----------------------------------------------------------------------

de_counts <- issp |> filter(germany) |> count(year)
stopifnot(
  setequal(de_counts$year, c(1995, 2003, 2013, 2023)),
  all(issp$born[!is.na(issp$born)] %in% 1:4)
)

write_rds(issp, clean("issp_ni.rds"))

message("issp_ni.rds: ", nrow(issp), " respondents, ",
        n_distinct(issp$country), " countries")
print(issp |> filter(germany) |> count(year, mode))
