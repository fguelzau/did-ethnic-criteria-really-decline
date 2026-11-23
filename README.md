# Civic demands, hidden descent: survey mode and the measurement of naturalisation attitudes in Germany, 1996–2023

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.24464195.svg)](https://doi.org/10.5281/zenodo.24464195)

Replication code for the article by Fabian Gülzau.
Preprint: SocArXiv, <https://doi.org/10.31235/osf.io/4ax5h>. Code archived at Zenodo: <https://doi.org/10.5281/zenodo.24464195>.

The shift from descent-based to civic naturalisation criteria is one of the best-established
findings in German integration research. It rests on one ALLBUS battery (`mn01`–`mn09`), which was
fielded face-to-face for its entire history. In 2023 ALLBUS randomly assigned respondents to an
interviewer-administered design or a self-administered design. The design effect is concentrated
on the two descent items (born in Germany, German descent). Part of the documented decline in
ethnic criteria therefore reflects what respondents are willing to report to an interviewer.
The scripts here rebuild every figure and table in the article and the online appendix from the
original survey files.

The repository name keeps the paper's working title (*Did ethnic criteria really decline?*).

## Data

The survey data are not included because the GESIS licence terms do not allow redistribution.
Download the files below (free registration at [GESIS](https://search.gesis.org) and the
[ESS data portal](https://ess.sikt.no)) and put them in `data/raw/`:

| Data set | File | Version | DOI |
|---|---|---|---|
| ALLBUS/GGSS Cumulation 1980–2023 (full version) | `ZA8839_v1-0-0.dta` | 1.0.0 | [10.4232/1.14695](https://doi.org/10.4232/1.14695) |
| ALLBUScompact 2023 | `ZA8833_v1-0-0.dta` | 1.0.0 | [10.4232/1.14572](https://doi.org/10.4232/1.14572) |
| ISSP National Identity I–III (1995, 2003, 2013) | `ZA5960_v1-0-0.dta` | 1.0.0 | [10.4232/1.13471](https://doi.org/10.4232/1.13471) |
| ISSP 2023 National Identity and Citizenship | `ZA10010_v1-0-0.dta` | 1.0.0 | [10.4232/5.ZA10010.1.0.0](https://doi.org/10.4232/5.ZA10010.1.0.0) |
| ESS Rounds 9, 10, 10 self-completion, 11 | one combined data-portal export, `ESS*.dta` | 9e03_3, 10e03_3, 10SCe03_2, 11e04_2 | [ESS9](https://doi.org/10.21338/ess9e03_3), [ESS10](https://doi.org/10.21338/ess10e03_3), [ESS10-SC](https://doi.org/10.21338/ess10sce03_2), [ESS11](https://doi.org/10.21338/ess11e04_2) |

The ALLBUS cumulation must be the full version (ZA8839). The compact cumulation (ZA8837) does not
release the 2023 sample points. The ESS export needs `name`, `cntry`, `essround`, `idno`, the weights
and the immigration items used in `code/42_ess.R`. Any file in `data/raw/` whose name starts with `ESS`
and ends in `.dta` is read.

## Software

- R (developed with 4.6.2). Install the packages with `Rscript code/00_packages.R`. `session_info.txt` records the R and
  package versions of the last full run (written by `code/99_run_all.R`).
- [pandoc](https://pandoc.org) for the appendix document (`code/50_appendix.R`). RStudio ships with pandoc.

## Running

From the project root (the folder containing the `.Rproj` file):

```sh
Rscript code/00_packages.R   # once
Rscript code/99_run_all.R    # about ten minutes
```

`99_run_all.R` runs the scripts in order. It writes the cleaned data to `data/clean/` and the results
to `output/figures/`, `output/tables/` and `output/text/` (including `appendix.docx`, the online
appendix). Once `01_` and `02_` have built `data/clean/`, any script can be run on its own.

The pipeline checks itself against published figures and stops when one is not reproduced:

- the mode-effect (AAMB) values for `mn01`/`mn02` in Asimov (2026), Table A1;
- the 1996 and 2006 "mainly ethnic" shares in Diehl and Tucci (2011), which our typology rule matches within 2.5 points (their exact coding is not documented; Table A4e compares alternative codings);
- the 2023 design-arm sample sizes.

Bootstrap seeds are fixed, so repeated runs give identical numbers.

## Scripts

Scripts are numbered in the order of the manuscript: `0x` data preparation, `1x` data and design,
`2x` results, `3x` robustness (online appendix), `4x` cross-national comparisons (online appendix).
`helpers.R` holds paths, constants and the bootstrap helpers and is sourced by every script.

| Script | Content | Outputs |
|---|---|---|
| `01_prepare_allbus.R`, `02_prepare_issp.R` | Analysis files (German citizens; 2023 design arm joined by `respid`) | `data/clean/` |
| `10_design_facts.R` | Waves and modes, the 2023 instrument, sample points and interviewers, ISSP series | `output/text/design_facts.md` |
| `11_table1_design.R` | Design, modes and Ns by wave | Table 1 |
| `21_design_effects.R` | Design effects by item and arm | Fig. 1, Table A2 |
| `22_item_gradient.R` | Design effects on items outside the battery | Fig. 2, Table A3 |
| `23_typology.R` | Typology (after Diehl and Tucci) by wave and arm; alternative codings of the typology | Fig. 3, Table A4e |
| `24_bridge.R` | Series 1996–2023, scenario band, reportability share | Figs 4, 5, A1; Tables A5, A6 |
| `25_allbus_vs_issp.R` | ALLBUS and ISSP descent series compared | Table A7, Fig. A2 |
| `31_universe.R` | Analysis universe | Table A1a–c |
| `32_selection_checks.R` | Arm balance, raking, interviewer-cluster bootstrap, cohorts, correlations | Tables A1d–e, A2f, A12b |
| `33_seq_conc_subgroups.R` | Sequential vs concurrent arm, straightlining, subgroups | Tables A8, A9, A12 |
| `34_scale_use_checks.R` | Typology and scale use (also with descent alone), cut points, cumulative logit, specificity, acquiescence | Tables A2d–e, A3a–c, A4, A8b |
| `35_interviewer_icc.R` | Interviewer variance | Table A10 |
| `36_others_present.R` | Others present at the interview | Table A11 |
| `41_issp_crossnational.R` | ISSP countries that changed mode | Table A13, Fig. A3 |
| `42_ess.R` | ESS switch-and-revert | Table A14, Fig. A4 |
| `50_appendix.R` | Assembles the online appendix | `output/text/appendix.docx` |
| `99_run_all.R` | Runs everything in order | |

`34_scale_use_checks.R` reads Tables A2, A3 and A8, so it runs after `21_`, `22_` and `33_`.

Output files carry the number they have in the article (`fig1_`–`fig5_`, `table1_`, `tableA1a_`–`tableA14d_`,
`figA1_`–`figA4_`). Files without a number (`aamb`, `fig1b_share_important`, `series_1996_2023`,
`reportability_sensitivity`, `typology_*`) are reproduction checks or the data behind a figure.

## How to cite

Gülzau, Fabian. 2026. "Civic demands, hidden descent: survey mode and the measurement of
naturalisation attitudes in Germany, 1996–2023." SocArXiv. <https://doi.org/10.31235/osf.io/4ax5h>.

The code: Gülzau, Fabian. 2026. "Replication code for: Civic demands, hidden descent: survey mode
and the measurement of naturalisation attitudes in Germany, 1996–2023." Zenodo.
<https://doi.org/10.5281/zenodo.24464195>.

## License

Code: MIT (see `LICENSE`). The data are subject to the GESIS and ESS terms of use.
