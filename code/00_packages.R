# Installs the packages the pipeline needs. Run once per machine.

pkgs <- c(
  "tidyverse", "haven", "here", "fs", "glue", "scales", "srvyr", "survey",
  "lme4", "broom", "gt", "ordinal", "withr", "rmarkdown"
)

missing <- setdiff(pkgs, rownames(installed.packages()))
if (length(missing) > 0) install.packages(missing)
