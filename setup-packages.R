# setup-packages.R
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  targets, dplyr, ggplot2, tidyr, lubridate, purrr, zoo, caret, timetk, modeltime, gridExtra,
  quietly = TRUE  # Optionally load packages quietly
)