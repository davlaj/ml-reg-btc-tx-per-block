# setup-packages.R
if (!require("pacman")) install.packages("pacman")

pacman::p_load(httr, 
               jsonlite, 
               targets, 
               dplyr, 
               ggplot2, 
               tidyr, 
               lubridate, 
               purrr, 
               zoo, 
               caret, 
               timetk, 
               modeltime, 
               gridExtra,
               patchwork,
               corrplot,
               GGally,
               forecast,
               reshape2,
               grid,
               ggbiplot)