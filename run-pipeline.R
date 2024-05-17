if (!require("pacman")) install.packages("pacman")
library(targets)

# Run the targets pipeline
tar_make()
