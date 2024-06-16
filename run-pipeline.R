if (!require("pacman")) install.packages("pacman")
library(targets)

# Clear all targets and their metadata in case the pipeline fails to run and generate the outputs properly 
# tar_destroy()

# Run the targets pipeline
tar_make()
