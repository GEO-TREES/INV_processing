# Process all sites sequentially
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2026-03-05

# Packages
library(yaml)

# Import parameters files for each site
p_list <- list.files("./sites", "param.yaml", recursive = TRUE, full.names = TRUE)

# Mock readline function to return "yes" when prompted
readline <- function() {
  return("y")
}

# For each site
for (i in p_list) { 
  p <- read_yaml(i)
  source("./zz_site.R")
}

# Restore normal readline behavior
rm(readline)
