# Process all sites sequentially
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2026-03-05

# Processes all sites with a ./sites/*/param.yaml file
# I.e. to exclude a site from processing, remove its param.yaml file

# Packages
library(yaml)

# Import parameters files for each site
p_list <- list.files("./sites", "param.yaml", recursive = TRUE, full.names = TRUE)

# Optionally filter p_list to subset of sites
# sites_sel <- c("Amacayacu", "Bicuar", "Misiones", "PanamaCanal")
# p_list <- p_list[grepl(paste(sites_sel, collapse = "|"), p_list)]

# Mock readline function to return "yes" when prompted
readline <- function(prompt = NULL) {
  return("y")
}

# For each site
for (i in p_list) { 
  p <- read_yaml(i)
  source("./zz_site.R")
}

# Restore normal readline behavior
rm(readline)
