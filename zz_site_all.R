# Process multiple acquisitions from potentially multiple sites
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2026-10-14

# Processes all acquisitions with a ./sites/<SITE>/<ACQUISITION>/<VERSION>/param.yaml file

# Packages
library(yaml)

# Import parameters files for each site
p_list <- list.files("./sites", "^param\\.yaml$", recursive = TRUE, full.names = TRUE)

# Optionally filter p_list to subset of sites
# Match the <SITE> directory exactly, so e.g. "Paracou" doesn't match "Paracou2"
sites_sel <- c("Bicuar", "Amacayacu")
p_site <- basename(dirname(dirname(dirname(p_list))))
p_list <- p_list[p_site %in% sites_sel]

# Mock readline function to return "yes" when prompted to delete previous outputs
readline <- function(prompt = NULL) {
  return("y")
}

# Run in batch mode: don't prompt for ambiguous taxonomic names
options(inv.batch = TRUE)

# For each site
for (i in p_list) {
  p_file <- i
  source("./zz_site.R")
}

# Restore normal behaviour
rm(readline)
options(inv.batch = NULL)
