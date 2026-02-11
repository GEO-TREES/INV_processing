# Run the data processing for all complete sites
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2026-02-11

# Import site status table
site_status <- read.csv("./dat/site_status.csv")

# List site data directories
site_dat_dirs <- list.files("./dat/sites/")

# Check all sites in status table have data directories
setequal(site_status$site_id, site_dat_dirs)

# List site script directories
site_code_dirs <- list.files("./sites/")

# Check all sites in status table have code directories
setequal(site_status$site_id, site_code_dirs)

# Filter to completed sites
site_id_complete <- site_status$site_id[site_status$status == "complete"]

# Process each site, 
for (i in site_id_complete) { 
  site_id <- i
  source("./zz_site.R")
}
