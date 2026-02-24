# Run the data processing for all complete sites
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2026-02-11

# Import site status table
site_status <- read.csv("./dat/site_status.csv")

# List site data directories
site_dat_dirs <- list.files("./dat/sites/")

# Filter to completed sites
site_id_complete <- site_status$site_id[site_status$status == "complete"]

# Check all sites in status table have data directories
stopifnot(all(site_id_complete %in% site_dat_dirs))

# List site script directories
site_code_dirs <- list.files("./sites/")

# Check all sites in status table have code directories
stopifnot(all(site_id_complete %in% site_code_dirs))

# Create vector of quadrat dimensions
quad_dim_list <- list(c(50,50))
#   c(100, 100),
#   c(50, 50),
#   c(25, 25))

# For each site
for (i in site_id_complete) { 
  site_id <- i
  message(i)
  # For each set of quadrat dimensions
  for (j in quad_dim_list) { 
    quad_dim <- j
    message(paste(j, collapse = "-"))
    source("./zz_site.R")
  }
}
