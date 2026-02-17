# Correct taxonomic names
# John L. Godlee (johngodlee@gmail.com)  
# Last updated: 2026-02-11

# Packages
library(BIOMASS)

# Define site ID
# site_id <- "Panama_Canal"

# Define directories
# outdir <- "./dat/sites/Panama_Canal/03_taxa"

# Source functions
source("./func.R")

# Import stem data
# stem <- read.csv("./dat/sites/Panama_Canal/02_stem/stem.csv")

# Load previous cache
wfo_path <- file.path(outdir, "wfo_cache.rds")
if (file.exists(wfo_path)) { loadWFOCache(wfo_path) }

# Check names
taxa <- correctTaxo(
  genus = stem$taxon_name, 
  species = NULL,
  interactive = TRUE, 
  preferAccepted = TRUE,
  preferFuzzy = FALSE,
  sub_pattern = subPattern(),
  useCache = TRUE,
  useAPI = TRUE,
  capacity = 120,
  fill_time_s = 30, 
  timeout = 10)

# Extract only the taxonomy data from the stems
taxa_out <- cbind(record_id = stem$record_id, taxa)

# Write stem taxonomic information to file
write.csv(taxa_out, file.path(outdir, "stem_taxa.csv"), row.names = FALSE)

# Write WFO cache to file
saveRDS(BIOMASS:::the$wfo_cache, file.path(outdir, "wfo_cache.rds"))



