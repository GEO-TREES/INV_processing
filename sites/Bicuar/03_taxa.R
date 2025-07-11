# Correct taxonomic names
# John L. Godlee (johngodlee@gmail.com)  
# Last updated: 2025-07-09

# Define site ID
site_id <- "Bicuar"

# Define directories
indir <- "../../dat/sites/Bicuar/02_stem_fmt"
outdir <- "../../dat/sites/Bicuar/03_taxa"

# Source functions
source("../../func.R")

# Import stem data
s <- read.csv(file.path(indir, "stems.csv"))

# Import WFO data
WorldFlora::WFO.remember("../../dat/wfo_raw/classification.csv")

# Check names
taxonCheck(s$taxon_name_orig, WFO.data = WFO.data, ret_unk = TRUE)

# Add corrections to master lookup table
lookup <- tibble::tribble(
~"original", ~"corrected",
"Indet indet", "Indet indet",
"Securidaca longipedunculata", "Securidaca longepedunculata")

# Run taxonomy checking with lookup table 
taxa <- taxonCheck(s$taxon_name_orig, lookup = lookup, 
  WFO.data = WFO.data, ret_unk = TRUE)

# Add taxonomic names back to original data
s_taxa <- merge(s, taxa, by = "taxon_name_orig", all.x = TRUE, sort = FALSE) 

# Check validity of output
stopifnot(all(!is.na(s_taxa$taxon_name_acc[!is.na(s_taxa$taxon_name_sanit)])))

# Add site name to lookup
lookup_out <- cbind(site_id = site_id, lookup)

# Write appended lookup table to file
write.csv(lookup, file.path(outdir, "lookup.csv"), row.names = FALSE)

# Extract only the taxonomy data from the stems
taxa_out <- s_taxa %>% 
  dplyr::select(
    measurement_id,
    starts_with("taxon_"),
    -ends_with("_orig"))

# Write stem taxonomic information to file
write.csv(taxa_out, file.path(outdir, "taxa.csv"), row.names = FALSE)

