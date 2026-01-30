# Correct taxonomic names
# John L. Godlee (johngodlee@gmail.com)  
# Last updated: 2025-07-09

# Packages
library(dplyr)

# Define site ID
site_id <- "Panama Canal"

# Define directories
indir <- "../../dat/sites/Panama Canal/02_stem"
outdir <- "../../dat/sites/Panama Canal/03_taxa"

# Source functions
source("../../func.R")

# Import stem data
s <- read.csv(file.path(indir, "stem.csv"))

# Import WFO data
WorldFlora::WFO.remember("../../dat/wfo_raw/classification.csv")

# Check names
taxonCheck(s$taxon_name_orig, WFO.data = WFO.data, ret_unk = TRUE)

# Add corrections to master lookup table
lookup <- tibble::tribble(
~"original", ~"corrected",
"Indet indet", "Indet indet",
"Sapium broadleaf", "Sapium indet",
"Coussarea curvigemmia", "Coussarea suaveolens",
"Swartzia simplex_var.ochnacea", "Swartzia simplex var. ochnacea",
"Rinorea sylvatica", "Rinorea indet",
"Swartzia simplex_var.grandiflora", "Swartzia simplex var. grandiflora",
"Appunia seibertii", "Appunia siebertii",
"Unidentified species", "Indet indet",
"Nectandra sp.4_(tiny_leaf)", "Nectandra indet",
"Trema micrantha", "Trema micranthum",
"Ardisia bartlettii", "Ardisia bartletii",
"Nectandra fuzzy", "Nectandra indet",
"Morinda seibertii", "Appunia siebertii",
"Guarea sherman", "Guarea indet",
"Nectandra metrop", "Nectandra indet",
"Unidentified arecaceae-soberania", "Arecaceae",
"Unidentified myrtaceae hoja mediana-fincaroubik", "Myrtaceae",
"Unidentified myrtaceae hoja sesil-fincaroubik", "Myrtaceae",
"Unidentified myrtaceae hoja pequena-fincaroubik", "Myrtaceae",
"Unidentified finca_roubik", "Indet indet",
"Protium aff.guianense", "Protium guianense",
"Unidentified rubiaceae-fincaroubik", "Rubiaceae",
"Licania kallunkii", "Licania kallunkiae",
"Posoqueria sp.1_(hojas_chicas)", "Posoqueria indet",
"Neea roja", "Neea indet",
"Meliosma hoja_grande", "Meliosma indet")

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
write.csv(taxa_out, file.path(outdir, "stem_taxa.csv"), row.names = FALSE)

