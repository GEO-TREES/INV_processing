# Clean Bicuar stem data
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2025-07-09

# Packages
library(dplyr)

# Define site ID
BRM_site <- "Bicuar"

# Define directories
indir <- "../../dat/sites/Bicuar/raw"
outdir <- "../../dat/sites/Bicuar/02_stem_fmt"

# Import stem column descriptions
stems_cols <- read.csv("../../dat/templates/stem_fmt_cols.csv")

# Import data
s <- read.csv(file.path(indir, "stems.csv"))
p <- read.csv(file.path(indir, "plots.csv"))


# Prepare stem data 
s_clean <- s %>% 
  left_join(., unique(p[,c("plot_id", "plot_name")]), by = "plot_id") %>% 
  filter(grepl("2024", census_date)) %>% 
  rename(
    Plot_name = plot_name) %>% 
  mutate(
    BRM_site = BRM_site,
    census_id = paste(BRM_site, Plot_name, 2024, sep = "_"),
    measurement_id = paste(census_id, row_number(), sep = "_"),
    alive = ifelse(stem_status %in% c("a", "r"), 1, 0),
    broken = ifelse(grepl("b|p", stem_mode), 1, 0),
    fallen = ifelse(grepl("f", stem_mode), 1, 0),
    missing = ifelse(grepl("v|q", stem_mode), 1, 0),
    liana = ifelse(grepl("w", stem_mode), 1, 0)) %>% 
  dplyr::select(
    BRM_site,
    Plot_name,
    census_date,
    census_id,
    measurement_id,
    x_rel = x_grid,
    y_rel = y_grid,
    diam,
    pom,
    height, 
    taxon_name_orig = species_name_clean,
    alive,
    broken,
    fallen,
    missing,
    liana)

# Check all columns in stems table
stopifnot(all(colnames(s_clean) == stems_cols$column_name))

# Write data to file
write.csv(s_clean, file.path(outdir, "stems.csv"), row.names = FALSE)
