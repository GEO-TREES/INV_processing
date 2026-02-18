# Format Wanang tree inventory data
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2026-02-18

# Packages
library(dplyr)
library(sf)

# Source functions
source("../../func.R")

# Define site ID
site_id <- "Wanang"

# Define directories
indir <- "../../dat/sites/Wanang/raw"
outdir <- "../../dat/sites/Wanang/01_fmt"

# Import column descriptions
poly_cols <- read.csv("../../templates/poly_cols.csv")
pt_cols <- read.csv("../../templates/pt_cols.csv")
census_cols <- read.csv("../../templates/census_cols.csv")
stem_cols <- read.csv("../../templates/stem_cols.csv")

# Import data
pt <- read.csv(file.path(indir, "plot_corners.csv"))
s <- read.csv(file.path(indir, "s.csv"))

# Format stem data
s_clean <- s %>%
  rename(
    subplot_id = QuadratName,
    x_rel_m = QX_plot,
    y_rel_m = QY_plot,
    stem_id = StemID,
    diam_cm = DBH,
    pom_m = HOM, 
    measurement_date = EaxctDate) %>%
  mutate(
         plot_id = as.character(plot_id),
         subplot_id = as.character(subplot_id),
         stem_id = as.character(stem_id),
    record_id = row_number(),
    tree_id = NA_character_,
    height_m = NA_real_,
    site_id,
    alive = ifelse(status %in% c("alive", "alivealive below"), TRUE, FALSE),
    missing = ifelse(status %in% c("missing"), TRUE, FALSE),
    broken = FALSE,
    fallen = FALSE, 
    agb_allometry = NA_character_,
    measurement_date = ifelse(is.na(measurement_date), first(na.omit(measurement_date)), measurement_date),
    taxon_name = paste(Genus, SpeciesName)) %>% 
  group_by(plot_id, census_id, stem_id) %>% 
  mutate(measurement_id = row_number()) %>% 
  ungroup() %>% 
  dplyr::select(all_of(stem_cols$column_name))

# Format plot corners
pt_clean <- pt %>% 
  mutate(plot_id = as.character(plot_id)) %>%
  st_as_sf(., coords = c("X", "Y"), crs = 32755) %>% 
  mutate(site_id) %>% 
  mutate(
         x_rel_m = as.numeric(x_rel_m),
         y_rel_m = as.numeric(y_rel_m)) %>% 
  dplyr::select(all_of(pt_cols$column_name))

# Create plot polygons
poly <- pt_clean %>% 
  group_by(site_id, plot_id) %>% 
  summarise() %>% 
  st_convex_hull() %>% 
  ungroup() %>% 
  dplyr::select(all_of(poly_cols$column_name))

# Create census table
census <- s_clean %>% 
  group_by(site_id, plot_id, census_id) %>% 
  summarise(census_date = as.character(mean(as.Date(measurement_date), na.rm = TRUE))) %>% 
  mutate(min_diam_thresh_cm = 10) %>% 
  dplyr::select(all_of(census_cols$column_name))

# Check all columns in output objects
colCheck(poly, poly_cols)
colCheck(pt_clean, pt_cols)
colCheck(census, census_cols)
colCheck(s_clean, stem_cols)

# Check values
polyValCheck(poly)
ptValCheck(pt_clean)
censusValCheck(census)
stemValCheck(s_clean)

# Write polygons to file
st_write(poly, file.path(outdir, "plot_poly.gpkg"), delete_dsn = TRUE)

# Write corner points to file
st_write(pt_clean, file.path(outdir, "plot_pt.gpkg"), delete_dsn = TRUE)

# Write census meta-data to file
write.csv(census, file.path(outdir, "census.csv"), row.names = FALSE)

# Write stem data to file
write.csv(s_clean, file.path(outdir, "stem.csv"), row.names = FALSE)
    
