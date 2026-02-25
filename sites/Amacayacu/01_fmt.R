# Process Amacayacu plot data
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2026-02-25

# Packages
library(dplyr)
library(tidyr)
library(sf)
library(readxl)

# Source functions
source("../../func.R")

# Define site ID
site_id <- "Amacayacu"

# Define directories
indir <- "../../dat/sites/Amacayacu/raw"
outdir <- "../../dat/sites/Amacayacu/01_fmt"

# Import column descriptions
plot_cols <- read.csv("../../templates/plot_cols.csv")
pt_cols <- read.csv("../../templates/pt_cols.csv")
census_cols <- read.csv("../../templates/census_cols.csv")
stem_cols <- read.csv("../../templates/stem_cols.csv")

# Import data
s <- read.csv(file.path(indir, "amafull_2026-02-24.csv"))
q <- read.csv(file.path(indir, "quadrat.20260224.csv"))
sp <- read.csv(file.path(indir, "species.20260224.csv"))
p <- read_excel(file.path(indir, "plot_pi.xlsx"))
pc <- read.csv(file.path(indir, "plot_corners.csv"))

# Process plot corners
pt <- pc %>% 
  rename(
    corner_id = corner) %>% 
  mutate(
    site_id,
    plot_id = "Amacayacu_1",
    x_rel_m = case_when(
      corner_id %in% c("SW", "NW") ~ 0,
      corner_id %in% c("SE", "NE") ~ 500,
      TRUE ~ NA_real_),
    y_rel_m = case_when(
      corner_id %in% c("SW", "SE") ~ 0,
      corner_id %in% c("NW", "NE") ~ 500,
      TRUE ~ NA_real_)) %>% 
  st_as_sf(., coords = c("longitude", "latitude"), crs = 4326) %>% 
  dplyr::select(all_of(pt_cols$column_name))

# Process stem data
s_clean <- s %>% 
  pivot_longer(
    cols = -c(stemtag, stemtag_field, tag, spcode, gx, gy, quadrat), 
    names_to = c(".value", "time_period"),
    names_pattern = "([A-Za-z]+)(\\d+)") %>% 
  left_join(., sp, by = "spcode") %>% 
  rename(
    tree_id = tag,
    stem_id = stemtag,
    taxon_name = species,
    x_rel_m = gx,
    y_rel_m = gy,
    subplot_id = quadrat, 
    census_id = time_period,
    pom_m = hom,
    measurement_date = date) %>% 
  filter(!is.na(status)) %>% 
  mutate(
    site_id, 
    census_id = as.integer(census_id),
    plot_id = "Amacayacu_1",
    diam_cm = dbh / 10,
    height_m = NA_real_,
    alive = ifelse(status %in% c("alive", "P"), TRUE, FALSE),
    fallen = ifelse(grepl("L", codes), TRUE, FALSE),
    broken = ifelse(grepl("Q", codes), TRUE, FALSE),
    missing = ifelse(grepl("DD", codes), TRUE, FALSE),
    agb_allometry = NA_character_) %>% 
  group_by(plot_id, census_id, stem_id) %>% 
  mutate(measurement_id = row_number()) %>% 
  ungroup() %>% 
  mutate(record_id = row_number()) %>% 
  dplyr::select(all_of(stem_cols$column_name))

# Create census table
census <- s_clean %>% 
  group_by(site_id, plot_id, census_id) %>% 
  summarise(census_date = as.character(mean(as.Date(measurement_date, "%d/%m/%y"), na.rm = TRUE))) %>% 
  ungroup() %>% 
  mutate(census_id = as.integer(census_id)) %>% 
  dplyr::select(all_of(census_cols$column_name))
  
# Create plot metadata table
p_clean <- data.frame(
  site_id,
  plot_id = "Amacayacu_1",
  census_date_geotrees = as.character(census$census_date[census$census_id == 4]),
  census_date_all = paste(census$census_date, collapse = ";"),
  plot_width_m = 500,
  plot_length_m = 500,
  plot_slope_deg = NA_real_,
  plot_aspect_deg = NA_real_,
  plot_elevation_m = NA_real_,
  plot_planar = TRUE,
  notes_plot = NA_character_,
  meas_diam_min_cm = 10,
  meas_pom_default_m = 1.3,
  meas_tree_stem = TRUE,
  meas_tree_group = TRUE,
  meas_dead = TRUE,
  meas_fallen = TRUE,
  meas_liana = NA,
  meas_palm = NA,
  meas_bamboo = NA,
  meas_protocol = NA_character_,
  notes_meas = NA_character_,
  forest_status = "old-growth",
  land_use = NA_character_,
  treatment = NA_character_,
  treatment_ref = NA_character_,
  fire_regime = NA_character_,
  cyclone_regime = NA_character_,
  flood_regime = NA_character_,
  earth_regime = NA_character_,
  herbivory_regime = NA_character_,
  notes_disturbance = NA_character_) %>% 
dplyr::select(all_of(plot_cols$column_name))

# Check all columns in output objects
colCheck(p_clean, plot_cols)
colCheck(pt, pt_cols)
colCheck(census, census_cols)
colCheck(s_clean, stem_cols)

# Check values
# plotValCheck(plots)
ptValCheck(pt)
censusValCheck(census)
stemValCheck(s_clean)

# Write corner points to file
st_write(pt, file.path(outdir, "plot_pt.gpkg"), delete_dsn = TRUE)

# Write census meta-data to file
write.csv(census, file.path(outdir, "census.csv"), row.names = FALSE)

# Write plot meta-data to file
write.csv(p_clean, file.path(outdir, "plot.csv"), row.names = FALSE)

# Write data to file
write.csv(s_clean, file.path(outdir, "stem.csv"), row.names = FALSE)
