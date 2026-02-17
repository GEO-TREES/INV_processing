# Clean Paracou plot polygons data
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2026-02-11

# Packages
library(dplyr)
library(tidyr)
library(sf)

# Source functions
source("../../func.R")

# Define site ID
site_id <- "Paracou"

# Define directories
indir <- "../../dat/sites/Paracou/raw"
outdir <- "../../dat/sites/Paracou/01_plot"

# Import column descriptions
poly_cols <- read.csv("../../templates/poly_cols.csv")
pt_cols <- read.csv("../../templates/pt_cols.csv")

# Import plot metadata
plot_meta <- read.csv(file.path(indir, "ParacouDescription.csv"))

# Extract plot corners
pt <- plot_meta %>% 
  pivot_longer(
    cols = starts_with("PlotL"),
    names_to = c(".value", "corner_id"),
    names_pattern = "Plot(Lat|Lon)(SW|SE|NE|NW)") %>% 
  dplyr::select(
    plot_id = Plot,
    corner_id,
    longitude = Lon,
    latitude = Lat, 
    PlotArea) %>% 
  filter(plot_id != "17(Arbocel)") %>% 
  distinct() %>% 
  st_as_sf(., coords = c("longitude", "latitude"), crs = 4326) %>% 
  st_transform(., crs = 32622) %>%  # UTM 22N
  mutate(
    site_id,
    x_rel_m = case_when(
      corner_id %in% c("SW", "NW") ~ 0,
      corner_id %in% c("SE", "NE") & PlotArea == 6.25 ~ 250,
      corner_id %in% c("SE", "NE") & PlotArea == 25 ~ 500,
      TRUE ~ NA_real_),
    y_rel_m = case_when(
      corner_id %in% c("SW", "SE") ~ 0,
      corner_id %in% c("NW", "NE") & PlotArea == 6.25 ~ 250,
      corner_id %in% c("NW", "NE") & PlotArea == 25 ~ 500,
      TRUE ~ NA_real_)) %>% 
  dplyr::select(all_of(pt_cols$column_name))


# Create polygons
poly <- pt %>% 
  group_by(site_id, plot_id) %>% 
  summarise() %>% 
  st_convex_hull() %>% 
  ungroup() %>% 
  mutate(
    min_diam_thresh_cm = 10,
    census_id_all = case_when(
      plot_id == "2" ~ "23;24;25;26",
      plot_id == "3" ~ "23;24;25;26",
      plot_id == "4" ~ "24;25;26;27",
      plot_id == "5" ~ "23;24;25;26",
      plot_id == "7" ~ "23;24;25;26",
      plot_id == "8" ~ "23;24;25;26",
      plot_id == "9" ~ "23;24;25;26",
      plot_id == "10" ~ "23;24;25;26",
      plot_id == "12" ~ "23;24;25;26",
      plot_id == "13" ~ "21;22;23;24;25;26;27;28",
      plot_id == "14" ~ "21;22;23;24;25;26;27;28",
      plot_id == "15" ~ "21;22;23;24;25;26;27;28",
      plot_id == "16" ~ "7",
      TRUE ~ NA_character_)) %>%
  filter(!plot_id %in% c("1", "11", "6")) %>% 
  dplyr::select(all_of(poly_cols$column_name))

# Check all columns in output objects
colCheck(poly, poly_cols)
colCheck(pt, pt_cols)

# Check values
polyValCheck(poly)
ptValCheck(pt)

# Write polygons to file
st_write(poly, file.path(outdir, "plot_poly.gpkg"), delete_dsn = TRUE)

# Write corner points to file
st_write(pt, file.path(outdir, "plot_pt.gpkg"), delete_dsn = TRUE)

