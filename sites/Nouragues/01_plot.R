# Clean Nouragues plot polygons data
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2025-07-09

# Packages
library(dplyr)
library(tidyr)
library(sf)

# Source functions
source("../../func.R")

# Define site ID
site_id <- "Nouragues"

# Define directories
indir <- "../../dat/sites/Nouragues/raw"
outdir <- "../../dat/sites/Nouragues/01_plot"

# Import column descriptions
poly_cols <- read.csv("../../templates/poly_cols.csv")
pt_cols <- read.csv("../../templates/pt_cols.csv")

# Import plot corners
plot_meta <- read.csv(file.path(indir, "NouraguesDescription.csv"))

# Extract plot corners
plot_corners <- plot_meta %>% 
  pivot_longer(
    cols = starts_with("SubPlotL"),
    names_to = c(".value", "corner_id"),
    names_pattern = "SubPlot(Lat|Lon)(SW|SE|NE|NW)") %>% 
  dplyr::select(
    id,
    plot_id = Plot,
    corner_id,
    longitude = Lon,
    latitude = Lat) %>% 
  filter(
    (plot_id == "Balenfois" & id == 1 & corner_id == "SW") | 
    (plot_id == "Balenfois" & id == 1 & corner_id == "SE") | 
    (plot_id == "Balenfois" & id == 2 & corner_id == "NW") | 
    (plot_id == "Balenfois" & id == 2 & corner_id == "NE") | 
    (plot_id == "Grand_Plateau" & id == 12 & corner_id == "SW") | 
    (plot_id == "Grand_Plateau" & id == 12 & corner_id == "SE") | 
    (plot_id == "Grand_Plateau" & id == 3 & corner_id == "NW") | 
    (plot_id == "Grand_Plateau" & id == 3 & corner_id == "NE") | 
    (plot_id == "Parare-Arataye" & id == 14 & corner_id == "SW") | 
    (plot_id == "Parare-Arataye" & id == 13 & corner_id == "SE") | 
    (plot_id == "Parare-Arataye" & id == 18 & corner_id == "NW") | 
    (plot_id == "Parare-Arataye" & id == 17 & corner_id == "NE") | 
    (plot_id == "Petit_Plateau" & id == 27 & corner_id == "SW") | 
    (plot_id == "Petit_Plateau" & id == 30 & corner_id == "SE") | 
    (plot_id == "Petit_Plateau" & id == 19 & corner_id == "NW") | 
    (plot_id == "Petit_Plateau" & id == 22 & corner_id == "NE"))

# Create plot corner sf 
pt <- plot_corners %>% 
  st_as_sf(., coords = c("longitude", "latitude"), crs = 4326) %>% 
  st_transform(., crs = 32622) %>%  # UTM 22N
  mutate(
    site_id,
    x_rel_m = case_when(
      plot_id == "Balenfois" & corner_id %in% c("SW", "NW") ~ 0,
      plot_id == "Balenfois" & corner_id %in% c("SE", "NE") ~ 100,
      plot_id == "Grand_Plateau" & corner_id %in% c("NW", "NE") ~ 0,
      plot_id == "Grand_Plateau" & corner_id %in% c("SW", "SE") ~ 1000,
      plot_id == "Parare-Arataye" & corner_id %in% c("SE", "SW") ~ 0,
      plot_id == "Parare-Arataye" & corner_id %in% c("NE", "NW") ~ 300,
      plot_id == "Petit_Plateau" & corner_id %in% c("NW", "NE") ~ 0,
      plot_id == "Petit_Plateau" & corner_id %in% c("SW", "SE") ~ 300,
      TRUE ~ NA_real_),
    y_rel_m = case_when(
      plot_id == "Balenfois" & corner_id %in% c("SW", "SE") ~ 0,
      plot_id == "Balenfois" & corner_id %in% c("NW", "NE") ~ 200,
      plot_id == "Grand_Plateau" & corner_id %in% c("NW", "SW") ~ 0,
      plot_id == "Grand_Plateau" & corner_id %in% c("NE", "SE") ~ 100,
      plot_id == "Parare-Arataye" & corner_id %in% c("SE", "NE") ~ 0,
      plot_id == "Parare-Arataye" & corner_id %in% c("SW", "NW") ~ 200,
      plot_id == "Petit_Plateau" & corner_id %in% c("NW", "SW") ~ 0,
      plot_id == "Petit_Plateau" & corner_id %in% c("NE", "SE") ~ 400,
      TRUE ~ NA_real_)) %>% 
  dplyr::select(-id) %>% 
  dplyr::select(all_of(pt_cols$column_name))

# Create polygons
poly <- pt %>% 
  group_by(site_id, plot_id) %>% 
  summarise() %>% 
  st_convex_hull() %>% 
  ungroup() %>% 
  mutate(
    min_diam_thresh_cm = 10,
    census_id_all = "1") %>% 
  filter(plot_id == "Petit_Plateau") %>%  # TODO:
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


