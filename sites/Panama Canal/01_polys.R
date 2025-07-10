# Clean Panama plot polygons data
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2025-06-18

# Packages
library(dplyr)
library(ggplot2)
library(sf)

# Define site ID
site_id <- "Panama Canal"

# Source functions
source("../../func.R")

# Define directories
indir <- "../../dat/sites/Panama Canal/raw"
outdir <- "../../dat/sites/Panama Canal/01_polys"

# Read in plot polygons as sf objects
# Gigante plot
gigante_poly <- read_sf(file.path(indir, "Gigante_Fertilization_Plot.shp")) %>% 
  mutate(
    plot_name = "Gigante fertilization plot",
    area_reported_ha = HECTARES) %>%
  dplyr::select(plot_name, area_reported_ha) 

# 50 ha plot of Barro Colorado Island
bci50ha_poly <- read_sf(file.path(indir, "BCI_50ha.shp")) %>% 
  mutate(
    plot_name = "BCI 50 ha plot",
    area_reported_ha = AREA) %>%
  dplyr::select(plot_name, area_reported_ha) 

# Several 1 ha satellite plots that were also censused in 2023
# Select only two plots coinciding with ALS acquisition
ctfssmall_poly <- read_sf(file.path(indir, "CTFS_Plots_Polygons.shp")) %>% 
  mutate(
    plot_name = DESC_,
    area_reported_ha = AREA_HA,
    perim_reported_m = Perimeter) %>% 
  filter(plot_name %in% c("P06", "P12", "P14", "P15", "ElCharco", 
    "FincaRoubik", "Metrop", "Soberania")) %>%
  dplyr::select(plot_name, area_reported_ha, perim_reported_m) 

# San Lorenzo plot
sanlorenzo_poly <- read_sf(file.path(indir, "Sherman_Study_Plot.shp")) %>% 
  mutate(
    plot_name = "San Lorenzo",
    area_reported_ha = Shape_Area * 0.0001) %>% 
  dplyr::select(plot_name, area_reported_ha) %>% 
  st_transform(., st_crs(gigante_poly))

# Define cutting line 
sanlorenzo_points <- sanlorenzo_poly %>% 
  st_cast(., "POINT") %>% 
  mutate(id = row_number())

ggplot() + 
geom_sf(data = sanlorenzo_points, aes(colour = as.factor(id)))

sanlorenzo_a_poly <- sanlorenzo_points[c(4, 5, 6, 7, 4),] %>% 
  st_combine() %>% 
  st_cast(., "POLYGON") %>% 
  st_sf() %>% 
  mutate(plot_name = "San Lorenzo A") %>% 
  relocate(plot_name)

sanlorenzo_b_poly <- sanlorenzo_points[c(1, 2, 3, 8, 1),] %>% 
  st_combine() %>% 
  st_cast(., "POLYGON") %>% 
  st_sf() %>% 
  mutate(plot_name = "San Lorenzo B") %>% 
  relocate(plot_name)

# Combine all polys 
polys <- bind_rows(gigante_poly, bci50ha_poly, ctfssmall_poly, 
  sanlorenzo_a_poly, sanlorenzo_b_poly) %>%
  relocate(geometry, .after = last_col()) %>% 
  mutate(site_id, .before = everything())

# Cast polygons to points
polys_pts <- st_cast(polys, "POINT") %>% 
  dplyr::select(site_id, plot_name) %>% 
  group_by(site_id, plot_name) %>%
  slice_head(n = -1) %>% 
  mutate(corner_id = row_number()) %>% 
  mutate(
    x_rel = case_when(
      plot_name == "BCI 50 ha plot" & corner_id == 1 ~ 0,
      plot_name == "BCI 50 ha plot" & corner_id == 2 ~ 0,
      plot_name == "BCI 50 ha plot" & corner_id == 3 ~ 1000,
      plot_name == "BCI 50 ha plot" & corner_id == 4 ~ 1000,
      plot_name == "ElCharco" & corner_id == 1 ~ 0,
      plot_name == "ElCharco" & corner_id == 2 ~ 100,
      plot_name == "ElCharco" & corner_id == 3 ~ 100,
      plot_name == "ElCharco" & corner_id == 4 ~ 0,
      plot_name == "FincaRoubik" & corner_id == 1 ~ 0,
      plot_name == "FincaRoubik" & corner_id == 2 ~ 100,
      plot_name == "FincaRoubik" & corner_id == 3 ~ 100,
      plot_name == "FincaRoubik" & corner_id == 4 ~ 0,
      plot_name == "Gigante fertilization plot" & corner_id == 1 ~ 0,
      plot_name == "Gigante fertilization plot" & corner_id == 2 ~ 0,
      plot_name == "Gigante fertilization plot" & corner_id == 3 ~ 480,
      plot_name == "Gigante fertilization plot" & corner_id == 4 ~ 480,
      plot_name == "Metrop" & corner_id == 1 ~ 0,
      plot_name == "Metrop" & corner_id == 2 ~ 100,
      plot_name == "Metrop" & corner_id == 3 ~ 100,
      plot_name == "Metrop" & corner_id == 4 ~ 0,
      plot_name == "P06" & corner_id == 1 ~ 0,
      plot_name == "P06" & corner_id == 2 ~ 100,
      plot_name == "P06" & corner_id == 3 ~ 100,
      plot_name == "P06" & corner_id == 4 ~ 0,
      plot_name == "P12" & corner_id == 1 ~ 0,
      plot_name == "P12" & corner_id == 2 ~ 100,
      plot_name == "P12" & corner_id == 3 ~ 100,
      plot_name == "P12" & corner_id == 4 ~ 0,
      plot_name == "P14" & corner_id == 1 ~ 0,
      plot_name == "P14" & corner_id == 2 ~ 100,
      plot_name == "P14" & corner_id == 3 ~ 100,
      plot_name == "P14" & corner_id == 4 ~ 0,
      plot_name == "P15" & corner_id == 1 ~ 0,
      plot_name == "P15" & corner_id == 2 ~ 100,
      plot_name == "P15" & corner_id == 3 ~ 100,
      plot_name == "P15" & corner_id == 4 ~ 0,
      plot_name == "San Lorenzo A" & corner_id == 1 ~ 140,
      plot_name == "San Lorenzo A" & corner_id == 2 ~ 140,
      plot_name == "San Lorenzo A" & corner_id == 3 ~ 0,
      plot_name == "San Lorenzo A" & corner_id == 4 ~ 0,
      plot_name == "San Lorenzo B" & corner_id == 1 ~ 100,
      plot_name == "San Lorenzo B" & corner_id == 2 ~ 100,
      plot_name == "San Lorenzo B" & corner_id == 3 ~ 0,
      plot_name == "San Lorenzo B" & corner_id == 4 ~ 0,
      plot_name == "Soberania" & corner_id == 1 ~ 0,
      plot_name == "Soberania" & corner_id == 2 ~ 100,
      plot_name == "Soberania" & corner_id == 3 ~ 100,
      plot_name == "Soberania" & corner_id == 4 ~ 0,
      TRUE ~ NA_real_),
    y_rel = case_when(
      plot_name == "BCI 50 ha plot" & corner_id == 1 ~ 500,
      plot_name == "BCI 50 ha plot" & corner_id == 2 ~ 0,
      plot_name == "BCI 50 ha plot" & corner_id == 3 ~ 0,
      plot_name == "BCI 50 ha plot" & corner_id == 4 ~ 500,
      plot_name == "ElCharco" & corner_id == 1 ~ 0,
      plot_name == "ElCharco" & corner_id == 2 ~ 0,
      plot_name == "ElCharco" & corner_id == 3 ~ 100,
      plot_name == "ElCharco" & corner_id == 4 ~ 100,
      plot_name == "FincaRoubik" & corner_id == 1 ~ 0,
      plot_name == "FincaRoubik" & corner_id == 2 ~ 0,
      plot_name == "FincaRoubik" & corner_id == 3 ~ 100,
      plot_name == "FincaRoubik" & corner_id == 4 ~ 100,
      plot_name == "Gigante fertilization plot" & corner_id == 1 ~ 800,
      plot_name == "Gigante fertilization plot" & corner_id == 2 ~ 0,
      plot_name == "Gigante fertilization plot" & corner_id == 3 ~ 0,
      plot_name == "Gigante fertilization plot" & corner_id == 4 ~ 800,
      plot_name == "Metrop" & corner_id == 1 ~ 0,
      plot_name == "Metrop" & corner_id == 2 ~ 0,
      plot_name == "Metrop" & corner_id == 3 ~ 100,
      plot_name == "Metrop" & corner_id == 4 ~ 100,
      plot_name == "P06" & corner_id == 1 ~ 0,
      plot_name == "P06" & corner_id == 2 ~ 0,
      plot_name == "P06" & corner_id == 3 ~ 100,
      plot_name == "P06" & corner_id == 4 ~ 100,
      plot_name == "P12" & corner_id == 1 ~ 0,
      plot_name == "P12" & corner_id == 2 ~ 0,
      plot_name == "P12" & corner_id == 3 ~ 100,
      plot_name == "P12" & corner_id == 4 ~ 100,
      plot_name == "P14" & corner_id == 1 ~ 0,
      plot_name == "P14" & corner_id == 2 ~ 0,
      plot_name == "P14" & corner_id == 3 ~ 100,
      plot_name == "P14" & corner_id == 4 ~ 100,
      plot_name == "P15" & corner_id == 1 ~ 0,
      plot_name == "P15" & corner_id == 2 ~ 0,
      plot_name == "P15" & corner_id == 3 ~ 100,
      plot_name == "P15" & corner_id == 4 ~ 100,
      plot_name == "San Lorenzo A" & corner_id == 1 ~ 0,
      plot_name == "San Lorenzo A" & corner_id == 2 ~ 140,
      plot_name == "San Lorenzo A" & corner_id == 3 ~ 140,
      plot_name == "San Lorenzo A" & corner_id == 4 ~ 0,
      plot_name == "San Lorenzo B" & corner_id == 1 ~ 0,
      plot_name == "San Lorenzo B" & corner_id == 2 ~ 400,
      plot_name == "San Lorenzo B" & corner_id == 3 ~ 400,
      plot_name == "San Lorenzo B" & corner_id == 4 ~ 0,
      plot_name == "Soberania" & corner_id == 1 ~ 0,
      plot_name == "Soberania" & corner_id == 2 ~ 0,
      plot_name == "Soberania" & corner_id == 3 ~ 100,
      plot_name == "Soberania" & corner_id == 4 ~ 100,
      TRUE ~ NA_real_)
    ) %>% 
  relocate(geometry, .after = last_col())

# Write polygons to file
st_write(polys, file.path(outdir, "polys.gpkg"), delete_dsn = TRUE)

# Write origin points to file
st_write(polys_pts, file.path(outdir, "pts.gpkg"), delete_dsn = TRUE)
