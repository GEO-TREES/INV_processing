# Clean Panama plot polygons data
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2025-06-18

# Packages
library(dplyr)
library(sf)

# Source functions
source("../../func.R")

# Define site ID
site_id <- "Panama Canal"

# Define directories
indir <- "../../dat/sites/Panama Canal/raw"
outdir <- "../../dat/sites/Panama Canal/01_plot"

# Import column descriptions
poly_cols <- read.csv("../../templates/poly_cols.csv")
pt_cols <- read.csv("../../templates/pt_cols.csv")

# Read in plot polygons as sf objects
# Gigante plot
gigante_poly <- read_sf(file.path(indir, "Gigante_Fertilization_Plot.shp")) %>% 
  mutate(
    plot_id = "Gigante fertilization plot",
    area_reported_ha = HECTARES) %>%
  dplyr::select(plot_id, area_reported_ha) 

# 50 ha plot of Barro Colorado Island
bci50ha_poly <- read_sf(file.path(indir, "BCI_50ha.shp")) %>% 
  mutate(
    plot_id = "BCI 50 ha plot",
    area_reported_ha = AREA) %>%
  dplyr::select(plot_id, area_reported_ha) 

# Several 1 ha satellite plots that were also censused in 2023
# Select only two plots coinciding with ALS acquisition
ctfssmall_poly <- read_sf(file.path(indir, "CTFS_Plots_Polygons.shp")) %>% 
  mutate(
    plot_id = DESC_,
    area_reported_ha = AREA_HA,
    perim_reported_m = as.numeric(Perimeter)) %>% 
  filter(plot_id %in% c("P06", "P12", "P14", "P15", "ElCharco", 
    "FincaRoubik", "Metrop", "Soberania")) %>%
  dplyr::select(plot_id, area_reported_ha, perim_reported_m) 

# San Lorenzo plot
sanlorenzo_poly <- read_sf(file.path(indir, "san_lorenzo.shp")) %>% 
  mutate(
    plot_id = c("San Lorenzo B", "San Lorenzo A"),
    area_reported_ha = Shape_Area * 0.0001) %>% 
  dplyr::select(plot_id, area_reported_ha) %>% 
  st_transform(., st_crs(gigante_poly)) %>% 
  st_cast(., "POINT") %>% 
  mutate(corner_id = as.character(row_number())) %>% 
  filter(corner_id %in% c(
    10, 9, 12, 11, 
    1, 3, 4, 5)) %>% 
  group_by(plot_id) %>% 
  summarise() %>% 
  st_cast(., "POLYGON")

# Define cutting line 
# sanlorenzo_points <- sanlorenzo_poly %>% 
#   st_cast(., "POINT") %>% 
#   mutate(id = row_number())

# ggplot() + 
# geom_sf(data = sanlorenzo_points, aes(colour = as.factor(id)))

#sanlorenzo_a_poly <- sanlorenzo_points[c(4, 5, 6, 7, 4),] %>% 
#  st_combine() %>% 
#  st_cast(., "POLYGON") %>% 
#  st_sf() %>% 
#  mutate(plot_id = "San Lorenzo A") %>% 
#  relocate(plot_id)
#
#sanlorenzo_b_poly <- sanlorenzo_points[c(1, 2, 3, 8, 1),] %>% 
#  st_combine() %>% 
#  st_cast(., "POLYGON") %>% 
#  st_sf() %>% 
#  mutate(plot_id = "San Lorenzo B") %>% 
#  relocate(plot_id)

# Combine all polys 
polys <- bind_rows(gigante_poly, bci50ha_poly, ctfssmall_poly, 
  sanlorenzo_poly) %>%
  relocate(geometry, .after = last_col()) %>% 
  mutate(site_id) %>% 
  relocate(site_id, .before = everything())

# Cast polygons to points
pts <- st_cast(polys, "POINT") %>% 
  dplyr::select(site_id, plot_id) %>% 
  group_by(site_id, plot_id) %>%
  slice_head(n = -1) %>% 
  mutate(corner_id = as.character(row_number())) %>% 
  mutate(
    x_rel_m = case_when(
      plot_id == "BCI 50 ha plot" & corner_id == 1 ~ 0,
      plot_id == "BCI 50 ha plot" & corner_id == 2 ~ 0,
      plot_id == "BCI 50 ha plot" & corner_id == 3 ~ 1000,
      plot_id == "BCI 50 ha plot" & corner_id == 4 ~ 1000,
      plot_id == "ElCharco" & corner_id == 1 ~ 0,
      plot_id == "ElCharco" & corner_id == 2 ~ 100,
      plot_id == "ElCharco" & corner_id == 3 ~ 100,
      plot_id == "ElCharco" & corner_id == 4 ~ 0,
      plot_id == "FincaRoubik" & corner_id == 1 ~ 0,
      plot_id == "FincaRoubik" & corner_id == 2 ~ 100,
      plot_id == "FincaRoubik" & corner_id == 3 ~ 100,
      plot_id == "FincaRoubik" & corner_id == 4 ~ 0,
      plot_id == "Gigante fertilization plot" & corner_id == 1 ~ 0,
      plot_id == "Gigante fertilization plot" & corner_id == 2 ~ 0,
      plot_id == "Gigante fertilization plot" & corner_id == 3 ~ 480,
      plot_id == "Gigante fertilization plot" & corner_id == 4 ~ 480,
      plot_id == "Metrop" & corner_id == 1 ~ 0,
      plot_id == "Metrop" & corner_id == 2 ~ 100,
      plot_id == "Metrop" & corner_id == 3 ~ 100,
      plot_id == "Metrop" & corner_id == 4 ~ 0,
      plot_id == "P06" & corner_id == 1 ~ 0,
      plot_id == "P06" & corner_id == 2 ~ 100,
      plot_id == "P06" & corner_id == 3 ~ 100,
      plot_id == "P06" & corner_id == 4 ~ 0,
      plot_id == "P12" & corner_id == 1 ~ 0,
      plot_id == "P12" & corner_id == 2 ~ 100,
      plot_id == "P12" & corner_id == 3 ~ 100,
      plot_id == "P12" & corner_id == 4 ~ 0,
      plot_id == "P14" & corner_id == 1 ~ 0,
      plot_id == "P14" & corner_id == 2 ~ 100,
      plot_id == "P14" & corner_id == 3 ~ 100,
      plot_id == "P14" & corner_id == 4 ~ 0,
      plot_id == "P15" & corner_id == 1 ~ 0,
      plot_id == "P15" & corner_id == 2 ~ 100,
      plot_id == "P15" & corner_id == 3 ~ 100,
      plot_id == "P15" & corner_id == 4 ~ 0,
      plot_id == "San Lorenzo A" & corner_id == 1 ~ 0,
      plot_id == "San Lorenzo A" & corner_id == 2 ~ 0,
      plot_id == "San Lorenzo A" & corner_id == 3 ~ 140,
      plot_id == "San Lorenzo A" & corner_id == 4 ~ 140,
      plot_id == "San Lorenzo B" & corner_id == 1 ~ 0,
      plot_id == "San Lorenzo B" & corner_id == 2 ~ 0,
      plot_id == "San Lorenzo B" & corner_id == 3 ~ 100,
      plot_id == "San Lorenzo B" & corner_id == 4 ~ 100,
      plot_id == "Soberania" & corner_id == 1 ~ 0,
      plot_id == "Soberania" & corner_id == 2 ~ 100,
      plot_id == "Soberania" & corner_id == 3 ~ 100,
      plot_id == "Soberania" & corner_id == 4 ~ 0,
      TRUE ~ NA_real_),
    y_rel_m = case_when(
      plot_id == "BCI 50 ha plot" & corner_id == 1 ~ 500,
      plot_id == "BCI 50 ha plot" & corner_id == 2 ~ 0,
      plot_id == "BCI 50 ha plot" & corner_id == 3 ~ 0,
      plot_id == "BCI 50 ha plot" & corner_id == 4 ~ 500,
      plot_id == "ElCharco" & corner_id == 1 ~ 0,
      plot_id == "ElCharco" & corner_id == 2 ~ 0,
      plot_id == "ElCharco" & corner_id == 3 ~ 100,
      plot_id == "ElCharco" & corner_id == 4 ~ 100,
      plot_id == "FincaRoubik" & corner_id == 1 ~ 0,
      plot_id == "FincaRoubik" & corner_id == 2 ~ 0,
      plot_id == "FincaRoubik" & corner_id == 3 ~ 100,
      plot_id == "FincaRoubik" & corner_id == 4 ~ 100,
      plot_id == "Gigante fertilization plot" & corner_id == 1 ~ 800,
      plot_id == "Gigante fertilization plot" & corner_id == 2 ~ 0,
      plot_id == "Gigante fertilization plot" & corner_id == 3 ~ 0,
      plot_id == "Gigante fertilization plot" & corner_id == 4 ~ 800,
      plot_id == "Metrop" & corner_id == 1 ~ 0,
      plot_id == "Metrop" & corner_id == 2 ~ 0,
      plot_id == "Metrop" & corner_id == 3 ~ 100,
      plot_id == "Metrop" & corner_id == 4 ~ 100,
      plot_id == "P06" & corner_id == 1 ~ 0,
      plot_id == "P06" & corner_id == 2 ~ 0,
      plot_id == "P06" & corner_id == 3 ~ 100,
      plot_id == "P06" & corner_id == 4 ~ 100,
      plot_id == "P12" & corner_id == 1 ~ 0,
      plot_id == "P12" & corner_id == 2 ~ 0,
      plot_id == "P12" & corner_id == 3 ~ 100,
      plot_id == "P12" & corner_id == 4 ~ 100,
      plot_id == "P14" & corner_id == 1 ~ 0,
      plot_id == "P14" & corner_id == 2 ~ 0,
      plot_id == "P14" & corner_id == 3 ~ 100,
      plot_id == "P14" & corner_id == 4 ~ 100,
      plot_id == "P15" & corner_id == 1 ~ 0,
      plot_id == "P15" & corner_id == 2 ~ 0,
      plot_id == "P15" & corner_id == 3 ~ 100,
      plot_id == "P15" & corner_id == 4 ~ 100,
      plot_id == "San Lorenzo A" & corner_id == 1 ~ 140,
      plot_id == "San Lorenzo A" & corner_id == 2 ~ 0,
      plot_id == "San Lorenzo A" & corner_id == 3 ~ 0,
      plot_id == "San Lorenzo A" & corner_id == 4 ~ 140,
      plot_id == "San Lorenzo B" & corner_id == 1 ~ 0,
      plot_id == "San Lorenzo B" & corner_id == 2 ~ 400,
      plot_id == "San Lorenzo B" & corner_id == 3 ~ 400,
      plot_id == "San Lorenzo B" & corner_id == 4 ~ 0,
      plot_id == "Soberania" & corner_id == 1 ~ 0,
      plot_id == "Soberania" & corner_id == 2 ~ 0,
      plot_id == "Soberania" & corner_id == 3 ~ 100,
      plot_id == "Soberania" & corner_id == 4 ~ 100,
      TRUE ~ NA_real_)
    ) %>% 
  relocate(geometry, .after = last_col())

# Check all columns in output objects
colCheck(polys, poly_cols)
colCheck(pts, pt_cols)

# Check values
polyValCheck(polys)
ptValCheck(pts)

# Write polygons to file
st_write(polys, file.path(outdir, "plot_poly.gpkg"), delete_dsn = TRUE)

# Write origin points to file
st_write(pts, file.path(outdir, "plot_pt.gpkg"), delete_dsn = TRUE)
