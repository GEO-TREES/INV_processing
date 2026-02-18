# Clean Panama plot polygons data
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2025-06-18

# Packages
library(dplyr)
library(sf)

# Source functions
source("../../func.R")

# Define site ID
site_id <- "Panama_Canal"

# Define directories
indir <- "../../dat/sites/Panama_Canal/raw"
outdir <- "../../dat/sites/Panama_Canal/01_fmt"

# Import column descriptions
poly_cols <- read.csv("../../templates/poly_cols.csv")
pt_cols <- read.csv("../../templates/pt_cols.csv")
stem_cols <- read.csv("../../templates/stem_cols.csv")
census_cols <- read.csv("../../templates/census_cols.csv")

# Import stem data from Gigante
# c/o Suzanne Lao, Helene Muller-Landau
s <- read.delim(file.path(indir, "Gigante_2023census_WorkingFile20250301.txt"))
n <- read.delim(file.path(indir, "nomenclature_R_20210224_Rready.txt"))

s2 <- read.csv(file.path(indir, "request_2023.csv")) 

# Read in plot polygons as sf objects
# Gigante plot
gigante_poly <- read_sf(file.path(indir, "Gigante_Fertilization_Plot.shp")) %>% 
  mutate(plot_id = "Gigante fertilization plot") %>%
  dplyr::select(plot_id) 

# 50 ha plot of Barro Colorado Island
bci50ha_poly <- read_sf(file.path(indir, "BCI_50ha.shp")) %>% 
  mutate(plot_id = "BCI 50 ha plot") %>%
  dplyr::select(plot_id) 

# Several 1 ha satellite plots that were also censused in 2023
# Select only two plots coinciding with ALS acquisition
ctfssmall_poly <- read_sf(file.path(indir, "CTFS_Plots_Polygons.shp")) %>% 
  mutate(plot_id = DESC_) %>% 
  filter(plot_id %in% c("P06", "P12", "P14", "P15", "ElCharco", 
    "FincaRoubik", "Metrop", "Soberania")) %>%
  dplyr::select(plot_id) 

# San Lorenzo plot
sanlorenzo_poly <- read_sf(file.path(indir, "san_lorenzo.shp")) %>% 
  mutate(
    plot_id = c("San Lorenzo B", "San Lorenzo A")) %>% 
  dplyr::select(plot_id) %>% 
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
  mutate(site_id) %>% 
  dplyr::select(all_of(poly_cols$column_name))

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
  dplyr::select(all_of(pt_cols$column_name))

# Prepare stem data 
s_clean <- s %>% 
  left_join(., n, by = c("sp23" = "sp6")) %>% 
  filter(!as.numeric(dbh23) %in% c(-9, 0)) %>%  # Remove missing records
  mutate(
    broken = ifelse(grepl("X|Q", code23), TRUE, FALSE),  # 
    fallen = ifelse(grepl("Y", code23), TRUE, FALSE),
    missing = FALSE,
    alive = TRUE,
    measurement_date = format(as.Date(as.character(date23), format = "%Y%m%d"), "%Y-%m-%d"),
    diam_cm = dbh23 / 10,
    plot_id = "Gigante fertilization plot",
    x_rel_m = as.numeric(gx23),
    y_rel_m = as.numeric(gy23),
    stem_id = tag,
    census_id = as.integer(1),
    taxon_name = paste(genus, species)) %>% 
  dplyr::select(any_of(stem_cols$column_name))

s2_clean <- s2 %>% 
  filter(
    as.numeric(DBH) != -9,  # Remove missing records
    PlotName %in% c("bci", "P12", "P14", "P06", "P15", "elcharco", "metrop", 
      "soberania", "FincaRoubik", "sherman")) %>% 
  mutate(
    alive = ifelse(Status %in% c("alive", "broken below"), TRUE, FALSE),
    broken = ifelse(Status == "broken below", TRUE, FALSE),
    missing = ifelse(Status == "missing", TRUE, FALSE),
    missing = FALSE,
    fallen = ifelse(grepl("Y", ListOfTSM), TRUE, FALSE),
    plot_id = case_when(
      PlotName == "bci" ~ "BCI 50 ha plot",
      PlotName == "elcharco" ~ "ElCharco",
      PlotName == "metrop" ~ "Metrop",
      PlotName == "soberania" ~ "Soberania",
      PlotName == "sherman" & QuadratID < 1520 ~ "San Lorenzo A",
      PlotName == "sherman" & QuadratID >= 1520 ~ "San Lorenzo B",
      TRUE ~ PlotName),
    diam_cm = as.numeric(DBH) / 10,
    x_rel_m = as.numeric(PX),
    y_rel_m = as.numeric(PY),
    x_rel_m = case_when(
      plot_id == "San Lorenzo B" ~ x_rel_m - 140,
      TRUE ~ x_rel_m),
    y_rel_m = case_when(
      plot_id == "San Lorenzo B" ~ y_rel_m - 40,
      TRUE ~ y_rel_m),
    pom_m = as.numeric(HOM),
    taxon_name = paste(Genus, SpeciesName),
    census_id = as.integer(PlotCensusNumber)
  ) %>% 
  rename(
    measurement_date = ExactDate,
    subplot_id = QuadratID,
    stem_id = StemID,
    tree_id = TreeID) %>% 
  dplyr::select(any_of(stem_cols$column_name))

# Join stems tables
# Add metadata to stems
s_all <- bind_rows(s_clean, s2_clean) %>% 
  mutate(
    subplot_id = as.character(subplot_id),
    tree_id = as.character(tree_id),
    stem_id = as.character(stem_id),
    record_id = row_number(),
    site_id,
    height_m = NA_real_,
    taxon_name = gsub("NA NA", "Indet indet", taxon_name),
    agb_allometry = NA_character_) %>%
  group_by(plot_id, census_id, stem_id) %>% 
  mutate(measurement_id = row_number()) %>% 
  ungroup() %>% 
  dplyr::select(all_of(stem_cols$column_name))

# Prepare census table
census <- s_all %>% 
  group_by(site_id, plot_id, census_id) %>% 
  summarise(census_date = format(mean(as.Date(measurement_date), na.rm = TRUE))) %>% 
  ungroup() %>% 
  mutate(min_diam_thresh_cm = 10) %>% 
  dplyr::select(all_of(census_cols$column_name))

# Check all columns in output objects
colCheck(polys, poly_cols)
colCheck(pts, pt_cols)
colCheck(s_all, stem_cols)
colCheck(census, census_cols)

# Check values
polyValCheck(polys)
ptValCheck(pts)
stemValCheck(s_all)
censusValCheck(census)

# Write polygons to file
st_write(polys, file.path(outdir, "plot_poly.gpkg"), delete_dsn = TRUE)

# Write origin points to file
st_write(pts, file.path(outdir, "plot_pt.gpkg"), delete_dsn = TRUE)

# Write stem data to file
write.csv(s_all, file.path(outdir, "stem.csv"), row.names = FALSE)

# Write census table to file
write.csv(census, file.path(outdir, "census.csv"), row.names = FALSE)
