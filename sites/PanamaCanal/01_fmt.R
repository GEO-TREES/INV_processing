# Clean Panama plot polygons data
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2025-06-18

# Packages
library(dplyr)
library(tidyr)
library(sf)

# Source functions
# source("./func.R")

# Define site ID
# site_id <- "PanamaCanal"

# Define directories
# indir <- "./dat/sites/PanamaCanal/raw"
# outdir <- "./dat/sites/PanamaCanal/01_fmt"

# Import column descriptions
# plot_cols <- read.csv("./templates/plot_cols.csv")
# pt_cols <- read.csv("./templates/pt_cols.csv")
# stem_cols <- read.csv("./templates/stem_cols.csv")
# census_cols <- read.csv("./templates/census_cols.csv")

# Import stem data from Gigante
# c/o Suzanne Lao, Helene Muller-Landau
s <- read.delim(file.path(indir, "gigante/Gigante_2023census_WorkingFile20250301.txt"))
n <- read.delim(file.path(indir, "gigante/nomenclature_R_20210224_Rready.txt"))

# Import stem data from other plots, including BCI 50 ha
s2 <- read.csv(file.path(indir, "request_2023.csv")) 

# Import stem data from Joe Wright, 2025
# Three censuses of "large trees" > 20 cm
# Six plots on BCI
s3 <- read.table(file.path(indir, "doi_10_5061_dryad_1g1jwsvc3__v20260224/mainfile.txt"), 
  header = TRUE)
s3_multi <- read.table(file.path(indir, "doi_10_5061_dryad_1g1jwsvc3__v20260224/stemfile.txt"),
  header = TRUE)
s3_sp <- read.table(file.path(indir, "doi_10_5061_dryad_1g1jwsvc3__v20260224/Key_sp6.txt"),
  header = TRUE)

# Import SW corners from Joe Wright's plots
joe_corner <- read.table(file.path(indir, "doi_10_5061_dryad_1g1jwsvc3__v20260224/Plot_UTM_coordinates.txt"),
  header = TRUE)

# Read in plot polygons as sf objects
# Gigante plot
gigante_poly <- read_sf(file.path(indir, "gigante/Gigante_Fertilization_Plot.shp")) %>% 
  mutate(plot_id = "Gigante fertilization plot") %>%
  dplyr::select(plot_id) 

# 50 ha plot of Barro Colorado Island
bci50ha_poly <- read_sf(file.path(indir, "bci_50ha/BCI_50ha.shp")) %>% 
  mutate(plot_id = "BCI 50 ha plot") %>%
  dplyr::select(plot_id) 

# Several 1 ha satellite plots that were also censused in 2023
# Select only two plots coinciding with ALS acquisition
ctfssmall_poly <- read_sf(file.path(indir, "ctfs_1ha/CTFS_Plots_Polygons.shp")) %>% 
  mutate(plot_id = DESC_) %>% 
  filter(plot_id %in% c("P06", "P12", "P14", "P15", "ElCharco", 
    "FincaRoubik", "Metrop", "Soberania")) %>%
  dplyr::select(plot_id) 

# San Lorenzo plot
sanlorenzo_poly <- read_sf(file.path(indir, "san_lorenzo/san_lorenzo.shp")) %>% 
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
  mutate(site_id)

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
  st_transform(., 4326) %>% 
  dplyr::select(any_of(pt_cols$column_name))

# Joe's polygons 
# Define function to generate other corners 
get_plot_geometry <- function(row) {
  # Calculate width (W) and height (H) based on local extents
  W <- row$xmax - row$xmin
  H <- row$ymax - row$ymin
  
  # Create a base square polygon at origin (0,0)
  # Order of corners: SW, SE, NE, NW, SW (must be closed)
  base_coords <- matrix(c(
    0, 0,
    W, 0,
    W, H,
    0, H,
    0, 0
  ), ncol = 2, byrow = TRUE)
  
  base_poly <- st_polygon(list(base_coords))
  
  # Set up the Rotation matrix 
  theta <- row$angle
  rot_matrix <- matrix(c(cos(theta), -sin(theta),
                         sin(theta),  cos(theta)), 
                       nrow = 2, byrow = TRUE)
  
  # Apply affine transformations: 
  # 1. Multiply by the rotation matrix
  # 2. Translate by adding the SW UTM coordinates
  transformed_poly <- (base_poly * rot_matrix) + c(row$SW_UTM_easting, row$SW_UTM_northing)
  
  return(transformed_poly)
}

# Apply the function to get a list of sfg (geometry) objects
joe_corner_fil <- joe_corner %>% 
  filter(Plot != "50-ha") %>% 
  mutate(
    SW_UTM_northing = ifelse(Plot == "AVA", SW_UTM_northing + 50, SW_UTM_northing),
    xmax = ifelse(Plot == "AVA", xmax - 50, xmax),
    ymax = ifelse(Plot == "AVA", ymax - 50, ymax))
geom_list <- lapply(1:nrow(joe_corner_fil), function(i) get_plot_geometry(joe_corner_fil[i, ]))

# 4. Extract all 4 corners back into a clean dataframe
joe_corner_all <- bind_rows(lapply(1:nrow(joe_corner_fil), function(i) {
  coords <- st_coordinates(geom_list[[i]])
  data.frame(
    Plot = joe_corner_fil$Plot[i],
    Corner = c("SW", "SE", "NE", "NW"),
    UTM_Easting = coords[1:4, "X"],  # Extracted Easting
    UTM_Northing = coords[1:4, "Y"]  # Extracted Northing
  )
})) %>% 
  st_as_sf(., coords = c("UTM_Easting", "UTM_Northing"), crs = 32617) %>% 
  rename(
    plot_id = Plot,
    corner_id = Corner) %>% 
  mutate(
    site_id,
    x_rel_m = case_when(
      plot_id == "10-ha" & corner_id == "SW" ~ 0,
      plot_id == "10-ha" & corner_id == "SE" ~ 1000,
      plot_id == "10-ha" & corner_id == "NW" ~ 0,
      plot_id == "10-ha" & corner_id == "NE" ~ 1000,
      plot_id == "25-ha" & corner_id == "SW" ~ 780,
      plot_id == "25-ha" & corner_id == "SE" ~ 1280,
      plot_id == "25-ha" & corner_id == "NW" ~ 780,
      plot_id == "25-ha" & corner_id == "NE" ~ 1280,
      plot_id == "AVA" & corner_id == "SW" ~ 0,
      plot_id == "AVA" & corner_id == "SE" ~ 150,
      plot_id == "AVA" & corner_id == "NW" ~ 0,
      plot_id == "AVA" & corner_id == "NE" ~ 150,
      plot_id == "Drayton" & corner_id == "SW" ~ 0,
      plot_id == "Drayton" & corner_id == "SE" ~ 200,
      plot_id == "Drayton" & corner_id == "NW" ~ 0,
      plot_id == "Drayton" & corner_id == "NE" ~ 200,
      plot_id == "Pearson" & corner_id == "SW" ~ 0,
      plot_id == "Pearson" & corner_id == "SE" ~ 200,
      plot_id == "Pearson" & corner_id == "NW" ~ 0,
      plot_id == "Pearson" & corner_id == "NE" ~ 200,
      plot_id == "Zetek" & corner_id == "SW" ~ 0,
      plot_id == "Zetek" & corner_id == "SE" ~ 200,
      plot_id == "Zetek" & corner_id == "NW" ~ 0,
      plot_id == "Zetek" & corner_id == "NE" ~ 200,
      TRUE ~ NA_real_),
    y_rel_m = case_when(
      plot_id == "10-ha" & corner_id == "SW" ~ 500,
      plot_id == "10-ha" & corner_id == "SE" ~ 500,
      plot_id == "10-ha" & corner_id == "NW" ~ 600,
      plot_id == "10-ha" & corner_id == "NE" ~ 600,
      plot_id == "25-ha" & corner_id == "SW" ~ 600,
      plot_id == "25-ha" & corner_id == "SE" ~ 600,
      plot_id == "25-ha" & corner_id == "NW" ~ 1100,
      plot_id == "25-ha" & corner_id == "NE" ~ 1100,
      plot_id == "AVA" & corner_id == "SW" ~ 50,
      plot_id == "AVA" & corner_id == "SE" ~ 50,
      plot_id == "AVA" & corner_id == "NW" ~ 300,
      plot_id == "AVA" & corner_id == "NE" ~ 300,
      plot_id == "Drayton" & corner_id == "SW" ~ 0,
      plot_id == "Drayton" & corner_id == "SE" ~ 0,
      plot_id == "Drayton" & corner_id == "NW" ~ 300,
      plot_id == "Drayton" & corner_id == "NE" ~ 300,
      plot_id == "Pearson" & corner_id == "SW" ~ 0,
      plot_id == "Pearson" & corner_id == "SE" ~ 0,
      plot_id == "Pearson" & corner_id == "NW" ~ 300,
      plot_id == "Pearson" & corner_id == "NE" ~ 300,
      plot_id == "Zetek" & corner_id == "SW" ~ 0,
      plot_id == "Zetek" & corner_id == "SE" ~ 0,
      plot_id == "Zetek" & corner_id == "NW" ~ 300,
      plot_id == "Zetek" & corner_id == "NE" ~ 300,
      TRUE ~ NA_real_)) %>% 
  st_transform(., 4326) %>% 
  mutate(
    plot_id = case_when(
      plot_id == "10-ha" ~ "10ha",
      plot_id == "25-ha" ~ "25ha",
      TRUE ~ plot_id)) %>% 
  dplyr::select(any_of(pt_cols$column_name))

pts_all <- bind_rows(pts, joe_corner_all) %>% 
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

s3_clean <- s3 %>% 
  filter(census == 2025) %>% 
  bind_rows(., s3_multi) %>% 
  left_join(., s3_sp, by = "sp6") %>% 
  rename(
    plot_id = plot,
    measurement_date = date,
    tree_id = tag,
    x_rel_m = px,
    y_rel_m = py,
    pom_m = hom,
    diam_cm = dbh,
    alive = status) %>% 
  mutate(
    census_id = as.integer(3),
    diam_cm = diam_cm / 10,
    measurement_date = format(as.Date(as.character(measurement_date), format = "%Y%m%d")),
    taxon_name = paste(genus, species),
    alive = case_when(
      alive == "future recruit" ~ TRUE,
      alive == "alive" ~ TRUE,
      alive == "dead" ~ FALSE,
      alive == "missing" ~ FALSE,
      TRUE ~ NA),
    broken = ifelse(grepl("R", code), TRUE , FALSE),
    missing = ifelse(grepl("N", code), TRUE , FALSE),
    fallen = FALSE,
    ) %>% 
  group_by(plot_id, tree_id) %>% 
  mutate(stem_id = row_number()) %>% 
  fill(
    all_of(c(
      "alive",
      "measurement_date",
      "x_rel_m",
      "y_rel_m",
      "taxon_name",
      "pom_m")),
    .direction = "downup") %>% 
  ungroup() %>% 
  group_by(plot_id, tree_id, stem_id) %>% 
  mutate(measurement_id = row_number()) %>% 
  ungroup() %>% 
  dplyr::select(any_of(stem_cols$column_name))

# Join stems tables
# Add metadata to stems
s_all <- bind_rows(s_clean, s2_clean, s3_clean) %>% 
  mutate(
    subplot_id = as.character(subplot_id),
    tree_id = as.character(tree_id),
    stem_id = as.character(stem_id),
    record_id = row_number(),
    site_id,
    height_m = NA_real_,
    taxon_name = gsub("NA NA", "Indet indet", taxon_name),
    diam_cm = ifelse(diam_cm == 0, NA_real_, diam_cm),
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
  dplyr::select(all_of(census_cols$column_name))

# Prepare plots table
plots <- census %>% 
  dplyr::select(
    site_id, plot_id, census_date_all = census_date) %>% 
  mutate(
    census_date_geotrees = census_date_all,
    plot_width_m = case_when(
      plot_id == "BCI 50 ha plot" ~ 500,
      plot_id == "ElCharco" ~ 100,
      plot_id == "FincaRoubik" ~ 100 ,
      plot_id == "Gigante fertilization plot" ~ 480,
      plot_id == "Metrop" ~ 100,
      plot_id == "P06" ~ 100,
      plot_id == "P12" ~ 100,
      plot_id == "P14" ~ 100,
      plot_id == "P15" ~ 100,
      plot_id == "San Lorenzo A" ~ 140,
      plot_id == "San Lorenzo B" ~ 100,
      plot_id == "Soberania" ~ 100,
      plot_id == "10-ha" ~ 100,
      plot_id == "25-ha" ~ 500,
      plot_id == "AVA" ~ 150,
      plot_id == "Drayton" ~ 200,
      plot_id == "Pearson" ~ 200,
      plot_id == "Zetek" ~ 200,
      TRUE ~ NA_real_),
    plot_length_m = case_when(
      plot_id == "BCI 50 ha plot" ~ 1000,
      plot_id == "ElCharco" ~ 100,
      plot_id == "FincaRoubik" ~ 100 ,
      plot_id == "Gigante fertilization plot" ~ 800,
      plot_id == "Metrop" ~ 100,
      plot_id == "P06" ~ 100,
      plot_id == "P12" ~ 100,
      plot_id == "P14" ~ 100,
      plot_id == "P15" ~ 100,
      plot_id == "San Lorenzo A" ~ 140,
      plot_id == "San Lorenzo B" ~ 400,
      plot_id == "Soberania" ~ 100,
      plot_id == "10-ha" ~ 1000,
      plot_id == "25-ha" ~ 500,
      plot_id == "AVA" ~ 250,
      plot_id == "Drayton" ~ 300,
      plot_id == "Pearson" ~ 300,
      plot_id == "Zetek" ~ 300,
      TRUE ~ NA_real_),
    plot_slope_deg = NA_real_,
    plot_aspect_deg = NA_real_,
    plot_elevation_m = NA_real_,
    plot_planar = FALSE,
    notes_plot = NA_character_,
    meas_diam_min_cm = case_when(
      plot_id == "BCI 50 ha plot" ~ 10,
      plot_id == "ElCharco" ~ 10,
      plot_id == "FincaRoubik" ~ 10,
      plot_id == "Gigante fertilization plot" ~ 10,
      plot_id == "Metrop" ~ 10,
      plot_id == "P06" ~ 10,
      plot_id == "P12" ~ 10,
      plot_id == "P14" ~ 10,
      plot_id == "P15" ~ 10,
      plot_id == "San Lorenzo A" ~ 10,
      plot_id == "San Lorenzo B" ~ 10,
      plot_id == "Soberania" ~ 10,
      plot_id == "10ha" ~ 20,
      plot_id == "25ha" ~ 20,
      plot_id == "AVA" ~ 20,
      plot_id == "Drayton" ~ 20,
      plot_id == "Pearson" ~ 20,
      plot_id == "Zetek" ~ 20,
      TRUE ~ NA_real_),
    meas_pom_default_m = 1.3,
    meas_tree_stem = TRUE,
    meas_tree_group = TRUE,
    meas_dead = TRUE,
    meas_fallen = TRUE,
    meas_liana = TRUE,
    meas_palm = TRUE,
    meas_bamboo = TRUE,
    meas_protocol = NA_character_,
    notes_meas = NA_character_,
    forest_status = NA_character_,
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
colCheck(plots, plot_cols)
colCheck(pts_all, pt_cols)
colCheck(s_all, stem_cols)
colCheck(census, census_cols)

# Check values
# plotValCheck(plots)
ptValCheck(pts_all)
stemValCheck(s_all)
censusValCheck(census)

# Write origin points to file
st_write(pts_all, file.path(outdir, "plot_pt.gpkg"), delete_dsn = TRUE)

# Write stem data to file
write.csv(s_all, file.path(outdir, "stem.csv"), row.names = FALSE)

# Write stem data to file
write.csv(plots, file.path(outdir, "plot.csv"), row.names = FALSE)

# Write census table to file
write.csv(census, file.path(outdir, "census.csv"), row.names = FALSE)
