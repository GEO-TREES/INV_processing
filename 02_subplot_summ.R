# Create subplot-level summary statistics from tree inventory data
# John L. Godlee (johngodlee@gmail.com)  
# Last updated: 2025-06-18# Define directories

indir <- "../dat/clean/panama"
outdir <- "../dat/processed/panama"

# Packages
library(dplyr)
library(sf)
library(BIOMASS)

# Source functions
source("./func.R")

# Import stem data
s <- read.csv(file.path(indir, "stems.csv"))

# Import plot polygons
p <- read_sf(file.path(indir, "polys.gpkg"))

# Import plot origins
o <- read_sf(file.path(indir, "origins.gpkg"))

ps <- read_sf(file.path(outdir, "subplot_polys.gpkg"))

# Convert stem data to sf object
s_sf <- s %>% 
  st_as_sf(., coords = c("x_grid", "y_grid"))

# Split stem data by plot
s_split <- split(s_sf, s_sf$plot_id)[p$plot_id]

# Rotate stem coordinates from each plot
s_trans <- globalCoord(s_split, p, o, p$bearing, name = colnames(s_sf))

# Split subplots by plot 
ps_split <- split(ps, ps$plot_id)[p$plot_id]

# Assign stems to subplots 
s_sub <- subplotAssign(s_trans, ps_split, 
  name = c(colnames(s_sf), "subplot_id"))

# Join stem dataframes
s_all <- bind_rows(s_sub)

# TODO: Move code below here to individual files when methods developed
# Get wood density 
s_all$wd <- getWoodDensity(
  genus = s_all$genus, 
  species = s_all$species,
  stand = s_all$plot_id,
  family = s_all$family,
  region = "SouthAmericaTrop")$meanWD

# Add plot mean coordinates to stem data
p_cent <- p %>% 
  st_centroid(.) %>% 
  st_transform(., 4326) %>% 
  bind_cols(., st_coordinates(.)) %>% 
  dplyr::select(
    plot_id,
    plot_centroid_x = X,
    plot_centroid_y = Y) %>% 
  st_drop_geometry()

s_c <- s_all %>% 
  left_join(., p_cent, by = "plot_id")

# Estimate stem biomass
s_c$agb <- computeAGB(
  D = s_c$diam,
  WD = s_c$wd,
  H = NULL,
  coord = st_drop_geometry(s_c[,c("plot_centroid_x", "plot_centroid_y")]),
  Dlim = 5)

# Summarise biomass to subplot level
s_summ <- s_c %>% 
  st_drop_geometry() %>% 
  left_join(., 
    st_drop_geometry(ps)[,c("plot_id", "subplot_id", "area")], 
    by = c("plot_id", "subplot_id")) %>% 
  mutate(ba = (pi * (diam/2)^2) / 10000) %>% 
  subplotSumm(., c("site_id", "plot_id", "subplot_id"), "area", "agb", "ba", "wd")

# Write summarised data to file
write.csv(s_summ, file.path(outdir, "subplot_summ.csv"), row.names = FALSE)
