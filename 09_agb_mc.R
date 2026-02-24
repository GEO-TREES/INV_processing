# Estimate AGB in quadrats
# John L. Godlee (johngodlee@gmail.com)  
# Last updated: 2025-06-18

# Define directories
# outdir <- "./dat/sites/PanamaCanal/09_agb_mc"

# Packages
library(dplyr)
library(sf)
library(BIOMASS)

# Source functions
source("./func.R")

# Import data
# stem_fil <- read.csv("./dat/sites/PanamaCanal/08_stem_fil/stem_fil.csv")
# plot_pt <- st_read("./dat/sites/PanamaCanal/01_fmt/plot_pt.gpkg")
# stem_pt <- st_read("./dat/sites/PanamaCanal/03_quad/stem_pt.gpkg")

# Extract plot centres
p_cent <- plot_pt %>% 
  group_by(site_id, plot_id) %>% 
  summarise() %>% 
  st_centroid() %>% 
  cbind(., st_coordinates(.)) %>% 
  st_drop_geometry() %>% 
  dplyr::select(plot_id, X, Y)

# Combine dataframes
stem_all <- stem_fil %>% 
  left_join(., p_cent, by = "plot_id")

# Split by quadrat
stem_split <- split(stem_all, 
  list(stem_all$quadrat_id, stem_all$census_id), 
  drop = TRUE, sep = "::")

# Define number of simulations
nsim <- 1000

# Run AGB MC error propagation
quad_length <- length(stem_split)
quad_agb_mc_list <- lapply(seq_along(stem_split), function(x) { 
  message(paste0(x, " / ", quad_length, " - ", names(stem_split)[x]))
  if (nrow(stem_split[[x]]) < 2) {
    list(
      "meanAGB" = stem_split[[x]]$agb_Mg,
      "medAGB" = stem_split[[x]]$agb_Mg,
      "sdAGB" = NA_real_,
      "credibilityAGB" = c("2.5%" = NA_real_, "97.5%" = NA_real_),
      "AGB_simu" = NA_real_)
  } else {
    AGBmonteCarlo(
      D = stem_split[[x]]$diam_cm,
      WD = stem_split[[x]]$meanWD,
      coord = stem_split[[x]][,c("X", "Y")],
      Dpropag = "chave2004",
      errWD = stem_split[[x]]$sdWD,
      n = nsim)
  }
})
names(quad_agb_mc_list) <- names(stem_split)

# Calculate mean and standard deviation of stem AGB
stem_agb_mc <- bind_rows(lapply(seq_along(quad_agb_mc_list), function(x) {
  simu <- quad_agb_mc_list[[x]]$AGB_simu
  if (is.matrix(simu)) {
    agb_Mg_mean <- apply(simu, 1, mean)
    agb_Mg_sd <- apply(simu, 1, sd)
  } else {
    agb_Mg_mean <- NA_real_
    agb_Mg_sd <- NA_real_
  }
  data.frame(record_id = stem_split[[x]]$record_id, agb_Mg_mean, agb_Mg_sd)
}))

# Write stem-level data to file
write.csv(stem_agb_mc, file.path(outdir, "stem_agb_mc.csv"), row.names = FALSE)

# Extract summary statistics from AGB MC error propagation simulations
quad_agb_mc_summ <- bind_rows(lapply(names(quad_agb_mc_list), function(x) { 
  data.frame(
    quadrat_id = gsub("::.*", "", x),
    census_id = gsub(".*::", "", x),
    agb_Mg_sum_mc_mean = quad_agb_mc_list[[x]]$meanAGB,
    agb_Mg_sum_mc_median = quad_agb_mc_list[[x]]$medAGB,
    agb_Mg_sum_mc_sd = quad_agb_mc_list[[x]]$sdAGB,
    agb_Mg_sum_mc_se = quad_agb_mc_list[[x]]$sdAGB / nsim,
    agb_Mg_sum_mc_ci2.5 = unname(quad_agb_mc_list[[x]]$credibilityAGB[1]),
    agb_Mg_sum_mc_ci97.5 = unname(quad_agb_mc_list[[x]]$credibilityAGB[2]))
}))

# Write quadrat-level summary data to file
write.csv(quad_agb_mc_summ, file.path(outdir, "quad_agb.csv"), row.names = FALSE)

