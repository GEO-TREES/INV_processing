# Estimate AGB in quadrats
# John L. Godlee (johngodlee@gmail.com)  
# Last updated: 2025-06-18

# Filter stems data
stem_fil <- stem_pt %>% 
  filter(record_id %in% record_fil) %>% 
  left_join(., stem, by = "record_id") %>% 
  left_join(., stem_wd, by = "record_id") %>% 
  left_join(., stem_height, by = "record_id") %>% 
  left_join(., stem_agb, by = "record_id") %>% 
  bind_cols(., st_coordinates(.)) %>% 
  mutate(
    plot_ID = plot_id,
    subplot_ID = quadrat_id,
    x_rel = x_rel_m,
    y_rel = y_rel_m,
    x_proj = x_utm,
    y_proj = y_utm) %>% 
  st_drop_geometry()

# Format quadrat corner points
quad_pt_clean <- quad_pt %>% 
  st_drop_geometry() %>% 
  dplyr::select(
    plot_ID = plot_id,
    subplot_ID = quadrat_id,
    x_rel = x_rel_m,
    y_rel = y_rel_m,
    x_proj = x_utm,
    y_proj = y_utm)

# Split by quadrat
stem_split <- split(stem_fil, stem_fil$quadrat_id)

# Define number of simulations
nsim <- 1000

# For each quadrat
quad_length <- length(stem_split)
quad_agb_mc_list <- lapply(seq_along(stem_split), function(x) { 
  message(paste0(x, " / ", quad_length, " - ", names(stem_split)[x]))
  if (nrow(stem_split[[x]]) < 2) {
    out <- list(
      "meanAGB" = stem_split[[x]]$agb_Mg,
      "medAGB" = stem_split[[x]]$agb_Mg,
      "sdAGB" = NA_real_,
      "credibilityAGB" = c("2.5%" = NA_real_, "97.5%" = NA_real_),
      "AGB_simu" = NA_real_)

    simu_all <- data.frame()
  } else {
    # Run AGB MC error propagation
    out <- AGBmonteCarlo(
      D = stem_split[[x]]$diam_cm,
      WD = stem_split[[x]]$meanWD,
      errWD = stem_split[[x]]$sdWD,
      H = if (param$height_method == "regional") stem_split[[x]]$height_m_pred else NULL,
      errH = if (param$height_method == "regional") stem_split[[x]]$height_m_pred_rse else NULL,
      HDmodel = if (param$height_method == "field") height_mod else NULL,
      coord = NULL,
      Dpropag = "chave2004",
      n = nsim,
      Carbon = FALSE,
      Dlim = NULL)

    # Extract individual simulations of AGBD 
    plot_divide_tmp <- list(
      sub_corner_coord = quad_pt_clean[
        quad_pt_clean$subplot_ID == unique(stem_split[[x]]$quadrat_id),],
      tree_data = stem_split[[x]])

    simu_all <- as.data.frame(subplot_summary(
      subplots = plot_divide_tmp, 
      value = "agb_Mg",
      AGB_simu = out$AGB_simu,
      draw_plot = FALSE,
      per_ha = FALSE,
      fun = sum)$long_AGB_simu) %>% 
      dplyr::select(
        quadrat_id = subplot_ID,
        sim = N_simu,
        sumAGB = AGBD)
  }
  return(list(out, simu_all))
})
names(quad_agb_mc_list) <- names(stem_split)

# Create dataframe of all simulations
quad_agb_simu <- bind_rows(lapply(quad_agb_mc_list, "[[", 2))

# Write quadrat simulations to file
write.csv(quad_agb_simu, file.path(outdir, "quad_agb_mc.csv"), row.names = FALSE)

# Calculate mean and standard deviation of stem AGB
stem_agb_mc <- bind_rows(lapply(seq_along(quad_agb_mc_list), function(x) {
  simu <- quad_agb_mc_list[[x]][[1]]$AGB_simu
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
    quadrat_id = x,
    agb_Mg_sum_mc_mean = quad_agb_mc_list[[x]][[1]]$meanAGB,
    agb_Mg_sum_mc_median = quad_agb_mc_list[[x]][[1]]$medAGB,
    agb_Mg_sum_mc_sd = quad_agb_mc_list[[x]][[1]]$sdAGB,
    agb_Mg_sum_mc_se = quad_agb_mc_list[[x]][[1]]$sdAGB / sqrt(nsim),
    agb_Mg_sum_mc_ci2.5 = unname(quad_agb_mc_list[[x]][[1]]$credibilityAGB[1]),
    agb_Mg_sum_mc_ci97.5 = unname(quad_agb_mc_list[[x]][[1]]$credibilityAGB[2]))
}))

# Write quadrat-level summary data to file
write.csv(quad_agb_mc_summ, file.path(outdir, "quad_agb.csv"), row.names = FALSE)

