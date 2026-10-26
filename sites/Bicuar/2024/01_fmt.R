# Clean Bicuar tree inventory data
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2026-10-22

# Import data
s <- read.csv(file.path(indir, "stems.csv"))
p <- read.csv(file.path(indir, "plots.csv"))
wd <- read.csv(file.path(indir, "wood_density_raw.csv"))
plot_corners <- read_sf(file.path(indir, "plot_corners.shp"))

# Process plot corners
pt <- plot_corners %>% 
  mutate(
    corner = TRUE,
    site_id = param$site_id,
    acquisition_id = param$acquisition_id,
    point_id = gsub(".*[0-9]+", "", name)) %>%
  rename(plot_id = plot_name) %>% 
  mutate(
    x_rel_m = case_when(
      point_id %in% c("SW", "NW") ~ 0,
      point_id %in% c("SE", "NE") ~ 100,
      TRUE ~ NA_real_),
    y_rel_m = case_when(
      point_id %in% c("SW", "SE") ~ 0,
      point_id %in% c("NW", "NE") ~ 100,
      TRUE ~ NA_real_)) %>% 
  filter(plot_id != "P1")

ptc <- st_coordinates(pt) 
crs <- unique(utmZone(ptc[,1], ptc[,2], epsg = TRUE))
crs_name <- unique(utmZone(ptc[,1], ptc[,2], epsg = FALSE))

pt_clean <- pt %>% 
  filter(plot_id != "P16") %>% 
  st_transform(., crs = crs) %>% 
  bind_cols(., st_coordinates(.)) %>% 
  rename(
    rover_easting_utm_m = X,
    rover_northing_utm_m = Y) %>% 
  mutate(
    crs_epsg = as.integer(crs),
    crs_name = "UTM 33S",
    rover_model = "Garmin GPSMap 65s") %>% 
  colGen(., pt_cols$column_name, pt_cols$class) %>% 
  st_drop_geometry() %>% 
  dplyr::select(all_of(pt_cols$column_name))

# Prepare stem data 
s_clean <- s %>% 
  left_join(., unique(p[,c("plot_id", "plot_name")]), by = "plot_id") %>% 
  group_by(plot_name, stem_id) %>% 
  arrange(census_date) %>% 
  fill(x_grid, y_grid, subplot_id, .direction = "downup") %>% 
  ungroup() %>% 
  dplyr::select(-plot_id) %>% 
  rename(
    measurement_date = census_date,
    plot_id = plot_name,
    x_rel_m = x_grid,
    y_rel_m = y_grid,
    diam_cm = diam,
    pom_m = pom,
    height_m = height, 
    taxon_name = species_name_clean,
    notes = notes_stem) %>% 
  mutate(
    acquisition_id = param$acquisition_id,
    site_id = param$site_id,
    measurement_date = decDate(measurement_date),
    measurement_date_res = "YYYY-MM-DD",
    alive = ifelse(stem_status %in% c("a", "r"), "A", "D"),
    broken = ifelse(grepl("b|p", stem_mode), "B", ""),
    fallen = ifelse(grepl("f", stem_mode), "F", "S"),
    missing = ifelse(grepl("v|q", stem_mode), "M", ""),
    stump = ifelse(grepl("t", stem_mode), "T", ""),
    code = pasteVals(alive, broken, fallen, missing, stump),
    growth_form = NA_character_,
    height_allometry = NA_character_,
    agb_allometry = NA_character_,
    subplot_id = as.character(subplot_id)) %>% 
  mutate(
    year = as.character(floor(measurement_date)),
    census_id = case_when(
      plot_id %in% paste0("P", 1:4) & year == "2018" ~ "1",
      plot_id %in% paste0("P", 2:4) & year == "2021" ~ "2",
      plot_id %in% paste0("P", 2:4) & year == "2024" ~ "3",
      plot_id %in% paste0("P", 5:15) & year == "2019" ~ "1",
      plot_id %in% paste0("P", 5:15) & year == "2021" ~ "2",
      plot_id %in% paste0("P", 5:15) & year == "2024" ~ "3",
      plot_id %in% paste0("P", 16) & year == "2021" ~ "1",
      plot_id %in% paste0("P", 16) & year == "2024" ~ "2",
      plot_id %in% paste0("M", 1:3) & year == "2022" ~ "1",
      plot_id %in% paste0("M", 1:3) & year == "2024" ~ "2",
      plot_id %in% paste0("O", 1:2) & year == "2024" ~ "1",
      plot_id %in% paste0("B", 1:2) & year == "2024" ~ "1",
      TRUE ~ NA_character_)) %>% 
  group_by(plot_id, census_id) %>% 
  mutate(census_date = as.character(median(as.Date(measurement_date)))) %>% 
  ungroup() %>% 
  group_by(plot_id, stem_id) %>% 
  mutate(measurement_id = row_number()) %>% 
  ungroup() 

s_out <- s_clean %>% 
  mutate(record_id = row_number()) %>% 
  filter(
    plot_id != "P16",
    measurement_date >= 2024) %>% 
  dplyr::select(all_of(stem_cols$column_name))

# Create optional height measurements table
s_height <- s_clean %>% 
  filter(
    !is.na(diam_cm), !is.na(height_m),
    !grepl("D", code),
    !grepl("B", code),
    !grepl("F", code),
    !grepl("M", code),
    !grepl("T", code)) %>% 
dplyr::select(all_of(height_cols$column_name))
  
# Create plots table
plots <- p %>% 
  dplyr::select(-plot_id) %>% 
  rename(plot_id = plot_name) %>% 
  mutate(site_id = param$site_id) %>% 
  filter(
    grepl("2024", census_date),
    !plot_id %in% c("P1", "P16")) %>% 
  mutate(
    acquisition_id = param$acquisition_id,
    census_id = "3",
    plot_width_m = 100,
    plot_length_m = 100,
    plot_slope_deg = NA_real_,
    plot_aspect_deg = NA_real_,
    plot_elevation_m = NA_real_,
    plot_planar = FALSE,
    notes_plot = NA_character_,
    meas_diam_min_cm = 5,
    meas_pom_default_m = 1.3,
    meas_tree_stem = TRUE,
    meas_tree_group = TRUE,
    meas_dead = TRUE,
    meas_fallen = TRUE,
    meas_liana = NA,
    meas_palm = NA,
    meas_bamboo = NA,
    meas_plot_loc = NA_character_,
    meas_stem_loc = NA_character_,
    meas_protocol = "SEOSAW_v3.6",
    notes_meas = NA_character_,
    forest_status = NA_character_,
    vegetation_type = NA_character_,
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

# Process wood density data
wd_clean <- wd %>% 
  mutate(site_id = param$site_id) %>% 
  rename(
    taxon_name = species,
    wood_density_gcm3 = WD) %>% 
  mutate(
    wood_density_sd_gcm3 = NA_real_,
    wood_density_n = as.integer(NA_real_)) %>%
  filter(!is.na(taxon_name), !is.na(wood_density_gcm3)) %>% 
  dplyr::select(all_of(wd_cols$column_name))

# Infer field subplot bounds from stem coordinates
# Subplots are cells of a regular grid in each plot, 100x10 m in B1, 100x20 m
# in B2, 20x100 m in O1, 50x20 m in M2, M3 and some P plots, and 20x50 m in
# the other P plots, but numbered differently between plots.
# For each plot, grids are made from each combination of round number cell
# sizes that divide the plot, with at least as many cells as subplots. Each
# subplot is assigned to a different cell, in order of the number of its
# stems in that cell. The grid where the most stems fall within the cell of
# their subplot is chosen, or the grid with fewer cells if two fit equally
cell_m <- c(5, 10, 20, 25, 50, 100)
s_xy <- s_out %>%
  filter(!is.na(subplot_id), !is.na(x_rel_m), !is.na(y_rel_m))
subplot_fit <- lapply(split(s_xy, s_xy$plot_id), function(s) {
  pl <- plots[plots$plot_id == s$plot_id[1],]
  sub <- unique(s$subplot_id)
  cand <- expand.grid(
    wx = cell_m[pl$plot_width_m %% cell_m == 0],
    wy = cell_m[pl$plot_length_m %% cell_m == 0])
  cand$n_cell <- (pl$plot_width_m / cand$wx) * (pl$plot_length_m / cand$wy)
  cand <- cand[cand$n_cell >= length(sub),]
  fits <- lapply(seq_len(nrow(cand)), function(i) {
    nx <- pl$plot_width_m / cand$wx[i]
    cx <- floor(s$x_rel_m / cand$wx[i])
    cy <- floor(s$y_rel_m / cand$wy[i])
    inside <- cx >= 0 & cx < nx & cy >= 0 &
      cy < pl$plot_length_m / cand$wy[i]
    cell <- ifelse(inside, cy * nx + cx, NA)
    # Count stems of each subplot in each cell, then assign subplots to cells
    # greedily, starting with the largest count
    tab <- table(factor(s$subplot_id[inside], levels = sub), cell[inside])
    assign <- setNames(rep(NA_real_, length(sub)), sub)
    while (length(tab) > 0 && max(tab) > 0) {
      m <- which(tab == max(tab), arr.ind = TRUE)[1,]
      assign[rownames(tab)[m[1]]] <- as.numeric(colnames(tab)[m[2]])
      tab <- tab[-m[1], -m[2], drop = FALSE]
    }
    list(assign = assign,
      n_in = sum(cell == assign[s$subplot_id], na.rm = TRUE))
  })
  best <- order(-vapply(fits, `[[`, numeric(1), "n_in"), cand$n_cell)[1]
  assign <- fits[[best]]$assign
  wx <- cand$wx[best]
  wy <- cand$wy[best]
  nx <- pl$plot_width_m / wx
  res <- data.frame(
    plot_id = s$plot_id[1],
    subplot_id = sub,
    x_rel_min_m = (assign %% nx) * wx,
    x_rel_max_m = (assign %% nx) * wx + wx,
    y_rel_min_m = (assign %/% nx) * wy,
    y_rel_max_m = (assign %/% nx) * wy + wy)
  # Percentage of stems within the bounds of their subplot
  i <- match(s$subplot_id, res$subplot_id)
  s_in <- s$x_rel_m >= res$x_rel_min_m[i] & s$x_rel_m <= res$x_rel_max_m[i] &
    s$y_rel_m >= res$y_rel_min_m[i] & s$y_rel_m <= res$y_rel_max_m[i]
  res$n_stem <- as.vector(table(factor(s$subplot_id, levels = sub)))
  res$pct_in <- 100 * as.vector(tapply(s_in %in% TRUE,
    factor(s$subplot_id, levels = sub), mean))
  res
}) %>%
  bind_rows() %>%
  arrange(plot_id, as.numeric(subplot_id))

msgTable(subplot_fit[subplot_fit$pct_in < 90,],
  "Field subplots where fewer than 90% of stems are within the inferred bounds:")

subplot <- subplot_fit %>% 
  mutate(site_id = param$site_id) %>% 
  dplyr::select(all_of(subplot_cols$column_name))

# Check all columns in output objects
colCheck(plots, plot_cols)
colCheck(pt_clean, pt_cols)
colCheck(s_out, stem_cols)
colCheck(wd_clean, wd_cols)
colCheck(s_height, height_cols)
colCheck(subplot, subplot_cols)

# Check values
valCheck(
  plot = plots, 
  stem = s_out, 
  wd = wd_clean,
  pt = pt_clean,
  height = s_height,
  subplot = subplot)

# Write corner points to file
write.csv(pt_clean, file.path(outdir, "plot_pt.csv"), row.names = FALSE)

# Write plot meta-data to file
write.csv(plots, file.path(outdir, "plot.csv"), row.names = FALSE)

# Write stem data to file
write.csv(s_out, file.path(outdir, "stem.csv"), row.names = FALSE)

# Write height data to file
write.csv(s_height, file.path(outdir, "height.csv"), row.names = FALSE)

# Write wood density data to file
write.csv(wd_clean, file.path(outdir, "wd.csv"), row.names = FALSE)

# Write field subplot bounds to file
write.csv(subplot, file.path(outdir, "subplot.csv"), row.names = FALSE)

