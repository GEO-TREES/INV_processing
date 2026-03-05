# Clean Misiones plot polygons data
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2025-07-09

# Import stem data 
s <- read_excel(file.path(indir, "Medicion2024 planilla compartida GEO-TREES.xlsx"), sheet = 2, guess_max = Inf)
sp <- read_excel(file.path(indir, "species.xlsx"), sheet = 1, guess_max = Inf)
recs <- read_excel(file.path(indir, "Medicion2024 planilla compartida GEO-TREES.xlsx"), sheet = 3, guess_max = Inf)

# Extract plot corners
polys <- st_read(file.path(indir, "parcela_ubicacion_campo_2026-02-24.kml")) %>% 
  mutate(
    site_id = param$site_id,
    plot_id = as.character(Name)) %>% 
  st_transform(., 4326)
  
corner_id_list <- rep(list(c("SW", "NW", "NE", "SE")), nrow(polys))
pt <- bind_rows(lapply(seq_len(nrow(polys)), function(i) {
  # Isolate polygon
  xsel <- polys[i,]

  # Extract corner coordinates
  xc <- as.data.frame(sf::st_coordinates(sf::st_union(xsel)))

  xc$sum <- xc$X + xc$Y
  xc$diff <- xc$X - xc$Y
  xc$corner_id <- NA_character_
  xc$corner_id[which.min(xc$diff)] <- "NW" 
  xc$corner_id[which.min(xc$sum)] <- "SW" 
  xc$corner_id[which.max(xc$diff)] <- "SE" 
  xc$corner_id[which.max(xc$sum)] <- "NE" 

  # Select chosen corner coordinate(s)
  xs <- xc[
    xc$corner_id %in% sort(corner_id_list[[i]]) & !is.na(xc$corner),
    c("X", "Y", "corner_id")]

  # Return selected corner coordinate(s)
  g <- st_sfc(lapply(1:nrow(xs), function(j) {
      st_point(as.matrix(xs[j,1:2]))
    }), crs = st_crs(polys))
  d <- st_drop_geometry(polys[rep(i, nrow(xs)), "plot_id"])
  d$corner_id <- xs$corner_id
  st_sf(d, geometry = g)
})) %>% 
  mutate(
    site_id = param$site_id,
    x_rel_m = case_when(
      corner_id %in% c("SW", "NW") ~ 0,
      corner_id %in% c("SE", "NE") ~ 100,
      TRUE ~ NA_real_),
    y_rel_m = case_when(
      corner_id %in% c("SW", "SE") ~ 0,
      corner_id %in% c("NW", "NE") ~ 100,
      TRUE ~ NA_real_)) %>% 
  dplyr::select(all_of(pt_cols$column_name))

# Clean stem data
s_clean <- s %>% 
  rename(
    plot_id = parcela,
    stem_id = id_fuste,
    measurement_date = anio_medicion,
    height_m = h_24,
    alive = `vivo/muerto`,
    taxon_name = sp_cod,
    diam_cm = cap_24,
    x_rel_m = x_per,
    y_rel_m = y_long,
    pom_m = pom,
    notes = observaciones_campo_2024) %>% 
  mutate(
    site_id = param$site_id,
    plot_id = as.character(plot_id),
    tree_id = NA_character_,
    diam_cm = case_when(
      diam_cm == "Muerto" ~ NA_real_,
      TRUE ~ as.numeric(diam_cm)),
    diam_cm = diam_cm / pi,
    pom_m = as.numeric(pom_m),
    alive = ifelse(alive == 1, "D", "A"),
    subplot_id = paste0(bloque, lado),
    fallen = ifelse(grepl("C", danio_24), "F", "S"),
    missing = "",
    broken = ifelse(grepl("B|R", danio_24), "B", ""),
    stump = "",
    code = pasteVals(alive, fallen, broken, missing, stump),
    census_id = as.integer(1),
    measurement_date = as.character(measurement_date),
    x_rel_m = case_when(
      x_rel_m == "1,2" ~ 12,
      TRUE ~ as.numeric(x_rel_m)),
    y_rel_m = as.numeric(y_rel_m),
    agb_allometry = NA_character_) %>%  # TODO:
  mutate(
    x_block_origin = (bloque - 1) * 20,
    x_transect = x_block_origin + 10,
    x_rel_m = ifelse(lado == "I", x_transect - x_rel_m, x_transect + x_rel_m)) %>% 
  group_by(plot_id, census_id, stem_id) %>% 
  mutate(measurement_id = row_number()) %>% 
  ungroup() %>% 
  dplyr::select(any_of(stem_cols$column_name))

# Clean recruits table
recs_clean <- recs %>% 
  rename(
    plot_id = parcela,
    stem_id = id_fuste,
    height_m = h_24,
    alive = `vivo/muerto`,
    taxon_name = sp_cod,
    diam_cm = cap_24,
    x_rel_m = x_per,
    y_rel_m = y_long,
    pom_m = pom,
    notes = observaciones_campo_2024) %>% 
  mutate(
    site_id = param$site_id,
    plot_id = as.character(plot_id),
    tree_id = NA_character_,
    diam_cm = as.numeric(diam_cm) / pi,
    pom_m = as.numeric(pom_m),
    alive = ifelse(alive == 1, "D", "A"),
    subplot_id = paste0(bloque, lado),
    fallen = ifelse(grepl("C", danio_24), "F", "S"),
    missing = "",
    broken = ifelse(grepl("B|R", danio_24), "B", ""),
    stump = "",
    code = pasteVals(alive, fallen, broken, missing, stump),
    census_id = as.integer(1),
    measurement_date = "2024",
    height_m = as.numeric(height_m),
    x_rel_m = case_when(
      x_rel_m == "1,2" ~ 12,
      TRUE ~ as.numeric(x_rel_m)),
    y_rel_m = as.numeric(y_rel_m),
    agb_allometry = NA_character_) %>% 
  group_by(plot_id, census_id, stem_id) %>% 
  mutate(measurement_id = row_number()) %>% 
  ungroup() %>% 
  dplyr::select(any_of(stem_cols$column_name))

s_all <- bind_rows(s_clean, recs_clean) %>% 
  mutate(record_id = row_number()) %>% 
  mutate(
    taxon_name = case_when(
    taxon_name == "AN" ~ "AR",
    taxon_name == "Ca" ~ "CA",
    taxon_name == "camboata blanco" ~ "CB",
    taxon_name == "Ga" ~ "GA",
    taxon_name == "Gr" ~ "GR",
    taxon_name == "Gy" ~ "GY",
    taxon_name == "MUERTO" ~ NA_character_,
    taxon_name == "myrcine" ~ "Myrsine",
    taxon_name == "Pereskia aculeata" ~ "Pereskia aculeata",
    taxon_name == "SY" ~ "SYM",
    taxon_name == "bauhinia" ~ "PB",
    taxon_name == "Bauhinia" ~ "PB",
    taxon_name == "TC" ~ "Tabe.cath",
    taxon_name == "ZP" ~ "PZ",
    taxon_name == "Apoyante" ~ NA_character_,
    taxon_name == "Sola1" ~ "Solanum",
    taxon_name == "LIANA" ~ NA_character_,
    taxon_name == "Liana" ~ NA_character_,
    TRUE ~ taxon_name)) %>% 
  left_join(., sp[,c("cod_sp", "accepted_name", "life_form")], by = c("taxon_name" = "cod_sp")) %>% 
  mutate(taxon_name = ifelse(is.na(accepted_name), taxon_name, accepted_name)) %>% 
  filter(plot_id %in% pt$plot_id) %>% 
  dplyr::select(all_of(stem_cols$column_name)) 

# Create census table
census <- polys %>% 
  st_drop_geometry() %>% 
  mutate(
    census_id = as.integer(1),
    census_date = unique(s_all$measurement_date),
    measurement_date_min = census_date,
    measurement_date_max = census_date) %>% 
  dplyr::select(all_of(census_cols$column_name))

# Create plots table
plots <- census %>% 
  rename(census_date_all = census_date) %>% 
  mutate(
    census_date_geotrees = census_date_all,
    plot_width_m = 100,
    plot_length_m = 100,
    plot_slope_deg = NA_real_,
    plot_aspect_deg = NA_real_,
    plot_elevation_m = NA_real_,
    plot_planar = FALSE,
    notes_plot = NA_character_,
    meas_diam_min_cm = 10,
    meas_pom_default_m = 1.3,
    meas_tree_stem = TRUE,
    meas_tree_group = FALSE,
    meas_dead = FALSE,
    meas_fallen = TRUE,
    meas_liana = TRUE,
    meas_palm = TRUE,
    meas_bamboo = NA,
    meas_protocol = "https://doi.org/10.1016/j.foreco.2022.120290",
    notes_meas = NA_character_,
    forest_status = "Secondary",
    land_use = NA_character_,
    treatment = NA_character_,  # TODO:
    treatment_ref = "https://doi.org/10.1016/j.foreco.2022.120290",
    fire_regime = NA_character_,
    cyclone_regime = NA_character_,
    flood_regime = NA_character_,
    earth_regime = NA_character_,
    herbivory_regime = NA_character_,
    notes_disturbance = NA_character_) %>% 
  dplyr::select(all_of(plot_cols$column_name))

# Check all columns in output objects
colCheck(plots, plot_cols)
colCheck(pt, pt_cols)
colCheck(census, census_cols)
colCheck(s_all, stem_cols)

# Check values
plotValCheck(plots)
ptValCheck(pt)
censusValCheck(census)
stemValCheck(s_all)

# Write corner points to file
st_write(pt, file.path(outdir, "plot_pt.gpkg"), delete_dsn = TRUE)

# Write census meta-data to file
write.csv(census, file.path(outdir, "census.csv"), row.names = FALSE)

# Write plot meta-data to file
write.csv(plots, file.path(outdir, "plot.csv"), row.names = FALSE)

# Write stem data to file
write.csv(s_all, file.path(outdir, "stem.csv"), row.names = FALSE)

