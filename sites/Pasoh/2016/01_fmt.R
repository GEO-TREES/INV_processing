# Clean Pasoh stem data
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2026-10-15

# Import stem data
load(file.path(indir, "pasoh.stem7.rdata"))  # TODO: replace with 2024 census
# pasoh.stem7

# Import species table
load(file.path(indir, "pasoh.spptable.rdata"))
# pasoh.spptable

# Import elevation raster 
elev <- rast(file.path(indir, "elev_rast_legacy.tif"))

# Translate stem codes into GEO-TREES stem codes, see ./templates/stem_codes.csv
# Stem status in `DFstatus`
status_map <- c(
  "alive" = "A",           # Alive
  "broken below" = "AT",   # Alive, broken below breast height
  "dead" = "D",            # Dead
  "no data" = "D")         # No data, `Status` is "D" for all of these stems

# ForestGEO codes, separated by ";" in `Codes`
# Pasoh doesn't use the standard ForestGEO dead codes (DC, DN, DS, DT), or
# Q and X for broken stems
code_map <- c(
  A = "",      # POM at alternative height, not 1.3 m
  B = "T",     # Standard ForestGEO is "large buttress", but at Pasoh all
               #   stems with B are "broken below" with no diameter
  C = "",      # POM changed since prior census
  D = "D",     # Dead. Not a standard ForestGEO code. Never combined with
               #   other codes, all stems are dead with no diameter
  I = "I",     # Stem irregular where measured
  L = "",      # Stem leaning
  M = "",      # Multiple stems
  P = "",      # Problem requiring further attention
  R = "",      # Resprout
  Y = "F",     # Stem prostrate. At Pasoh all stems are dead with no diameter
  # TODO: Pasoh-specific codes, not in the standard ForestGEO list. Ignored
  #   until definitions are found
  E = "",      # Mostly alive, with diameter
  H = "",      # All alive, with diameter
  K = "",      # All alive, with no diameter
  S = "",      # All alive, with diameter
  Z = "")      # Mostly alive, with diameter

# Format stem data
s_clean <- pasoh.stem7 %>% 
  left_join(., pasoh.spptable, by = c("SP" = "sp")) %>% 
  rename(
    subplot_id = Quadrat,
    tree_id = TreeID,
    stem_id = StemID,
    measurement_date = ExactDate,
    taxon_name = Latin,
    x_rel_m = PX,
    y_rel_m = PY,
    diam_cm = DBH,
    pom_m = HOM) %>% 
  mutate(
    site_id = param$site_id,
    plot_id = "Pasoh",
    acquisition_id = param$acquisition_id,
    measurement_date = decDate(na_if(measurement_date, "NULL")),
    measurement_date_res = ifelse(is.na(measurement_date), NA_character_, "YYYY-MM-DD"),
    tree_id = as.character(tree_id),
    stem_id = as.character(stem_id),
    diam_cm = as.numeric(diam_cm) / 10,
    x_rel_m = as.numeric(x_rel_m),
    y_rel_m = as.numeric(y_rel_m),
    pom_m = as.numeric(pom_m),
    pom_m = ifelse(pom_m == 0, NA_real_, pom_m),
    subplot_id = as.character(subplot_id),
    taxon_name = paste0(
      toupper(substr(tolower(taxon_name), 1, 1)), 
      substr(tolower(taxon_name), 2, nchar(taxon_name))
    ),
    alive = DFstatus %in% c("alive", "broken below"),
    code = codeCombine(
      codeMap(DFstatus, status_map, split = NA),
      codeMap(na_if(Codes, "NULL"), code_map, split = "[;, ]+"),
      standing = alive),
    # Stems broken below the POM with a diameter were measured below the
    # break, so are broken above the POM
    code = ifelse(grepl("T", code) & !is.na(diam_cm),
      codeCombine(gsub("T", "B", code)), code)) %>%
  mutate(record_id = row_number()) %>%
  colGen(., stem_cols$column_name, stem_cols$class) %>% 
  dplyr::select(all_of(stem_cols$column_name))

# Clean plot coordinates
plot_pt <- st_as_sfc(st_bbox(elev)) %>% 
  st_cast(., "POINT") %>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  slice_head(n = 4) %>% 
  rename(
    rover_easting_utm_m = X,
    rover_northing_utm_m = Y) %>% 
  mutate(
    site_id = param$site_id,
    plot_id = "Pasoh",
    point_id = c("SW", "SE", "NE", "NW"),
    corner = TRUE,
    x_rel_m = case_when(
      point_id == "SW" ~ 0,
      point_id == "SE" ~ 1000,
      point_id == "NE" ~ 1000,
      point_id == "NW" ~ 0,
      TRUE ~ NA_real_),
    y_rel_m = case_when(
      point_id == "SW" ~ 0,
      point_id == "SE" ~ 0,
      point_id == "NE" ~ 500,
      point_id == "NW" ~ 500,
      TRUE ~ NA_real_),
    crs_epsg = as.integer(32648),
    crs_name = "UTM 48N") %>% 
  colGen(., pt_cols$column_name, pt_cols$class) %>% 
  dplyr::select(all_of(pt_cols$column_name))


# Clean plot meta-data
plot_meta <- data.frame(
  site_id = param$site_id,
  acquisition_id = param$acquisition_id,
  plot_id = "Pasoh",
  plot_planar = TRUE,
  plot_length_m = 1000,
  plot_width_m = 500,
  meas_diam_min_cm = 10,
  meas_pom_default_m = 1.3,
  meas_tree_stem = TRUE,
  meas_tree_group = TRUE,
  meas_dead = FALSE,
  meas_fallen = FALSE,
  meas_liana = NA,
  meas_palm = NA,
  meas_bamboo = NA,
  meas_plot_loc = NA_character_,
  meas_stem_loc = "ForestGEO protocol. Well-surveyed 10x10 m subplot grid. Tape measures to locate stems by X and Y coordinates.",
  meas_protocol = NA_character_,
  notes_meas = NA_character_,
  forest_status = "old-growth",
  vegetation_type = NA_character_,
  land_use = NA_character_,
  treatment = NA_character_,
  fire_regime = NA_character_,
  cyclone_regime = NA_character_,
  flood_regime = NA_character_,
  earth_regime = NA_character_,
  herbivory_regime = NA_character_,
  notes_disturbance = NA_character_,
  notes_plot = NA_character_) %>% 
  dplyr::select(all_of(plot_cols$column_name))

# Check all columns in output objects
colCheck(plot_meta, plot_cols)
colCheck(plot_pt, pt_cols)
colCheck(s_clean, stem_cols)

# Check values
valCheck(
  plot = plot_meta,
  stem = s_clean,
  pt = plot_pt)

# Write corner points to file
write.csv(plot_pt, file.path(outdir, "plot_pt.csv"), row.names = FALSE)

# Write plot meta-data to file
write.csv(plot_meta, file.path(outdir, "plot.csv"), row.names = FALSE)

# Write data to file
write.csv(s_clean, file.path(outdir, "stem.csv"), row.names = FALSE)
