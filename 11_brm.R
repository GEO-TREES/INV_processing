# Prepare L2 and L3 data products 
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2026-02-16

# Prepare L1 stems dataset
L1_stem <- stem_summ %>% 
  bind_cols(., st_coordinates(.)) %>% 
  st_drop_geometry() %>% 
  dplyr::select(
    BRM_site = site_id,
    Acquisition = acquisition_id,
    Plot_name = plot_id,
    Tree_label = tree_id,
    Stem_label = stem_id,
    Botany_genus = genusAccepted,
    Botany_species = speciesAccepted,
    Botany_author = authorAccepted,
    Botany_family = familyAccepted,
    Position_x = x_rel_m,
    Position_y = y_rel_m,
    Longitude = X,
    Latitude = Y)

# Prepare L1 plot GNSS points dataset 
L1_pt <- plot_pt %>% 
  dplyr::select(
    BRM_site = site_id,
    Acquisition = acquisition_id,
    Plot_name = plot_id,
    Point_label = point_id,
    Position_x = x_rel_m,
    Position_y = y_rel_m)

# Prepare L2 stems dataset
L2_stem <- stem_fil %>% 
  left_join(., stem_agb_mc, by = "record_id") %>% 
  mutate(
    Tree_label = pasteVals(tree_id, stem_id, sep = ":"),
    agb_Mg_mean = ifelse(is.na(agb_Mg_mean), agb_Mg, agb_Mg_mean)) %>% 
  dplyr::select(
    BRM_site = site_id,
    Acquisition = acquisition_id,
    Plot_name = plot_id,
    Tree_label,
    Longitude = longitude,
    Latitude = latitude,
    AGB_tree_estimate = agb_Mg_mean,
    AGB_tree_uncertainty = agb_Mg_sd,
    Height_tree_estimate = height_m_pred)#,
    # TODO: Height_tree_uncertainty = )

# Check all values filled
stopifnot(all(!is.na(L2_stem$AGB_tree_estimate)))
# stopifnot(all(!is.na(L2_stem$AGB_tree_uncertainty)))

# Prepare L2 plot polygons dataset
L2_poly <- plot_poly %>% 
  dplyr::select(
    BRM_site = site_id,
    Acquisition = acquisition_id,
    Plot_name = plot_id)

# Prepare L3 dataset
L3_quad <- quad_summ %>% 
  dplyr::select(
    BRM_site = site_id,
    Acquisition = acquisition_id,
    Plot_name = plot_id,
    Quadrat_name = quadrat_id,
    AGBD_stand_estimate = agb_Mg_sum_mc_mean_ha,
    AGBD_stand_uncertainty = agb_Mg_sum_mc_sd_ha,
    Height_stand_estimate = height_m_pred_max,
    # Height_stand_uncertainty = # TODO: Canopy height standard deviation, reporting the L2 uncertainty propagated to stand-level estimate. 
    Basal_area = ba_m2_sum_ha,
    Lorey_height = lorey_height_m,
    Wood_density = meanWD_wm_ba) 

# Check all values filled
stopifnot(all(!is.na(L3_quad$AGBD_stand_estimate)))
# stopifnot(all(!is.na(L3_quad$AGBD_stand_uncertainty)))

# Construct output filenames
L1_filename <- paste(
    param$site_id, 
    "PDA", 
    param$acquisition_id, 
    "L1", 
    software_version_sanit, 
  sep = "_")

L2_filename <- paste(
    param$site_id, 
    "PDA", 
    param$acquisition_id, 
    "L2", 
    software_version_sanit, 
  sep = "_")

L3_filename <- paste(
    param$site_id, 
    "PDA", 
    param$acquisition_id, 
    "L3", 
    software_version_sanit, 
  sep = "_")

# Write L1 dataset to file
write.csv(L1_stem, file.path(L_dir_list[["L1"]], paste0(L1_filename, "_stem", ".csv")), row.names = FALSE)
st_write(L1_pt , file.path(L_dir_list[["L1"]], paste0(L1_filename, "_pt", ".gpkg")), delete_dsn = TRUE) 

# Write L2 dataset to file
write.csv(L2_stem, file.path(L_dir_list[["L2"]], paste0(L2_filename, "_stem", ".csv")), row.names = FALSE)
st_write(L2_poly, file.path(L_dir_list[["L2"]], paste0(L2_filename, "_poly", ".gpkg")), delete_dsn = TRUE)

# Write L3 dataset to file
st_write(L3_quad, file.path(L_dir_list[["L3"]], paste0(L3_filename, "_quad", ".gpkg")), delete_dsn = TRUE)

# Construct RO-crates

# Author 
me <- entity(
  x = "#john-godlee",
  type = "Person",
  name = "John L. Godlee",
  email = "GodleeJ@si.edu"
)

# Affiliation
aff <- entity(
  x = "#si-org",
  type = "Organization",
  name = "Smithsonian Institution"
)

# License
lic <- entity(
  x = "TERMS.md",
  type = c("File", "CreativeWork") 
)

# Input directory
indir <- entity(
  x = paste0(param$raw_dir, "/"),
  type = "Dataset",
  description = "Directory containing raw L0 data."
)

# Output files
L1_stem_outfile <- entity(
  x = file.path(L_dir_list[["L1"]], paste0(L1_filename, "_stem", ".csv")),
  type = "File",
  description = "L1 re-formatted stem measurements.",
  encodingFormat = "text/csv"
)

L1_pt_outfile <- entity(
  x = file.path(L_dir_list[["L1"]], paste0(L1_filename, "_pt", ".gpkg")),
  type = "File",
  description = "L1 re-formatted point geo-location measurements.",
  encodingFormat = "text/csv"
)

L2_stem_outfile <- entity(
  x = file.path(L_dir_list[["L2"]], paste0(L2_filename, "_stem", ".csv")),
  type = "File",
  description = "L2 stem AGB estimates.",
  encodingFormat = "text/csv"
)

L2_poly_outfile <- entity(
  x = file.path(L_dir_list[["L2"]], paste0(L2_filename, "_poly", ".gpkg")),
  type = "File",
  description = "L2 plot polygons.",
  encodingFormat = "application/geopackage+sqlite3"
)

L3_quad_outfile <- entity(
  x = file.path(L_dir_list[["L3"]], paste0(L3_filename, "_quad", ".gpkg")),
  type = "File",
  description = "L3 AGBD estimates within plot quadrats.",
  encodingFormat = "application/geopackage+sqlite3"
)

# Parameters file
yaml <- entity(
  x = "param.yaml",
  type = "File",
  description = "Configuration parameters.",
  encodingFormat = "application/yaml"
)

# Software 
code <- entity(
  x = paste0("#PDA_processing_", param$software_version),
  type = c("SoftwareApplication", "SoftwareSourceCode"),
  name = "GEO-TREES AGBD processing pipeline",
  version = param$software_version,
  url = paste0("https://github.com/GEO-TREES/PDA_processing/releases/tag/", param$software_version),
  programmingLanguage = "R"
)

# Execution action
exec <- entity(
  x = "#run-tree_inventory_pipeline",
  type = "CreateAction",
  name = "Tree inventory pipeline execution",
  description = "Execute tree inventory pipeline to generate data products",
  endTime = strftime(Sys.time(), format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
)

L_outfile_list <- list(
  "L1" = list(
    "L1_stem" = L1_stem_outfile,
    "L1_pt" = L1_pt_outfile
  ),
  "L2" = list(
    "L2_stem" = L2_stem_outfile, 
    "L2_poly" = L2_poly_outfile
  ),
  "L3" = list(
    "L3_quad" = L3_quad_outfile
  )
)

# Initialize crates, add entities and relationships
rocrate_list <- lapply(names(L_outfile_list), function(x) { 
  rc <- rocrate(
    context = "https://w3id.org/ro/crate/1.2/context",
    datePublished = as.character(Sys.Date()),
    name = paste("GEO-TREES Bicuar PDA", x)
  ) |> 
    add_entity(me) |>
    add_entity(aff) |>
    add_entity(lic) |>
    add_entity(indir) |>
    add_entity(yaml) |>
    add_entity(code) |>
    add_entity(exec)

  for (i in L_outfile_list[[x]]) {
    rc <- rc |> add_entity(i)
  }

  L_outfile_id_list <- unname(lapply(L_outfile_list[[x]], function(i) { 
    list(`@id` = i$`@id`) 
  }))

  rc |>
    add_entity_value(id = "./", key = "author", value = list(`@id` = me$`@id`)) |>
    add_entity_value(id = "./", key = "license", value = list(`@id` = lic$`@id`)) |>
    add_entity_value(id = "./", key = "hasPart", 
      value = c(
        list(
          list(`@id` = yaml$`@id`),
          list(`@id` = indir$`@id`),
          list(`@id` = lic$`@id`)
        ),
        L_outfile_id_list
      )
    ) |>
    add_entity_value(id = "./", key = "mentions", value = list(
        list(`@id` = exec$`@id`),
        list(`@id` = code$`@id`)
      )) |>
    add_entity_value(id = me$`@id`, key = "affiliation", value = list(`@id` = aff$`@id`)) |>
    add_entity_value(id = exec$`@id`, key = "object", 
      value = list(
        list(`@id` = yaml$`@id`),
        list(`@id` = indir$`@id`)
      )) |>
    add_entity_value(id = exec$`@id`, key = "result", value = L_outfile_id_list) |>
    add_entity_value(id = exec$`@id`, key = "instrument", value = list(`@id` = code$`@id`))
})
names(rocrate_list) <- names(L_outfile_list)

# Write crates to file
lapply(names(rocrate_list), function(x) { 
  write_rocrate(rocrate_list[[x]], 
    file.path(L_dir_list[[x]], "ro-crate-metadata.json"))
})

# Copy param.yaml to each data output
lapply(L_dir_list, function(x) { 
  file.copy("./param.yaml", file.path(x, "param.yaml"))
})

