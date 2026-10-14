# Test data outputs from all sites
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2026-10-14

# Packages
library(dplyr)
library(sf)

# Optionally filter to particular sites. NULL includes all sites
sites_sel <- NULL

# Find quad_summ files, in
# ./dat/sites/<SITE>/INV/<ACQUISITION>/<VERSION>/<SOFTWARE>/10_quad_summ/
quad_summ_files <- list.files("./dat/sites", "^quad_summ\\.gpkg$",
  recursive = TRUE, full.names = TRUE)

quad_summ_pattern <- "^\\./dat/sites/([^/]+)/INV/([^/]+)/([^/]+)/([^/]+)/10_quad_summ/quad_summ\\.gpkg$"

quad_summ_files_df <- data.frame(filepath = quad_summ_files) %>%
  filter(grepl(quad_summ_pattern, filepath)) %>%
  mutate(
    site_id = sub(quad_summ_pattern, "\\1", filepath),
    acquisition_id = sub(quad_summ_pattern, "\\2", filepath),
    product_version = sub(quad_summ_pattern, "\\3", filepath),
    software_version = sub(quad_summ_pattern, "\\4", filepath))

if (!is.null(sites_sel)) {
  quad_summ_files_df <- filter(quad_summ_files_df, site_id %in% sites_sel)
}

# Import quad_summ objects, keeping only the columns used here
# Add meta-data from filepath
quad_summ_list <- lapply(seq_len(nrow(quad_summ_files_df)), function(x) {
  out <- st_read(quad_summ_files_df$filepath[x], quiet = TRUE)
  if (!"agbd_Mg_ha_mc_mean" %in% names(out)) {
    message("Output from an older version of the pipeline, re-run: ",
      quad_summ_files_df$filepath[x])
    return(NULL)
  }
  out <- out[, c("site_id", "acquisition_id", "plot_id", "quadrat_id",
    "agbd_Mg_ha_mc_mean")]
  out$product_version <- quad_summ_files_df$product_version[x]
  out$software_version <- quad_summ_files_df$software_version[x]
  return(out)
})

# Transform CRS to WGS84 and join into one dataframe
quad_summ_list <- quad_summ_list[!vapply(quad_summ_list, is.null, logical(1))]
if (length(quad_summ_list) == 0) {
  stop("No outputs from the current version of the pipeline found")
}
quad_summ_all <- bind_rows(lapply(quad_summ_list, st_transform, crs = 4326))

# Which quadrats contain no biomass?
quad_summ_all %>%
  filter(agbd_Mg_ha_mc_mean == 0 | is.na(agbd_Mg_ha_mc_mean)) %>%
  st_drop_geometry() %>%
  dplyr::select(site_id, acquisition_id, product_version, software_version,
    plot_id, quadrat_id, agbd_Mg_ha_mc_mean) %>%
  as_tibble() %>%
  print(n = Inf)
