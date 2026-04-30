# Prepare example data for GEO-TREES Tree Inventory workflow example
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2026-04-23

# Set input directory
indir <- "~/gdrive/geo-trees/PDA_processing/dat/sites/Amacayacu/PDA/2024-08-03/v1/v0-1"
paramfile <- "~/gdrive/geo-trees/PDA_processing/sites/Amacayacu/2024-08-03/v1/param.yaml"

# Ensure reproducibility
set.seed(42)

# Packages
library(dplyr)
library(tidyr)
library(yaml)

# Source functions
source("../../../func.R")

# Import templates
stem_cols <- read.csv("../../../templates/stem_cols.csv")
plot_cols <- read.csv("../../../templates/plot_cols.csv")
pt_cols <- read.csv("../../../templates/pt_cols.csv")
taxon_cols <- read.csv("../../../templates/taxon_cols.csv")
height_cols <- read.csv("../../../templates/height_cols.csv")
wd_cols <- read.csv("../../../templates/wd_cols.csv")

stem_col_class <- setNames(stem_cols$class, stem_cols$column_name)
plot_col_class <- setNames(plot_cols$class, plot_cols$column_name)
pt_col_class <- setNames(pt_cols$class, pt_cols$column_name)
taxon_col_class <- setNames(taxon_cols$class, taxon_cols$column_name)
height_col_class <- setNames(height_cols$class, height_cols$column_name)
wd_col_class <- setNames(wd_cols$class, wd_cols$column_name)

# Import raw data 
stem <- read.csv(file.path(indir, "01_fmt/stem.csv"), colClasses = stem_col_class)
plot <- read.csv(file.path(indir, "01_fmt/plot.csv"), colClasses = plot_col_class)
pt <- read.csv(file.path(indir, "01_fmt/plot_pt.csv"), colClasses = pt_col_class)
wd <- read.csv(file.path(indir, "04_wd/stem_wd.csv"))
hd <- read.csv(file.path(indir, "05_height/stem_height.csv"))
taxa <- read.csv(file.path(indir, "02_taxa/stem_taxa.csv"))
param <- read_yaml(paramfile)

# Anonymyise stem data
stem_anon <- stem %>% 
  mutate(
    site_id = "Site1",
    plot_id = "Plot1")

# Anonymyise plot meta-data
plot_anon <- plot %>% 
  mutate(
    site_id = "Site1",
    plot_id = "Plot1")

# Anonymise plot spatial points
pt_anon <- pt %>% 
  mutate(
    site_id = "Site1",
    plot_id = "Plot1",
    rover_easting_rms_m = rover_easting_rms_m + 5432,
    rover_northing_rms_m = rover_northing_rms_m + 5432)

# Anonymise param.yaml
param_anon <- param 
param_anon$site_id <- "Site1"
param_anon$out_dir <- paste0("./dat/sites/Site1/PDA/", unique(stem$acquisition_id), "/v1")
param_anon$raw_dir <- paste0("./dat/sites/Site1/PDA/", unique(stem$acquisition_id), "/L0")
param_anon$s3_dir <- paste0("GEO-TREES_PDA/dat/sites/Site1/", unique(stem$acquisition_id), "/L0")

# Thin out stem data
stem_fil <- stem_anon %>% 
  slice_sample(n = 5000)

# Clean taxonomy data
taxa_clean <- taxa %>%
  mutate(
    site_id = "Site1",
    taxon_subspecies = NA_character_,
    taxon_variety = NA_character_,
    acquisition_id = unique(stem_anon$acquisition_id)) %>% 
  dplyr::select(
    site_id,
    acquisition_id, 
    taxon_name = nameOriginal,
    taxon_family = familyAccepted,
    taxon_genus = genusAccepted,
    taxon_species = speciesAccepted,
    taxon_subspecies,
    taxon_variety) %>% 
  filter(
    taxon_name != "",
    taxon_name %in% stem_fil$taxon_name)

# Create synthetic height-diameter data
hd_syn <- stem_anon %>% 
  left_join(., hd, by = "record_id") %>% 
  dplyr::select(site_id, taxon_name, diam_cm, height_m = height_m_pred) %>% 
  sample_n(1000) %>% 
  rowwise() %>% 
  mutate(
    plot_id = NA_character_,
    noise_type = sample(c("small", "large", "error"), 1, prob = c(0.85, 0.10, 0.05)),
    height_m = case_when(
      noise_type == "small" ~ rnorm(1, mean = height_m, sd = height_m * 0.05),
      noise_type == "large"   ~ rnorm(1, mean = height_m, sd = height_m * 0.25),
      noise_type == "error" ~ runif(1, min = 1, max = 10)
    ),
    height_m = round(height_m, 1)
  ) %>%
  ungroup() %>% 
  dplyr::select(
    site_id, 
    plot_id,
    taxon_name, 
    diam_cm, 
    height_m)

# Create synthetic wood density data
wd_syn <- wd %>% 
  filter(
    !is.na(species), 
    levelWD == "species") %>% 
  sample_n(50) %>% 
  rowwise() %>%
  mutate(
    n_samples = sample(1:20, 1),
    sampled_WD = list(rnorm(n = n_samples, mean = meanWD, sd = sdWD))
  ) %>%
  unnest(sampled_WD) %>%
  ungroup() %>% 
  slice_head(n = 500) %>% 
  mutate(
    taxon_name = paste(genus, species),
    site_id = "Site1") %>% 
  group_by(site_id, taxon_name) %>% 
  summarise(
    wood_density_gcm3 = mean(sampled_WD),
    wood_density_sd_gcm3 = sd(sampled_WD),
    wood_density_n = n()) %>% 
  ungroup() %>% 
  dplyr::select(
    site_id,
    taxon_name, 
    wood_density_gcm3,
    wood_density_sd_gcm3,
    wood_density_n)

# Check output dataframes
colCheck(stem_fil, stem_cols)
colCheck(plot_anon, plot_cols)
colCheck(pt_anon, pt_cols)
colCheck(taxa_clean, taxon_cols)
colCheck(wd_syn, wd_cols)
colCheck(hd_syn, height_cols)
    
valCheck(
  plot = plot_anon,
  stem = stem_fil,
  pt = pt_anon,
  taxon = taxa_clean,
  height = hd_syn,
  wd = wd_syn)

# Write files
write.csv(stem_fil, "./data/stem.csv", row.names = FALSE)
write.csv(plot_anon, "./data/plot.csv", row.names = FALSE)
write.csv(pt_anon, "./data/pt.csv", row.names = FALSE)
write.csv(taxa_clean, "./data/taxa.csv", row.names = FALSE)
write.csv(wd_syn, "./data/wd.csv", row.names = FALSE)
write.csv(hd_syn, "./data/hd.csv", row.names = FALSE)
write_yaml(param_anon, "./data/param.yaml")

# Copy WFO cache
file.copy(
  from = file.path(indir, "02_taxa/wfo_cache.rds"),
  to = "./data/wfo_cache.rds",
  overwrite = TRUE)

# Copy version YAML file
file.copy(
  from = "../../../version.yaml",
  to = "./data/version.yaml",
  overwrite = TRUE)
