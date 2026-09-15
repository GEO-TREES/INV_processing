# Format ForestGEO data for GEO-TREES
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2026-09-07

# This script provides a generic template for converting tree inventory data
# from the ForestGEO data portal into the format required for the GEO-TREES
# tree inventory data processing pipeline. This script should be amended
# depending on the particular dataset.

# Packages
library(dplyr)
library(stringr)
library(yaml)

# Define input directory containing data from a single site
indir <- "~/gdrive/geo-trees/papers/agb_mech/sites/dat/raw/amacayacu/stems"

# Define output directory
outdir <- "./"

# Import data
s <- read.csv(file.path(indir, "./PlotDataReport10-07-2025_409093409.txt"), sep = "\t")

# Import param.yaml
param <- read_yaml("../sites/Amacayacu/2024-08-03/v1/param.yaml")

# Import column definitions
stem_cols <- read.csv("../templates/stem_cols.csv")

# Source functions
source("../func.R")

# Prepare stem data 
s_clean <- s %>% 
  rename(
    tree_id = Tag,
    stem_id = StemTag,
    taxon_name = Latin,
    x_rel_m = PX, 
    y_rel_m = PY,
    subplot_id = Quadrat,
    pom_m = HOM,
    diam_cm = DBH,
    measurement_date = Date
    ) %>% 
  mutate(
    site_id = param$site_id, 
    acquisition_id = param$acquisition_id,
    plot_id = "Amacayacu",  # TODO
    diam_cm = diam_cm / 10,
    height_m = NA_real_,
    measurement_date = as.character(measurement_date),
    alive = ifelse(Status == "alive", "A", "D"),
    fallen = ifelse(str_detect(Codes, "\\b(DC|Y)\\b"), "F", "S"),
    missing = ifelse(str_detect(Codes, "\\b(DN|DT)\\b"), "M", ""),
    broken = ifelse(str_detect(Codes, "\\bQ\\b"), "B", ""),
    stump = ifelse(str_detect(Codes, "\\bX\\b"), "T", ""),
    buttress = ifelse(str_detect(Codes, "\\bB\\b"), "U", ""),
    irregular = ifelse(str_detect(Codes, "\\bI\\b"), "I", ""),
    code = pasteVals(alive, fallen, missing, broken, stump, buttress, irregular),
    agb_allometry = NA_character_,  # TODO
    growth_form = NA_character_,  # TODO
    height_allometry = NA_character_,  # TODO
    notes = NA_character_  # TODO
  ) %>% 
  group_by(plot_id, stem_id) %>% 
  mutate(measurement_id = row_number()) %>% 
  ungroup() %>% 
  mutate(record_id = row_number()) %>% 
  dplyr::select(all_of(stem_cols$column_name))
  
# Check all columns in output objects
colCheck(s_clean, stem_cols)

# Check values
valCheck(
  stem = s_clean
  )

# Write stem data to file
write.csv(s_clean, file.path(outdir, "stem.csv"), row.names = FALSE)


