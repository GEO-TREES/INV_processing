# Format ForestPlots data for GEO-TREES
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2026-09-07

# This script provides a generic template for converting tree inventory data
# from the ForestPlots database into the format required for the GEO-TREES tree
# inventory data processing pipeline. This script should be amended depending
# on the particular dataset.

# Packages
library(dplyr)
library(yaml)

# Source functions
source("../func.R")

# Define input directory containing data from a single site
indir <- "~/gdrive/seco/plot_data_process/dat/fp_raw"

# Define output directory
outdir <- "./"

# Import param.yaml
param <- read_yaml("../sites/Bicuar/2024-02-12/v1/param.yaml")

# Import data
s <- read.csv(file.path(indir, "fp_individuals.csv"))

# Import column definitions
stem_cols <- read.csv("../templates/stem_cols.csv")

# Prepare stem data
s_clean <- s %>% 
  filter(grepl("NXV", Plot.Code)) %>% 
  rename(
    plot_id = Plot.Code,
    tree_id = Main.Stem.Tag,
    stem_id = TreeID, 
    subplot_id = Standardised.SubPlot.T1,
    pom_m = POM,
    diam_cm = D4,
    x_rel_m = Standardised.X,
    y_rel_m = Standardised.Y,
    taxon_name = Recommended.Species,
    height_m = Height,
    measurement_date = Census.Date,
    notes = Comments
    ) %>% 
  mutate(
    site_id = param$site_id,
    acquisition_id = param$acquisition_id,
    subplot_id = as.character(subplot_id),
    measurement_date = as.character(measurement_date),
    diam_cm = diam_cm / 10,
    diam_cm = ifelse(diam_cm == 0, NA_real_, diam_cm),
    pom_m = pom_m / 1000,
    pom_m = ifelse(pom_m == 0, NA_real_, pom_m),
    height_m = ifelse(height_m == 0, NA_real_, height_m),
    tree_id = ifelse(tree_id == "", stem_id, tree_id),
    stem_id = paste(tree_id, stem_id, sep = ":"),
    # F1 = "0" means dead. If it is anything else (and not NA), it is alive.
    alive = ifelse(grepl("0", F1), "D", "A"),
    fallen = ifelse(
      grepl("d", F1) | grepl(paste("g", "c", "i", sep = "|"), F2), 
      "F", "S"),
    broken = ifelse(
      grepl(paste("b", "k", sep = "|"), F1) | grepl(paste("b", "e", "h", sep = "|"), F2), 
      "B", ""),
    missing = ifelse(grepl(paste("k", "l", sep = "|"), F2), "M", ""),
    missing = ifelse(!is.na(diam_cm), "", missing),
    pom_m = ifelse(missing == "M", NA_real_, pom_m),
    stump = ifelse(grepl("e", F3), "T", ""), 
    buttress = ifelse(grepl("6", F3), "U", ""),
    irregular = ifelse(grepl("i", F3) | grepl("e", F1), "I", ""),
    hollow = ifelse(grepl("f", F1), "H", ""),
    code = pasteVals(alive, fallen, missing, broken, stump, buttress, irregular),
    growth_form = NA_character_,  # TODO
    height_allometry = NA_character_,  # TODO
    agb_allometry = NA_character_  # TODO
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

