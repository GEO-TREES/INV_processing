# Clean Nouragues stem data
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2026-02-16

# Packages
library(dplyr)
library(tidyr)

# Source functions
source("../../func.R")

# Define site ID
site_id <- "Nouragues"

# Define directories
indir <- "../../dat/sites/Nouragues/raw"
outdir <- "../../dat/sites/Nouragues/02_stem"

# Import stem column descriptions
stem_cols <- read.csv("../../templates/stem_cols.csv")

# Import stem data
s <- read.csv(file.path(indir, "2023-09-29_Petit_Plateau2022.csv"))

# Prepare stem data 
s_clean <- s %>% 
  rename(
    plot_id = Plot,
    subplot_id = SubPlot,
    tree_id = idTree,
    x_rel_m = Xfield,
    y_rel_m = Yfield,
    census_id = CensusYear,
    measurement_date = CensusDate,
    alive = CodeAlive) %>% 
  mutate(
    site_id,
    plot_id = as.character(plot_id),
    subplot_id = as.character(subplot_id),
    tree_id = as.character(tree_id),
    stem_id = NA_character_,
    pom_m = POM * 0.01,
    diam_cm = CircCorr / pi,
    height_m = NA_real_,
    alive = as.logical(alive),
    GenusFilled = iconv(GenusFilled, "UTF-8", "UTF-8", sub = ""),
    SpeciesFilled = iconv(SpeciesFilled, "UTF-8", "UTF-8", sub = ""),
    taxon_name = paste(trimws(GenusFilled), trimws(SpeciesFilled)),
    broken = FALSE,
    fallen = ifelse(MeasCode == 12 , TRUE, FALSE),
    missing = FALSE,
    agb_allometry = NA_character_) %>% 
  group_by(plot_id) %>% 
  mutate(census_id = dense_rank(census_id)) %>% 
  ungroup() %>% 
  group_by(plot_id, census_id) %>% 
  mutate(census_date = format(mean(as.Date(measurement_date)))) %>% 
  ungroup() %>% 
  group_by(plot_id, tree_id, stem_id, census_id) %>% 
  mutate(measurement_id = row_number()) %>% 
  ungroup() %>% 
  mutate(record_id = row_number()) %>% 
  dplyr::select(all_of(stem_cols$column_name))

# Check all columns in output objects
colCheck(s_clean, stem_cols)

# Check values
stemValCheck(s_clean)

# Write data to file
write.csv(s_clean, file.path(outdir, "stem.csv"), row.names = FALSE)


