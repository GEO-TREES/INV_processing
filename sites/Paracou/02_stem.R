# Clean Paracou stem data
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2026-02-11

# Packages
library(dplyr)
library(tidyr)

# Source functions
source("../../func.R")

# Define site ID
site_id <- "Paracou"

# Define directories
indir <- "../../dat/sites/Paracou/raw"
outdir <- "../../dat/sites/Paracou/02_stem"

# Import stem column descriptions
stem_cols <- read.csv("../../templates/stem_cols.csv")

# Import stem data
s_P13 <- read.csv(file.path(indir, "Paracou Biodiversity Plots/2024-08-29_ParacouP13AllYears.csv"))
s_P14 <- read.csv(file.path(indir, "Paracou Biodiversity Plots/2024-08-29_ParacouP14AllYears.csv"))
s_P15 <- read.csv(file.path(indir, "Paracou Biodiversity Plots/2024-08-29_ParacouP15AllYears.csv"))
s_P16 <- read.csv(file.path(indir, "Paracou Biodiversity Plots/2023-09-29_ParacouP16AllYears.csv"))

s_P2 <- read.csv(file.path(indir, "Paracou Disturbance Experiment - Level1 Treatment Plots/2024-04-18_ParacouP2AllYears.csv"))
s_P7 <- read.csv(file.path(indir, "Paracou Disturbance Experiment - Level1 Treatment Plots/2024-04-18_ParacouP7AllYears.csv"))
s_P9 <- read.csv(file.path(indir, "Paracou Disturbance Experiment - Level1 Treatment Plots/2024-04-18_ParacouP9AllYears.csv"))

s_P3 <- read.csv(file.path(indir, "Paracou Disturbance Experiment - Level2 Treatment Plots/2024-04-18_ParacouP3AllYears.csv"))
s_P5 <- read.csv(file.path(indir, "Paracou Disturbance Experiment - Level2 Treatment Plots/2024-04-18_ParacouP5AllYears.csv"))
s_P10 <- read.csv(file.path(indir, "Paracou Disturbance Experiment - Level2 Treatment Plots/2024-08-08_ParacouP10AllYears.csv"))

s_P4 <- read.csv(file.path(indir, "Paracou Disturbance Experiment - Level3 Treatment Plots/2024-04-18_ParacouP4AllYears.csv"))
s_P8 <- read.csv(file.path(indir, "Paracou Disturbance Experiment - Level3 Treatment Plots/2024-04-18_ParacouP8AllYears.csv"))
s_P12 <- read.csv(file.path(indir, "Paracou Disturbance Experiment - Level3 Treatment Plots/2024-04-18_ParacouP12AllYears.csv"))

# Check all columns are identical 
stopifnot(all(
  names(s_P13) == names(s_P14),
  names(s_P13) == names(s_P15),
  names(s_P13) == names(s_P16),
  names(s_P13) == names(s_P2),
  names(s_P13) == names(s_P7),
  names(s_P13) == names(s_P9),
  names(s_P13) == names(s_P3),
  names(s_P13) == names(s_P5),
  names(s_P13) == names(s_P10),
  names(s_P13) == names(s_P4),
  names(s_P13) == names(s_P8),
  names(s_P13) == names(s_P12)
))

s <- bind_rows(
  s_P13, 
  s_P14,
  s_P15,
  s_P16,
  s_P2,
  s_P7,
  s_P9,
  s_P3,
  s_P5,
  s_P10,
  s_P4,
  s_P8,
  s_P12)

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
  filter(as.Date(census_date) > as.Date("2017-01-01")) %>% 
  mutate(record_id = row_number()) %>% 
  dplyr::select(all_of(stem_cols$column_name))

# Check all columns in output objects
colCheck(s_clean, stem_cols)

# Check values
stemValCheck(s_clean)

# Write data to file
write.csv(s_clean, file.path(outdir, "stem.csv"), row.names = FALSE)

