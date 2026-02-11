# Clean Panama stem data
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2025-06-23

# Packages
library(dplyr)

# Source functions
source("../../func.R")

# Define site ID
site_id <- "Panama Canal"

# Define directories
indir <- "../../dat/sites/Panama Canal/raw"
outdir <- "../../dat/sites/Panama Canal/02_stem"

# Import column descriptions
stem_cols <- read.csv("../../templates/stem_cols.csv")

# Import stem data from Gigante
# c/o Suzanne Lao, Helene Muller-Landau
s <- read.delim(file.path(indir, "Gigante_2023census_WorkingFile20250301.txt"))
n <- read.delim(file.path(indir, "nomenclature_R_20210224_Rready.txt"))

s2 <- read.csv(file.path(indir, "request_2023.csv")) 

# Prepare stem data 
s_clean <- s %>% 
  left_join(., n, by = c("sp23" = "sp6")) %>% 
  filter(!as.numeric(dbh23) %in% c(-9, 0)) %>%  # Remove missing records
  mutate(
    broken = ifelse(grepl("X|Q", code23), TRUE, FALSE),  # 
    fallen = ifelse(grepl("Y", code23), TRUE, FALSE),
    missing = FALSE,
    liana = FALSE,
    alive = TRUE,
    census_date = format(as.Date(as.character(date23), format = "%Y%m%d"), "%Y-%m-%d"),
    diam_cm = dbh23 / 10,
    plot_id = "Gigante fertilization plot",
    x_rel_m = as.numeric(gx23),
    y_rel_m = as.numeric(gy23),
    stem_id = tag,
    taxon_name = paste(genus, species)) %>% 
  dplyr::select(any_of(stem_cols$column_name))

s2_clean <- s2 %>% 
  filter(
    as.numeric(DBH) != -9,  # Remove missing records
    PlotName %in% c("bci", "P12", "P14", "P06", "P15", "elcharco", "metrop", 
      "soberania", "FincaRoubik", "sherman")) %>% 
  mutate(
    alive = ifelse(Status %in% c("alive", "broken below"), TRUE, FALSE),
    broken = ifelse(Status == "broken below", TRUE, FALSE),
    missing = ifelse(Status == "missing", TRUE, FALSE),
    liana = FALSE,
    missing = FALSE,
    fallen = ifelse(grepl("Y", ListOfTSM), TRUE, FALSE),
    plot_id = case_when(
      PlotName == "bci" ~ "BCI 50 ha plot",
      PlotName == "elcharco" ~ "ElCharco",
      PlotName == "metrop" ~ "Metrop",
      PlotName == "soberania" ~ "Soberania",
      PlotName == "sherman" & QuadratID < 1520 ~ "San Lorenzo A",
      PlotName == "sherman" & QuadratID >= 1520 ~ "San Lorenzo B",
      TRUE ~ PlotName),
    diam_cm = as.numeric(DBH) / 10,
    x_rel_m = as.numeric(PX),
    y_rel_m = as.numeric(PY),
    x_rel_m = case_when(
      plot_id == "San Lorenzo B" ~ x_rel_m - 140,
      TRUE ~ x_rel_m),
    y_rel_m = case_when(
      plot_id == "San Lorenzo B" ~ y_rel_m - 40,
      TRUE ~ y_rel_m),
    pom_m = as.numeric(HOM),
    taxon_name = paste(Genus, SpeciesName)
  ) %>% 
  rename(
    census_id = PlotCensusNumber,
    census_date = ExactDate,
    subplot_id = QuadratID,
    stem_id = StemID,
    tree_id = TreeID) %>% 
  dplyr::select(any_of(stem_cols$column_name))

# Join stems tables
# Add metadata to stems
s_all <- bind_rows(s_clean, s2_clean) %>% 
  mutate(
    subplot_id = as.character(subplot_id),
    tree_id = as.character(tree_id),
    stem_id = as.character(stem_id),
    record_id = row_number(),
    site_id,
    height_m = NA_real_,
    taxon_name = gsub("NA NA", "Indet indet", taxon_name),
    agb_allometry = NA_character_) %>%
  group_by(plot_id, census_id, stem_id) %>% 
  mutate(measurement_id = row_number()) %>% 
  ungroup() %>% 
  dplyr::select(all_of(stem_cols$column_name))

# Check columns in stems table
colCheck(s_all, stem_cols)

# Check values in stems table
stemValCheck(s_all)

# Write data to file
write.csv(s_all, file.path(outdir, "stem.csv"), row.names = FALSE)
