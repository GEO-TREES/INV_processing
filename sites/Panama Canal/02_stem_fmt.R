# Clean Panama stem data
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2025-06-23

# Packages
library(dplyr)

# Define site ID
site_id <- "Panama Canal"

# Define directories
indir <- "../../dat/sites/Panama Canal/raw"
outdir <- "../../dat/sites/Panama Canal/02_stem_fmt"

# Import stem column descriptions
stems_cols <- read.csv("../../dat/templates/stem_fmt_cols.csv")

# Import stem data from Gigante
# c/o Suzanne Lao, Helene Muller-Landau
s <- read.delim(file.path(indir, "Gigante_2023census_WorkingFile20250301.txt"))
n <- read.delim(file.path(indir, "nomenclature_R_20210224_Rready.txt"))

s2 <- read.csv(file.path(indir, "request_2023.csv")) 

# Prepare stem data 
s_clean <- s %>% 
  left_join(., n, by = c("sp23" = "sp6")) %>% 
  filter(as.numeric(dbh23) != -9) %>%
  mutate(
    broken = ifelse(grepl("X|Q", code23), 1, 0),
    fallen = ifelse(grepl("Y", code23), 1, 0),
    missing = 0,
    liana = 0,
    alive = 1,
    census_date = format(as.Date(as.character(date23), format = "%Y%m%d"), "%Y-%m-%d"),
    diam = dbh23 / 10,
    plot_name = "Gigante fertilization plot",
    x_rel = as.numeric(gx23),
    y_rel = as.numeric(gy23)
  ) %>% 
  dplyr::select(
    plot_name, 
    census_date,
    stem_id = tag,
    x_rel,
    y_rel, 
    diam,
    pom = pom23,
    alive,
    broken, 
    fallen,
    missing,
    liana,
    genus,
    species)

s2_clean <- s2 %>% 
  filter(
    as.numeric(DBH) != -9,
    PlotName %in% c("bci", "P12", "P14", "P06", "P15", "elcharco", "metrop", 
      "soberania", "FincaRoubik", "sherman")) %>% 
  mutate(
    alive = ifelse(Status %in% c("alive", "broken below"), 1, 0),
    broken = ifelse(Status == "broken below", 1, 0),
    missing = ifelse(Status == "missing", 1, 0),
    liana = 0,
    missing = 0,
    fallen = ifelse(grepl("Y", ListOfTSM), 1, 0),
    plot_name = case_when(
      PlotName == "bci" ~ "BCI 50 ha plot",
      PlotName == "elcharco" ~ "ElCharco",
      PlotName == "metrop" ~ "Metrop",
      PlotName == "soberania" ~ "Soberania",
      PlotName == "sherman" & QuadratID < 1520 ~ "San Lorenzo A",
      PlotName == "sherman" & QuadratID >= 1520 ~ "San Lorenzo B",
      TRUE ~ PlotName),
    diam = as.numeric(DBH) / 10,
    x_rel = as.numeric(PX),
    y_rel = as.numeric(PY),
    x_rel = case_when(
      plot_name == "San Lorenzo B" ~ x_rel - 140,
      TRUE ~ x_rel),
    y_rel = case_when(
      plot_name == "San Lorenzo B" ~ y_rel - 40,
      TRUE ~ y_rel),
    pom = as.numeric(HOM)
  ) %>% 
  dplyr::select(
    plot_name, 
    census_date = ExactDate,
    subplot_id = QuadratID,
    stem_id = StemID,
    x_rel, 
    y_rel,
    diam,
    missing,
    alive,
    liana,
    fallen,
    broken,
    pom,
    genus = Genus,
    species = SpeciesName) 

# Join stems tables
# Add metadata to stems
s_all <- bind_rows(s_clean, s2_clean) %>% 
  mutate(site_id) %>% 
  filter(is.finite(x_rel), is.finite(y_rel), is.finite(diam)) %>% 
  mutate(
    height = NA_real_,
    measurement_id = paste(site_id, plot_name, stem_id, row_number(), sep = ":"),
    stem_id = paste(site_id, plot_name, stem_id, sep = ":"),
    census_id = paste(site_id, plot_name, 2023, sep = ":"),
    plot_id = paste(site_id, plot_name, sep = ":"),
    taxon_name_orig = paste(genus, species),
    taxon_name_orig = gsub("NA NA", "Indet indet", taxon_name_orig)) %>% 
  dplyr::select(
    site_id,
    plot_name,
    plot_id,
    census_date,
    census_id,
    stem_id,
    measurement_id,
    x_rel,
    y_rel,
    diam,
    pom,
    height,
    taxon_name_orig,
    alive,
    broken,
    fallen,
    missing,
    liana)

# Check all columns in stems table
stopifnot(all(colnames(s_all) == stems_cols$column_name))

# Check measurment IDs are unique
stopifnot(all(!duplicated(s_all$measurement_id)))

# Write data to file
write.csv(s_all, file.path(outdir, "stems.csv"), row.names = FALSE)
