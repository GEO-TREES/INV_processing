# Prepare wood density dataset
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2025-07-08

# Packages
library(dplyr)
library(BIOMASS)

# Source functions
source("./func.R")

# Define directories
indir <- "./dat/01_wd/raw"
outdir <- "./dat/01_wd"

# Import wood density data
cirad <- read.csv(file.path(indir, "cirad_wd/Cirad-wood-density-database.csv"))
zanne <- readRDS(file.path(indir, "zanne_wd/zanne_wdData.rds"))

# Join some columns from plots to stems table 
cirad_clean <- cirad %>%
  mutate(
    Species_split = strsplit(Species, " "),
    genus = unlist(lapply(Species_split, "[", 1)),
    species = unlist(lapply(Species_split, "[", 2)),
    wd_region = case_when(
      Country == "Algeria" ~ "AfricaExtraTrop",
      Country == "Australia" ~ "Australia",
      Country == "Benin" ~ "AfricaTrop",
      Country == "Brazil" ~ "SouthAmericaTrop",
      Country == "Burkina Faso" ~ "AfricaTrop",
      Country == "Burma" ~ "SouthAsia",
      Country == "Burundi" ~ "AfricaTrop",
      Country == "Cambodia" ~ "SouthEastAsia",
      Country == "Cameroon" ~ "AfricaTrop",
      Country == "Canada" ~ "NorthAmerica",
      Country == "Central African Republic" ~ "AfricaTrop",
      Country == "Chile" ~ "SouthAmericaExtraTrop",
      Country == "China" ~ "Asia",
      Country == "Colombia" ~ "SouthAmericaTrop",
      Country == "Comoros" ~ "IndianOcean",
      Country == "Cote d'Ivoire" ~ "AfricaTrop",
      Country == "Cuba" ~ "Caribbean",
      Country == "Democratic Republic of the Congo" ~ "AfricaTrop",
      Country == "Dominican Republic" ~ "Caribbean",
      Country == "Ecuador" ~ "SouthAmericaTrop",
      Country == "France" ~ "Europe",
      Country == "French Guiana" ~ "SouthAmericaTrop",
      Country == "French Polynesia" ~ "Oceania",
      Country == "Gabon" ~ "AfricaTrop",
      Country == "Guadeloupe" ~ "Caribbean",
      Country == "Guinea" ~ "AfricaTrop",
      Country == "Guyana" ~ "SouthAmericaTrop",
      Country == "Honduras" ~ "CentralAmericaTrop",
      Country == "Indonesia" ~ "SouthEastAsiaTrop",
      Country == "Japan" ~ "Asia",
      Country == "Liberia" ~ "AfricaTrop",
      Country == "Madagascar" ~ "IndianOcean",
      Country == "Malaysia" ~ "SouthEastAsiaTrop",
      Country == "Mali" ~ "AfricaTrop",
      Country == "Martinique" ~ "Caribbean",
      Country == "Mauritius" ~ "IndianOcean",
      Country == "Mayotte" ~ "IndianOcean",
      Country == "Mexico" ~ "CentralAmericaTrop",
      Country == "Morocco" ~ "AfricaExtraTrop",
      Country == "New Caledonia" ~ "Oceania",
      Country == "New Zealand" ~ "Oceania",
      Country == "Nicaragua" ~ "SouthAmericaTrop",
      Country == "Niger" ~ "AfricaTrop",
      Country == "Papua New Guinea" ~ "SouthEastAsiaTrop",
      Country == "Paraguay" ~ "SouthAmericaExtraTrop",
      Country == "Peru" ~ "SouthAmericaTrop",
      Country == "Philippines" ~ "SouthEastAsiaTrop",
      Country == "Portugal" ~ "Europe",
      Country == "Reunion" ~ "IndianOcean",
      Country == "Senegal" ~ "AfricaTrop",
      Country == "Seychelles" ~ "IndianOcean",
      Country == "Solomon Islands" ~ "Oceania",
      Country == "Spain" ~ "Europe",
      Country == "Suriname" ~ "SouthAmericaTrop",
      Country == "Sweden" ~ "Europe",
      Country == "Thailand" ~ "SouthEastAsiaTrop",
      Country == "Togo" ~ "AfricaTrop",
      Country == "Trinidad and Tobago" ~ "Caribbean",
      Country == "United Republic of Tanzania" ~ "AfricaTrop",
      Country == "United States" ~ "NorthAmerica",
      Country == "Uruguay" ~ "SouthAmericaExtraTrop",
      Country == "Vanuatu" ~ "Oceania",
      Country == "Venezuela" ~ "SouthAmericaTrop",
      Country == "Viet Nam" ~ "SouthEastAsiaTrop",
      Country == "Wallis and Futuna Islands" ~ "Oceania",
      TRUE ~ NA_character_),
    wd_source = "cirad") %>% 
  dplyr::select(
    family = Family,
    genus,
    country = Country,
    species_epithet = species, 
    wd = Db,
    wd_region,
    wd_source) %>% 
  filter(!genus %in% c("LAU"))

# Clean Zanne wood density database
zanne_clean <- zanne %>%
  mutate(
    wd_source = "zanne",
    wd_region = case_when(
      regionId == "SouthEastAsia" ~ "SouthEastAsiaTrop",
      regionId == "Mexico" ~ "CentralAmericaTrop",
      regionId == "Madagascar" ~ "IndianOcean",
      regionId == "India" ~ "SouthAsia",
      regionId == "China" ~ "Asia",
      TRUE ~ regionId)) %>% 
  dplyr::select(family, genus, species_epithet = species, wd, 
    wd_region, wd_source)

# Combine Zanne and Cirad wood density data
wd_clean <- bind_rows(cirad_clean, zanne_clean) %>% 
  mutate(
    species = paste(genus, species_epithet), 
    species = gsub(" sp$", " sp.", species),
    .after = "species_epithet")

# Check taxonomic names in wood density data
taxon_check <- correctTaxo(
  genus = wd_clean$species, 
  species = NULL,
  interactive = TRUE, 
  preferAccepted = TRUE,
  preferFuzzy = FALSE,
  sub_pattern = subPattern(),
  useCache = TRUE,
  useAPI = TRUE,
  capacity = 60,
  fill_time_s = 1, 
  timeout = 60
)

# Check no rows added or lost
stopifnot(nrow(wd_clean) == nrow(taxon_check))

# Combine dataframes
wd_out <- cbind(wd_clean, taxon_check)

# Check no names are duplicated
stopifnot(all(!duplicated(names(wd_out))))

# Filter out missing names
wd_fil <- wd_out[!is.na(wd_out$nameAccepted),]

# Check validity of output
stopifnot(all(!is.na(wd_fil$nameAccepted)))
stopifnot(all(!is.na(wd_fil$wd)))
stopifnot(all(!is.na(wd_fil$wd_region[!is.na(wd_fil$country)])))
stopifnot(all(!is.na(wd_fil$wd_region[wd_fil$wd_source == "zanne"])))

# Write wood density data to file
write.csv(wd_fil, file.path(outdir, "wd.csv"), row.names = FALSE)

# Write WFO cache to file
saveRDS(BIOMASS:::the$wfo_cache, file.path(outdir, "wfo_cache.rds"))
