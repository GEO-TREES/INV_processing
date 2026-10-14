# Initiate repository
renv::init(bare = TRUE)

# List packages and where called 
renv::dependencies()

# Install packages
renv::install()

# Install GEO-TREES development branch of BIOMASS
renv::install("umr-amap/BIOMASS@BIOMASS_for_GEOTREES")

# Snapshot package versions
renv::snapshot()

# Check renv status
renv::status()

