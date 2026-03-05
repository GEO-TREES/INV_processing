# Initiate repository
renv::init(bare = TRUE)

# List packages and where called 
renv::dependencies()

# Install packages
renv::install()

# Install dev branch
renv::install("umr-amap/BIOMASS@dev_john")

# Snapshot package versions
renv::snapshot()

# Check renv status
renv::status()

