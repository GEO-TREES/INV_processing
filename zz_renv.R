# Initiate repository
renv::init(bare = TRUE)

# Find packages to install
# See .renvignore for files searched
renv::hydrate(
  update = TRUE,
  report = TRUE,
  prompt = TRUE)

# Snapshot package versions
renv::snapshot()

# Check renv status
renv::status()

