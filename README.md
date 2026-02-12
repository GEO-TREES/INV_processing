# Workflows for processing GEO-TREES tree inventory data

This repository contains code to clean and process tree inventory data from GEO-TREES sites.

Each directory in `./sites/` contains scripts to perform initial cleaning on raw data from each GEO-TREES site:

* `./*/01_plot.R` - create plot polygons, format plot metadata
* `./*/02_stem.R` - clean stem measurement data

In the root directory there are additional scripts which process data from any site:

* `./03_taxa.R` - correct taxonomic information
* `./04_quad.R` - create quadrats in each plot
* `./05_wd.R` - estimate wood density for each stem measurement
* `./06_height.R` - estimate stem height for each stem measurement
* `./07_agb.R` - estimate above-ground woody biomass for each stem measurement
* `./08_stem_summ.R` - create master stem measurement table
* `./09_quad_summ.R` - summarise quadrat measurements

* `./zz_site.R` - run scripts in order to process a single site
* `./zz_site_all.R` - process all sites listed as "complete" in `./dat/site_status.csv`

* `./zz_wd_prep.R` - prepare wood density dataset
* `./zz_renv.R` - prepare reproducible R environment

* `./func.R` - frequently used functions


Key outputs from each site include:

* `01_plot.R`:
    * `plot.csv` - Plot metadata table, where each row is a plot. 
    * `plot_poly.gpkg` - Plot polygons.
    * `plot_pt.gpkg` - Points locating the corners of plots, with additional columns describing the stem map coordinate system.
* `03_taxa.R`:
    * `wfo_cache.rds` - Cache generated from taxonomic name cleaning. Documents choices made by user.
* `08_stem_summ.R`:
    * `stem_summ.gpkg` - Combined stem-level dataset. Includes data from `03_taxa/stem_taxa.csv`, `04_quad/stem_pt.gpkg`, `05_wd/stem_wd.csv`, `06_height/stem_height.csv`, `07_agb/stem_agb.csv`.
* `09_quad_summ.R`:
    * `quad_summ.gpkg` - Combined quadrat-level dataset. Includes data from `04_quad/quad_poly.gpkg`, `08_stem_summ/stem_summ.gpkg`.

## Environment 

This repository uses `renv` ([https://github.com/rstudio/renv](https://github.com/rstudio/renv)) to manage a reproducible R environment. This project only installs the specific packages used in the production scripts.

When you open this project for the first time, R will automatically detect the `renv` setup. If required, run `renv::restore()` to install all required dependencies (including specific versions from CRAN and GitHub).

The `./.renvignore` file specifies files and directories to be monitored for new packages. If you need to add a new package to the project:

1. Ensure the package is called in one of the tracked files or directories.
2. Install the package: `renv::install("package_name")` or `renv::install("user/repo")` for packages on GitHub.
3. Update the lockfile: `renv::snapshot()`

