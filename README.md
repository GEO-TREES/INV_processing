# Workflow for processing GEO-TREES tree inventory data

This repository contains code to clean and process tree inventory data from GEO-TREES sites.

Each directory in `./sites/` contains one or more acquisition sub-directories. Acquisitions are bundles of raw (L0) tree inventory data, which can be used together with a single airborne LiDAR (ALS) data acquisition to produce estimates of above-ground woody biomass density (AGBD) across the focal landscape of a site. 

A tree inventory acquisition may comprise one or more discrete data collection events, e.g. the census of a group of plots within the site during a single field campaign. Acquisitions may include data from some or all plots within a site. Acquisitions should only contain one census per plot. Acquisitions are named according to the mid-date of all stem measurements within the acquisition. 

Crucially, censuses bundled within an acquisition should generally occur within one year either side of the corresponding ALS acquisition. While this window is somewhat subjective, depending on disturbance events and seasonality, staying within this range minimises temporal mismatches that could compromise AGBD estimates.

Each acquisition directory contains a `./<SITE>/<ACQUISITION>/01_fmt.R` script to perform initial cleaning of raw (L0) data. This script produces four files:

* `stem.csv` - Stem measurement table, where each row is a measurement of a stem within a single census within a plot.
* `plot.csv` - Plot metadata table, where each row is a plot. 
* `plot_pt.gpkg` - Points locating the corners of plots, with additional columns describing the stem map coordinate system.

See `./templates/*` for guidance on which columns should be included in these files.

In the root directory there are additional scripts which process data from any site:

* `./02_taxa.R` - correct taxonomic information
* `./03_wd.R` - estimate wood density for each stem measurement
* `./04_height.R` - estimate stem height for each stem measurement
* `./05_agb_stem.R` - estimate above-ground woody biomass for each stem measurement
* `./06_record_fil.R` - filter stem data before estimating AGBD
* `./07_quad.R` - create quadrats in each plot 
* `./08_agb_mc.R` - AGB Monte-Carlo error propagation
* `./09_stem_summ.R` - create master stem measurement table
* `./10_quad_summ.R` - summarise quadrat measurements
* `./11_brm.R` - Create L1, L2 and L3 datasets

* `./zz_site.R` - run scripts in order to process a single site
* `./zz_site_all.R` - run scripts in order to process all sites with a valid `param.yaml` configuration file
* `./zz_wd_prep.R` - prepare wood density dataset
* `./zz_renv.R` - prepare reproducible R environment

* `./func.R` - frequently used functions

Key outputs from each site include:

* `./02_taxa.R`:
    * `wfo_cache.rds` - Cache generated from taxonomic name cleaning. Documents choices made by user.
* `./09_stem_summ.R`: `stem_summ.gpkg` - Combined stem-level dataset. 
* `./10_quad_summ.R`: `quad_summ.gpkg` - Combined quadrat-level dataset.
* `./11_brm.R`:
    * L1
        * `*_L1_*_stem.csv` - GEO-TREES L1 stem measurement data
        * `*_L1_*_plot.csv` - GEO-TREES L1 plot meta-data 
        * `*_L1_*_pt.gpkg` - GEO-TREES L1 plot spatial reference points
    * L2
        * `*_L2_*_stem.csv` - GEO-TREES L2 stem AGB estimates
        * `*_L2_*_poly.gpkg` - GEO-TREES L2 plot spatial polygons
    * L3
        * `*_L3_*_quad.gpkg` - GEO-TREES L3 quadrat AGBD estimates

## Environment 

This repository uses `renv` ([https://github.com/rstudio/renv](https://github.com/rstudio/renv)) to manage a reproducible R environment. This project only installs the specific packages used in the production scripts.

When you open this project for the first time, R will automatically detect the `renv` setup. If required, run `renv::restore()` to install all required dependencies (including specific versions from CRAN and GitHub).

The `./.renvignore` file specifies files and directories to be monitored for new packages. If you need to add a new package to the project:

1. Ensure the package is called in one of the tracked files or directories.
2. Install the package: `renv::install("package_name")` or `renv::install("user/repo@branch")` for packages on GitHub.
3. Update the lockfile: `renv::snapshot()`

## Reproducibility

To track data inputs and versions of the processing code, each `L`-level data output is accompanied by an [RO-Crate](https://www.researchobject.org/ro-crate/) JSON file containing this meta-data. `./11_brm.R` contains code to programmatically create these files. These files can be used to precisely reproduce each data output.

A `param.yaml` file is used to track user-defined parameters for each acquisition, such as the site name, quadrat dimensions, input and output directories. A copy of this file is placed alongside each `L`-level data output. A template of `param.yaml` is located in `./templates/param.yaml`.

`./version.yaml` is used to track code versions. 


