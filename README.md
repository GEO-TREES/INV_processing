# Workflow for processing GEO-TREES tree inventory data

This repository contains code to clean and process tree inventory data from GEO-TREES sites.

Each directory in `./sites/` contains one or more acquisition sub-directories. Acquisitions are bundles of raw (L0) tree inventory data, which can be used together with a single airborne LiDAR (ALS) data acquisition to produce estimates of above-ground woody biomass density (AGBD) across the focal landscape of a site. 

A tree inventory acquisition may comprise one or more discrete data collection events, e.g. the census of a group of plots within the site during a single field campaign. Acquisitions may include data from some or all plots within a site. Acquisitions should only contain one census per plot. Acquisitions are named according to the mid-date of all stem measurements within the acquisition. 

Crucially, censuses bundled within an acquisition should generally occur within one year either side of the corresponding ALS acquisition. While this window is somewhat subjective, depending on disturbance events and seasonality, staying within this range minimises temporal mismatches that could compromise AGBD estimates.

Each acquisition directory contains:

* `./sites/<SITE>/<ACQUISITION>/01_fmt.R` - script to perform initial cleaning of raw (L0) data.
* `./sites/<SITE>/<ACQUISITION>/wfo_cache.rds` - taxonomic name matching choices, shared by all product versions of the acquisition. See `./02_taxa.R`.
* `./sites/<SITE>/<ACQUISITION>/<VERSION>/param.yaml` - processing parameters for each product version, e.g. `v1`, `v2`. See [Reproducibility](#reproducibility).

`01_fmt.R` produces three files:

* `stem.csv` - Stem measurement table, where each row is a measurement of a stem within a single census within a plot.
* `plot.csv` - Plot metadata table, where each row is a plot. 
* `plot_pt.csv` - Points locating the corners of plots, with additional columns describing the stem map coordinate system.

and optionally:

* `taxon.csv` - Additional taxonomic information for each taxonomic name.
* `height.csv` - Stem diameter and height measurements, used to fit a local height-diameter model.
* `wd.csv` - Local wood density measurements.

See `./templates/*` for guidance on which columns should be included in these files, and the valid values in each column. `colCheck()` and `valCheck()` in `./func.R` check these files against the templates.

Raw data and processed outputs are stored in `./dat/`, which is not tracked by git. Raw data for each acquisition are in the directory given by `raw_dir` in `param.yaml`, e.g. `./dat/sites/<SITE>/INV/<ACQUISITION>/L0/`.

In the root directory there are additional scripts which process data from any site:

* `./02_taxa.R` - correct taxonomic information
* `./03_wd.R` - estimate wood density for each stem measurement
* `./04_height.R` - estimate stem height for each stem measurement
* `./05_agb_stem.R` - estimate above-ground woody biomass for each stem measurement
* `./06_record_fil.R` - filter stem data before estimating AGBD
* `./07_quad.R` - fit plot polygons and create quadrats in each plot
* `./08_agb_mc.R` - AGB Monte-Carlo error propagation
* `./09_stem_summ.R` - create master stem measurement table
* `./10_quad_summ.R` - summarise quadrat measurements
* `./11_brm.R` - Create L1, L2 and L3 datasets

* `./zz_site.R` - run scripts in order to process a single product version of an acquisition, defined by `p_file`, the path to its `param.yaml` file
* `./zz_site_all.R` - run `./zz_site.R` for all product versions of the sites listed in `sites_sel`. Runs in batch mode: previous outputs are deleted without prompting, and ambiguous taxonomic names are left unmatched rather than prompting the user
* `./zz_renv.R` - prepare reproducible R environment

* `./func.R` - frequently used functions

Other directories:

* `./templates/` - column definitions and templates for L0 data files and `param.yaml`
* `./network_conv/` - template scripts to convert data exported from ForestGEO, ForestPlots, and SEOSAW into L0 data files. To use one for a new site, copy it to `./sites/<SITE>/<ACQUISITION>/01_fmt.R` and check every line marked `TODO`
* `./tests/` - tests for functions in `./func.R`
* `./vignettes/` - documentation, see [Documentation](#documentation)

Key outputs from each site include:

* `./02_taxa.R`:
    * `stem_taxa.csv` - Matched taxonomic names.
    * `taxa_review.csv` - Taxonomic names which were changed or not matched, with the matching method and the number of stems affected. Check this file after processing a new site.
* `./09_stem_summ.R`: `stem_summ.gpkg` - Combined stem-level dataset. 
* `./10_quad_summ.R`: `quad_summ.gpkg` - Combined quadrat-level dataset.
* `./11_brm.R`:
    * L1
        * `*_L1_*_stem.csv` - GEO-TREES L1 stem measurement data
        * `*_L1_*_plot.csv` - GEO-TREES L1 plot meta-data 
        * `*_L1_*_pt.csv` - GEO-TREES L1 plot spatial reference points
    * L2
        * `*_L2_*_stem.csv` - GEO-TREES L2 stem AGB estimates
        * `*_L2_*_poly.gpkg` - GEO-TREES L2 plot spatial polygons
    * L3
        * `*_L3_*_quad.gpkg` - GEO-TREES L3 quadrat AGBD estimates

## Documentation

`./vignettes/worked_example/` contains a Quarto notebook which provides extensive documentation and additional explanation for each step of this data processing workflow. 

## Taxonomic name matching

`./02_taxa.R` matches taxonomic names to the [World Flora Online](https://www.worldfloraonline.org/) using `BIOMASS::correctTaxo()`. In an interactive R session, you are prompted to choose the correct name where a name has multiple possible matches. Choices are saved to `./sites/<SITE>/<ACQUISITION>/wfo_cache.rds` as you go, so they are kept if matching is interrupted, and are reused in later runs and by other product versions. Commit this file to record the choices made.

When processing in batch mode with `./zz_site_all.R`, ambiguous names are left unmatched and are not saved to the cache. To match them, run `./zz_site.R` for that site in an interactive R session.

## Environment 

This repository uses `renv` ([https://github.com/rstudio/renv](https://github.com/rstudio/renv)) to manage a reproducible R environment. This project only installs the specific packages used in the production scripts.

When you open this project for the first time, R will automatically detect the `renv` setup. If required, run `renv::restore()` to install all required dependencies (including specific versions from CRAN and GitHub).

The `./.renvignore` file specifies files and directories to be monitored for new packages. If you need to add a new package to the project:

1. Ensure the package is called in one of the tracked files or directories.
2. Install the package: `renv::install("package_name")` or `renv::install("user/repo@branch")` for packages on GitHub.
3. Update the lockfile: `renv::snapshot()`

The `BIOMASS` package is installed from the `BIOMASS_for_GEOTREES` branch on GitHub, which contains functions developed for GEO-TREES, e.g. `plotPolygonFit()`: `renv::install("umr-amap/BIOMASS@BIOMASS_for_GEOTREES")`.

## Tests

Tests for the functions in `./func.R` are in `./tests/`. Run them from the repository root:

```r
testthat::test_dir("./tests")
```

## Reproducibility

To track data inputs and versions of the processing code, each `L`-level data output is accompanied by an [RO-Crate](https://www.researchobject.org/ro-crate/) JSON file containing this meta-data. `./11_brm.R` contains code to programmatically create these files. These files can be used to precisely reproduce each data output.

A `param.yaml` file is used to track user-defined parameters for each product version of an acquisition, such as the site name, quadrat dimensions, input and output directories, the methods used to estimate stem height, wood density, and plot polygons, and author information for the RO-Crate files. A copy of this file is placed alongside each `L`-level data output. A template of `param.yaml` is located in `./templates/param.yaml`, and each parameter is described in the vignette. `./zz_site.R` checks that all parameters are present and valid before processing.

If `opt_s3` is `true`, raw data are first downloaded from the AWS S3 directory `s3_dir` to `raw_dir`. This is for processing on the [MAAP](https://docs.maap-project.org/en/hub/index.html) platform.

`./version.yaml` is used to track code versions. 


