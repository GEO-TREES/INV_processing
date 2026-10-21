# Workflow for processing GEO-TREES tree inventory data

This repository contains code to clean and process tree inventory data from GEO-TREES sites.

Each directory in `./sites/` contains one or more acquisition sub-directories. Acquisitions are bundles of raw (L0) tree inventory data, which can be used together with a single airborne LiDAR (ALS) data acquisition to produce estimates of above-ground woody biomass density (AGBD) across the focal landscape of a site. 

A tree inventory acquisition may comprise one or more discrete data collection events, e.g. the census of a group of plots within the site during a single field campaign. Acquisitions may include data from some or all plots within a site. Acquisitions should only contain one census per plot. Acquisitions are named according to the census year, i.e. the year of the mid-date of all stem measurements within the acquisition, e.g. `2024`. 

Crucially, censuses bundled within an acquisition should generally occur within one year either side of the corresponding ALS acquisition. While this window is somewhat subjective, depending on disturbance events and seasonality, staying within this range minimises temporal mismatches that could compromise AGBD estimates.

Each acquisition directory contains:

* `./sites/<SITE>/<ACQUISITION>/01_fmt.R` - script to perform initial cleaning of raw (L0) data.
* `./sites/<SITE>/<ACQUISITION>/<VERSION>/param.yaml` - processing parameters for each product version, e.g. `v1`, `v2`. See [Reproducibility](#reproducibility).

`01_fmt.R` produces three files:

* `stem.csv` - Stem measurement table, where each row is a measurement of a stem within a single census within a plot.
* `plot.csv` - Plot metadata table, where each row is a plot. 
* `plot_pt.csv` - Points locating the corners of plots, with additional columns describing the stem map coordinate system.

and optionally:

* `taxon.csv` - Additional taxonomic information for each taxonomic name. Where a name has no matched family, e.g. stems identified only to family, `taxon_family` is used as its family, so that wood density is estimated at family level rather than from the plot mean.
* `height.csv` - Stem diameter and height measurements, used to fit a local height-diameter model.
* `wd.csv` - Local wood density measurements.
* `stem_dup.csv` - Repeat diameter measurements of stems in `stem.csv`, matched to their original measurement by `plot_id` and `stem_id`, used to estimate diameter measurement error if `diam_error` is `"field"` in `param.yaml`. Repeat measurements should be made independently of the original measurements, at the same POM, soon after the original measurements, and cover the range of stem sizes, e.g. a random sample of stems across the site rather than all stems in one quadrat. Both measurements are assumed to have the same error. Several hundred pairs are needed to estimate the proportion of large errors.

See `./templates/*` for guidance on which columns should be included in these files, and the valid values in each column. `colCheck()` and `valCheck()` in `./func.R` check these files against the templates.

Raw data and processed outputs are stored in `./dat/`, which is not tracked by git. Raw data for each acquisition are in the directory given by `raw_dir` in `param.yaml`, e.g. `./dat/sites/<SITE>/INV/<ACQUISITION>/L0/`, in a sub-directory for each version of the raw data given by `raw_version`, e.g. `L0/v1/`. When raw data are corrected, e.g. using the files in `report/`, create a new version, e.g. `L0/v2/`, and update `raw_version`, so that previous outputs can still be reproduced. Taxonomic name matching choices for each acquisition are stored with the raw data, in `./dat/sites/<SITE>/INV/<ACQUISITION>/wfo_cache.rds`, see [Taxonomic name matching](#taxonomic-name-matching).

In the root directory there are additional scripts which process data from any site:

* `./02_taxa.R` - correct taxonomic information
* `./03_wd.R` - estimate wood density for each stem measurement
* `./04_height.R` - estimate stem height for each stem measurement
* `./05_agb_stem.R` - estimate above-ground woody biomass for each stem measurement
* `./06_record_fil.R` - select stems used to estimate AGB, and AGB density. Stems below the minimum diameter threshold of a plot have their own AGB estimates, but are not part of multi-stemmed trees, and are not included in AGB density
* `./07_quad.R` - fit plot polygons and create quadrats in each plot. By default the quadrat grid is centred in each plot, so where the plot dimensions are not a multiple of the quadrat dimensions, equal strips along opposite edges of the plot are left out, up to `quad_grid_tol` of the plot area. The alignment can be changed with the optional `quad_origin` in `param.yaml`
* `./08_agb_mc.R` - AGB Monte-Carlo error propagation
* `./09_stem_summ.R` - create master stem measurement table
* `./10_quad_summ.R` - summarise quadrat measurements
* `./11_brm.R` - Create L1, L2 and L3 datasets
* `./12_report.R` - summarise outputs and processing choices for site reports

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

* `./02_taxa.R`: `stem_taxa.csv` - Matched taxonomic names.
* `./09_stem_summ.R`: `stem_summ.gpkg` - Combined stem-level dataset. `in_agb_calc` flags stems used to estimate AGB, and `in_quadrat_calc` flags stems used to estimate quadrat AGB density. Stems outside the quadrat grid are not used to estimate AGB density. Stems without coordinates in trees with other stems that have coordinates take the quadrat of the tree. 
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
* `./12_report.R`, in `report/`, tables for GEO-TREES site reports, alongside the figures written to `report/` by other steps. `large_diam_cm`, the diameter thresholds for large trees, is set in `./zz_site.R`. Settings at the top of `./12_report.R` control the tolerance for stems measured above the default POM, and the figures of the site report which the figures of each step reproduce:
    * `site_summary.csv` - One row per reported value, with units, grouped by section: plot inventory (e.g. census dates and the time from the ALS acquisition, if `als_date` is set in `param.yaml`), measurement methods (e.g. allometry, taxonomic resolution, the taxonomic level of wood density estimates and their share of AGB, stems measured above the default POM), stem measurement completeness, auxiliary datasets, AGB density with its 95% interval, large trees, data quality (e.g. excluded stems, GNSS outliers, stems outside the quadrat grid, trees without coordinates), and provenance (e.g. software and BIOMASS versions).
    * `plot_summary.csv` - The same values for each plot.
    * `census_dates.csv` - Census dates of each plot: the earliest, median and latest measurement dates of stems used to estimate AGB density, and their resolution.
    * `processing_choices.csv`, `processing_choices.md` - Choices made during processing, with the parameter which sets each one: stem selection, taxonomic name matching, the sources of wood density and height, the allometry, plot geometry, and which sources of uncertainty are propagated, and which are not.
    * `figures.csv` - Figures for the report, written to `report/` by each step, with suggested captions, and the figure of the site report which each one reproduces, if any.

## Documentation

`./vignettes/worked_example/` contains a Quarto notebook which provides extensive documentation and additional explanation for each step of this data processing workflow. 

Files to check raw data are written to `report/`, alongside the outputs of each processing step. Check these files after processing a new site, and correct the raw data if needed:

* `taxa_review.csv` - Taxonomic names which were changed or not matched, with the matching method and the number of stems affected.
* `stem_outside.csv` - Stems outside the quadrat grid of their plot, with their distance to the nearest quadrat. Stems within `outside_max_dist_m` of a quadrat, set in `param.yaml`, are assigned to the nearest quadrat. Other stems are excluded from quadrat summaries. Stems inside the plot, in strips left out of the quadrat grid, are excluded but not listed.
* `tree_unmapped.csv` - Trees where no stems have coordinates. If `unmapped_method` is `"random"`, these trees are assigned to quadrats at random during AGB error propagation. If it is `"exclude"`, they are not used to estimate AGB density.
* `diam_dup.csv` - If `diam_error` is `"field"`, repeat diameter measurements paired with their original measurements, with `prob_large`, the probability that the pair includes a large error, e.g. a data entry error. Pairs not used to estimate diameter measurement error are marked in `status`.

Figures to check the results of each step are also written to `report/`:

* `03_wd_level.png` - Wood density estimates at each taxonomic level, for stems used to estimate AGB density.
* `04_height_diam.png` - Stem height against diameter: measured heights, if any, and predicted heights drawn on top, for stems used to estimate AGB density. If `height_method` is `"field"`, with the fitted height-diameter model.
* `05_agb_diam.png` - Tree AGB against diameter, on log scales. Points far from the main cloud may be data errors.
* `05_agb_diam_class.png` - Share of AGB in each 10 cm diameter class, for trees used to estimate AGB density.
* `06_diam_hist.png` - Diameter of stems used to estimate AGB density, all plots.
* `07_plot_poly.png` - Fitted plot polygons, outlines of the GNSS corners, quadrats, and GNSS points, including outliers removed during polygon fitting, in each plot.
* `07_plot_location_site.png` - Plot locations across the site, in longitude and latitude, with the GNSS corners and fitted plot polygons.
* `07_stem_map.png` - Stem locations within the quadrat grid of each plot, with plot outlines, highlighting stems outside the plot, and stems in strips left out of the quadrat grid.
* `08_plot_agbd.png` - Plot AGB density over the quadrat grid, from error propagation with a 95% interval, and from the sum of stem AGB.
* `08_diam_error.png` - If `diam_error` is `"field"`, differences between repeat diameter measurements, with the fitted measurement error model.
* `10_quad_agbd_map.png` - Map of quadrat AGB density in each plot. Axis ranges differ between plots, with equal x and y scales.
* `10_quad_agbd_map_site.png` - Map of quadrat AGB density across the site, in longitude and latitude.
* `10_quad_agbd_hist.png` - Distribution of quadrat AGB density, all plots.
* `10_stem_agb_map.png`, `10_stem_agb_map_site.png` - Maps of stem AGB in each plot, and across the site. In the map of each plot, axis ranges differ between plots, with equal x and y scales.
* `10_quad_agbd_compare.png` - Quadrat AGB density from error propagation against the sum of stem AGB.
* `10_quad_agbd_interval.png` - Quadrat AGB density with its 95% interval from error propagation, quadrats ranked within each plot.
* `10_quad_agbd_interval_site.png` - Quadrat AGB density with its 95% interval, all quadrats of the site ranked.
* `10_quad_agbd_cv_map.png` - Map of the coefficient of variation of quadrat AGB density in each plot. Axis ranges differ between plots, with equal x and y scales.
* `10_quad_agbd_cv.png` - Coefficient of variation against quadrat AGB density.

Each step also prints summary tables to the console, e.g. the proportion of stems matched to a taxonomic name, the taxonomic level of wood density estimates, why stems are excluded from AGB estimates, the area of fitted plot polygons, and plot AGB density with its uncertainty. Warnings are given where results suggest errors in the raw data, e.g. if the area of a fitted plot polygon differs by more than 5% from the area defined by the relative coordinates of its corners. These summary tables are also written to the output directory of each step, e.g. `08_agb_mc/plot_agb.csv`, and are used by `./12_report.R`. `./zz_site.R` lists all files in `report/` at the end of a run.

## Taxonomic name matching

`./02_taxa.R` matches taxonomic names to the [World Flora Online](https://www.worldfloraonline.org/) using `BIOMASS::correctTaxo()`. In an interactive R session, you are prompted to choose the correct name where a name has multiple possible matches. Choices are saved to `wfo_cache.rds`, in the directory containing `raw_dir`, as you go, so they are kept if matching is interrupted, and are reused in later runs and by other product versions. A copy is written to the outputs of `./02_taxa.R`.

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

A `param.yaml` file is used to track user-defined parameters for each product version of an acquisition, such as the site name, quadrat dimensions, input and output directories, the methods used to estimate stem height, wood density, and plot polygons, which stems are included in AGB density, how multi-stemmed trees and trees without coordinates are treated, the diameter measurement error, and author information for the RO-Crate files. A copy of this file is placed alongside each `L`-level data output. A template of `param.yaml` is located in `./templates/param.yaml`, and each parameter is described in the vignette. `./zz_site.R` checks that all parameters are present and valid before processing.

If `opt_s3` is `true`, raw data are first downloaded from the AWS S3 directory `s3_dir` to `raw_dir`. This is for processing on the [MAAP](https://docs.maap-project.org/en/hub/index.html) platform.

`./version.yaml` is used to track code versions. 


