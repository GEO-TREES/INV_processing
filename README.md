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
* `wd.csv` - Local wood density measurements, used if `wd_method` is `"field"` or `"local"` in `param.yaml`.
* `stem_dup.csv` - Repeat diameter measurements of stems in `stem.csv`, matched to their original measurement by `plot_id` and `stem_id`, used to estimate diameter measurement error if `diam_error` is `"field"` in `param.yaml`. Repeat measurements should be made independently of the original measurements, at the same POM, soon after the original measurements, and cover the range of stem sizes, e.g. a random sample of stems across the site rather than all stems in one quadrat. Both measurements are assumed to have the same error. Several hundred pairs are needed to estimate the proportion of large errors.
* `subplot.csv` - Bounds of field subplots in the relative coordinate system of each plot, one row per subplot, matched to stems by `plot_id` and `subplot_id`. Used to place stems without coordinates within their field subplot during AGB error propagation, see [Missing measurements](#missing-measurements).

See `./templates/*` for guidance on which columns should be included in these files, and the valid values in each column. `colCheck()` and `valCheck()` in `./func.R` check these files against the templates.

Raw data and processed outputs are stored in `./dat/`, which is not tracked by git. Raw data for each acquisition are in the directory given by `raw_dir` in `param.yaml`, e.g. `./dat/sites/<SITE>/INV/<ACQUISITION>/L0/`, in a sub-directory for each version of the raw data given by `raw_version`, e.g. `L0/v1/`. When raw data are corrected, e.g. using the files in `report/`, create a new version, e.g. `L0/v2/`, and update `raw_version`, so that previous outputs can still be reproduced. Taxonomic name matching choices for each acquisition are stored with the raw data, in `./dat/sites/<SITE>/INV/<ACQUISITION>/wfo_cache.csv`, see [Taxonomic name matching](#taxonomic-name-matching).

In the root directory there are additional scripts which process data from any site:

* `./02_taxa.R` - correct taxonomic information
* `./03_wd.R` - estimate wood density for each stem measurement, from the Global Wood Density Database v2 (`wd_method: "regional"`), local measurements in `wd.csv` with the Global Wood Density Database v2 for other taxa (`"field"`), or local measurements only (`"local"`)
* `./04_height.R` - estimate stem height for each stem measurement, from a regional (`height_method: "regional"`), locally fitted (`"field"`) or published (`"custom"`, given in `height_custom`) height-diameter model. `height_custom` takes the same forms as `agb_custom`: an equation, optionally with `draws` or `vcov` to propagate the uncertainty in its parameters, or a model fitted with `brms` and saved as an `.rds` file, see `heightCustom()` in `./func.R`. This needs `BIOMASS::customHD()` with several sets of parameter values. A locally fitted model is a Bayesian model, fitted with `brms`, so that the uncertainty in its parameters is propagated to AGB as well as its residual error, see [Bayesian height-diameter models](#bayesian-height-diameter-models). Optionally, diameters of non-palm stems measured above 1.3 m are first corrected to their equivalent at 1.3 m with an exponential taper model (`taper_method`: `"cushman2014"`, Barro Colorado Island only, or `"cushman2021"`). Corrected diameters are used to estimate height and AGB, and measured diameters for diameter thresholds
* `./05_agb_stem.R` - estimate above-ground woody biomass for each stem measurement. If `stem_allometry` is `true`, stems use the allometry given in `agb_allometry` in the stem table, e.g. `"goodman2013"` for palms, see `agb_allometries` in `./func.R`. The optional `agb_method` follows `height_method`: with `"regional"` (default) trees use Chave et al. (2014), and with `"custom"` all trees without their own allometry use the one in `agb_custom`: either an equation (`formula`, `parameters`, `sigma`, `link`; with `link: "log"` the formula gives the median AGB, so coefficients which already include the correction exp(sigma^2 / 2) must be given without it), or a model fitted with `brms` and saved as an `.rds` file (`model`, `variables`), both with `units` and `reference`, see `agbCustom()` and `agbAllomBrms()` in `./func.R`. For a `brms` model, each Monte Carlo simulation in `./08_agb_mc.R` uses one posterior draw of its parameters for all trees, so the uncertainty in its parameters is propagated. An equation with one set of `parameters` propagates only its residual error; give `draws`, a `.csv` file of sets of parameter values, or `vcov`, the covariance matrix of the estimates, to propagate the uncertainty in its coefficients too. Allometries for individual stems can also be defined in `param.yaml`, in `stem_allometry_custom`, a named list in the format of `agb_custom`
* `./06_record_fil.R` - select stems used to estimate AGB, and AGB density. Run after `./01_fmt.R`, so that the selection is made once and used by every other step. Stems used to estimate AGB density must be alive and standing, and not broken below the POM. Dead stems and fallen stems with a diameter measurement have their own AGB estimates, as for living, standing stems, i.e. with no reduction for lost branches, leaves or decayed wood, but are not included in AGB density. Stems below the minimum diameter threshold of a plot, and stems of growth forms in the optional `growth_form_exclude`, e.g. strangler figs, have their own AGB estimates, but are not part of multi-stemmed trees, and are not included in AGB density
* `./07_quad.R` - fit plot polygons and create quadrats in each plot. By default the quadrat grid is centred in each plot, so where the plot dimensions are not a multiple of the quadrat dimensions, equal strips along opposite edges of the plot are left out, up to `quad_grid_tol` of the plot area. The alignment can be changed with the optional `quad_origin` in `param.yaml`
* `./08_agb_mc.R` - AGB Monte-Carlo error propagation. If the optional `corner_error` in `param.yaml` is `"residual"` or a standard deviation in m, the error of the GNSS coordinates of plot corners is propagated to quadrat area: `./07_quad.R` shifts each plot corner by a random error in each simulation with `BIOMASS::divide_plot(sd_coord = )`, and the AGB of each quadrat is divided by its area in that simulation. With `"residual"` the standard deviation is estimated from the residuals of the GNSS points around the fitted plot polygons, pooled across the plots of the site, see `cornerErrorSD()` in `./func.R`. Optionally (`crown_method: "custom"`), quadrat AGB density is also estimated with the AGB of each tree spread over its crown, a circle with area from the crown area allometry in `crown_custom`, with its residual error, in the same simulations. Crowns crossing a quadrat edge are split among quadrats, and crowns outside the quadrat grid are folded back into the grid, so plot AGB density is unchanged
* `./09_stem_summ.R` - create master stem measurement table
* `./10_quad_summ.R` - summarise quadrat measurements
* `./11_brm.R` - Create L1, L2 and L3 datasets
* `./12_report.R` - summarise outputs and processing choices for site reports

* `./run.R` - functions to run the steps: `runSite()` for a single product version of an acquisition, and `runSiteAll()` for many. Also run from the command line, see [Running the pipeline](#running-the-pipeline)
* `./00_param.R` - read and check `param.yaml`, run by `runSite()` before the steps
* `./zz_renv.R` - prepare reproducible R environment

* `./func.R` - frequently used functions

Other directories:

* `./templates/` - column definitions and templates for L0 data files and `param.yaml`
* `./network_conv/` - template scripts to convert data exported from ForestGEO, ForestPlots, and SEOSAW into L0 data files. To use one for a new site, copy it to `./sites/<SITE>/<ACQUISITION>/01_fmt.R` and check every line marked `TODO`
* `./tests/` - tests for functions in `./func.R` and `./run.R`, including a run of the whole pipeline on example data
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
        * `*_L3_*_quad.gpkg` - GEO-TREES L3 quadrat AGBD estimates: the mean (`AGBD_stand_estimate`), median (`AGBD_stand_median`, comparable with `medAGB` of `BIOMASS::AGBmonteCarlo()`), standard deviation (`AGBD_stand_uncertainty`) and 95% interval (`AGBD_stand_ci_lower`, `AGBD_stand_ci_upper`, the 2.5% and 97.5% quantiles) of the simulations. With crown-distributed AGBD (`AGBD_crown_estimate`, `AGBD_crown_median`, `AGBD_crown_uncertainty`, `AGBD_crown_ci_lower`, `AGBD_crown_ci_upper`) if `crown_method` is `"custom"`. Processing parameters are included as columns, one per parameter
* `./12_report.R`, in `report/`, tables for GEO-TREES site reports, alongside the figures written to `report/` by other steps. `large_diam_cm`, the diameter thresholds for large trees, is set in `./00_param.R`. Settings at the top of `./12_report.R` control the tolerance for stems measured above the default POM, and the figures of the site report which the figures of each step reproduce:
    * `site_summary.csv` - One row per reported value, with units, grouped by section: plot inventory (e.g. census dates and the time from the ALS acquisition, if `als_date` is set in `param.yaml`), measurement methods (e.g. allometry, taxonomic resolution, the taxonomic level of wood density estimates and their share of AGB, stems measured above the default POM), stem measurement completeness, auxiliary datasets, AGB density with its 95% interval, large trees, data quality (e.g. excluded stems, GNSS outliers, stems outside the quadrat grid, trees without coordinates), and provenance (e.g. software and BIOMASS versions).
    * `plot_summary.csv` - The same values for each plot.
    * `census_dates.csv` - Census dates of each plot: the earliest, median and latest measurement dates of stems used to estimate AGB density, and their resolution.
    * `processing_choices.csv`, `processing_choices.md` - Choices made during processing, with the parameter which sets each one: stem selection, taxonomic name matching, the sources of wood density and height, the allometry, plot geometry, and which sources of uncertainty are propagated, and which are not.
    * `figures.csv` - Figures for the report, written to `report/` by each step, with suggested captions, and the figure of the site report which each one reproduces, if any.

## Running the pipeline

Run from the repository root. From the command line, with the path to one or more `param.yaml` files, the names of sites, or `--all` for all sites. `--clean` deletes the outputs of previous runs of this software version first, except the cache of taxonomic name matches. The exit status is not zero if a run fails:

```bash
Rscript run.R ./sites/Paracou/2019/v1/param.yaml --clean
```

```bash
Rscript run.R --sites=Bicuar,Amacayacu --clean
```

In an R session:

```r
source("./run.R")

# One product version of an acquisition
runSite("./sites/Paracou/2019/v1/param.yaml", clean = TRUE)

# Repeat AGB error propagation and the steps after it, using the outputs of 
# earlier steps from a previous run
runSite("./sites/Paracou/2019/v1/param.yaml", from = "08_agb_mc")

# All product versions of some sites, or of all sites if `sites` is not given
runs <- runSiteAll(sites = c("Bicuar", "Amacayacu"))
```

`runSite()` does not prompt, and writes nothing to the global environment, so it can be used in scripts and scheduled jobs. It returns the parameters, the directories of the L1, L2 and L3 datasets, and the run time of each step. `runSiteAll()` carries on if one run fails, and returns a table with the status and error message of each run.

Each step is a script, run in its own environment with the parameters, the functions in `./func.R`, its output directory `outdir`, and the files it reads from earlier steps, which are listed in `stepInputs()` in `./run.R`. A step can't use an object created by another step unless it is written to file.

## Documentation

`./vignettes/worked_example/` contains a Quarto notebook which provides extensive documentation and additional explanation for each step of this data processing workflow. 

Files to check raw data are written to `report/`, alongside the outputs of each processing step. Check these files after processing a new site, and correct the raw data if needed:

* `taxa_review.csv` - Taxonomic names which were changed or not matched, with the matching method and the number of stems affected.
* `stem_outside.csv` - Stems outside the quadrat grid of their plot, with their distance to the nearest quadrat. Stems within `outside_max_dist_m` of a quadrat, set in `param.yaml`, are assigned to the nearest quadrat. Other stems are excluded from quadrat summaries. Stems inside the plot, in strips left out of the quadrat grid, are excluded but not listed.
* `stem_outside_subplot.csv` - If the site supplies `subplot.csv`, stems with coordinates outside the bounds of their field subplot, with their distance to the nearest edge of the subplot, and the subplot whose bounds contain them, if any. Also lists stems in field subplots missing from `subplot.csv`. Either the coordinates or the field subplot of these stems may be wrong. A summary of each field subplot is written to `07_quad/subplot_check.csv`.
* `tree_unmapped.csv` - Trees where no stems have coordinates, with the bounds they are placed within during AGB error propagation, and where the bounds come from (`location`), see [Missing measurements](#missing-measurements).
* `diam_imputed.csv` - Stems without a diameter measurement, whose diameter is imputed during AGB error propagation, see [Missing measurements](#missing-measurements).
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
* `07_subplot_map.png` - If the site supplies `subplot.csv`, field subplots in each plot, labelled with their ID and filled by the percentage of stems with coordinates outside their bounds, with these stems joined to the nearest edge of their subplot. Axis ranges differ between plots, with equal x and y scales.
* `08_subplot_unmapped_map.png` - If the site supplies `subplot.csv`, the AGB of trees without coordinates placed within each field subplot, per hectare of the subplot, labelled with the number of trees. Axis ranges differ between plots, with equal x and y scales.
* `08_plot_agbd.png` - Plot AGB density over the quadrat grid, from error propagation with a 95% interval, and from the sum of stem AGB.
* `08_diam_error.png` - If `diam_error` is `"field"`, differences between repeat diameter measurements, with the fitted measurement error model.
* `08_crown_map.png` - If `crown_method` is `"custom"`, tree crowns in each plot, as translucent circles with the median crown area from the crown area allometry, filled by tree AGB, with quadrats. Axis ranges differ between plots, with equal x and y scales.
* `08_quad_agbd_crown_diff_map.png` - If `crown_method` is `"custom"`, crown-distributed quadrat AGB density minus quadrat AGB density with each tree in the quadrat containing its stems, in each plot. Axis ranges differ between plots, with equal x and y scales.
* `10_quad_agbd_map.png` - Map of quadrat AGB density in each plot. Axis ranges differ between plots, with equal x and y scales.
* `10_quad_agbd_map_site.png` - Map of quadrat AGB density across the site, in longitude and latitude.
* `10_quad_agbd_hist.png` - Distribution of quadrat AGB density, all plots.
* `10_stem_agb_map.png`, `10_stem_agb_map_site.png` - Maps of stem AGB in each plot, and across the site. In the map of each plot, axis ranges differ between plots, with equal x and y scales.
* `10_quad_agbd_compare.png` - Quadrat AGB density from error propagation against the sum of stem AGB.
* `10_quad_agbd_interval.png` - Quadrat AGB density with its 95% interval from error propagation, quadrats ranked within each plot.
* `10_quad_agbd_interval_site.png` - Quadrat AGB density with its 95% interval, all quadrats of the site ranked.
* `10_quad_agbd_cv_map.png` - Map of the coefficient of variation of quadrat AGB density in each plot. Axis ranges differ between plots, with equal x and y scales.
* `10_quad_agbd_cv.png` - Coefficient of variation against quadrat AGB density.

Each step also prints summary tables to the console, e.g. the proportion of stems matched to a taxonomic name, the taxonomic level of wood density estimates, why stems are excluded from AGB estimates, the area of fitted plot polygons, and plot AGB density with its uncertainty. Warnings are given where results suggest errors in the raw data, e.g. if the area of a fitted plot polygon differs by more than 5% from the area defined by the relative coordinates of its corners. These summary tables are also written to the output directory of each step, e.g. `08_agb_mc/plot_agb.csv`, and are used by `./12_report.R`. `runSite()` lists all files in `report/` at the end of a run.

## Missing measurements

Live stems without a diameter measurement, or without coordinates, are kept, and the uncertainty in their size or location is propagated to quadrat AGBD in `./08_agb_mc.R`, using `BIOMASS`:

* Stems without a diameter, if the optional `missing_diam_method` in `param.yaml` is `"impute"` (default), are assumed to be at or above the minimum diameter threshold, as they were recorded in the census. In each simulation, `BIOMASS::AGBmonteCarlo()` replaces the diameter of each of these stems with a simulated diameter drawn at random from the other trees in the same plot used to estimate AGB, which can include trees below the minimum diameter threshold for AGB density. For very large plots, split into groups of quadrats, the pool is the group of quadrats. Their height and AGB are estimated from the imputed diameter. These stems have no deterministic AGB estimate in `./05_agb_stem.R`. With `"exclude"`, these stems are excluded.
* Stems without coordinates in trees where other stems have coordinates are given the mean coordinates of the other stems of the tree in `./07_quad.R`.
* Trees where no stems have coordinates, if `unmapped_method` is `"random"`, are placed at a random location in each simulation by `BIOMASS::subplot_summary(tree_bounds = )`, and their AGB is added to the quadrat containing that location. The location is drawn within the bounds of the field subplot of the tree (`subplot_id` in the stem table), from the optional `subplot.csv`, or, if the site does not supply it, from the extent of the stems with coordinates in the same field subplot, if there are at least 10 of them. Otherwise the location is drawn within the plot. Placements in strips of the plot left out of the quadrat grid are not included in that simulation. With `"exclude"`, these trees are excluded from AGB density.

## Bayesian height-diameter models

If `height_method` is `"field"`, `./04_height.R` compares four forms of height-diameter model, fitted by least squares with `BIOMASS::modelHD()`, and uses the form set by `height_model`, by default the form with the lowest residual standard error. That form is then fitted as a Bayesian model with `BIOMASS::modelHD(bayesian = TRUE)`, which uses the `brms` package and Stan. In each simulation of `./08_agb_mc.R`, `BIOMASS::AGBmonteCarlo()` then uses one posterior draw of the model parameters for all trees, so the uncertainty in the parameters is propagated to AGB, as well as the residual error of the model. A model fitted by least squares would only propagate its residual error, so a locally fitted model is always Bayesian. Published models (`height_method: "custom"`) have fixed parameters, so only their residual error is propagated.

The Bayesian model:

* needs the `brms` package and a C++ toolchain to compile Stan models. If `height_method` is `"field"`, `runSite()` stops before processing if `brms` is not installed. The compiled model of each form is kept in the `BIOMASS` cache, `./dat/BIOMASS/cache/`, so it is only compiled once
* is fitted with a fixed random seed, 3 chains and 2700 posterior draws, the defaults of `BIOMASS::modelHD()`
* must converge, i.e. the maximum Rhat of its parameters must be at or below 1.05. The parameters of the Weibull and Michaelis-Menten models can be poorly identified. If `height_model` is `"best"` and the Bayesian model of the form with the lowest residual standard error does not converge, the form with the next lowest is tried, with a warning. If `height_model` names a form which does not converge, or no form converges, processing stops: set `height_model` to another form, e.g. `"log2"`
* uses the default priors of `BIOMASS::modelHD()`, except for the Weibull model, H = a (1 - exp(-(D / b)^c)), which does not converge with them. It is given the bounded uniform priors suggested in the `BIOMASS` vignette: a between 0 and 80 m, or 1.5 times the tallest measured stem if that is more, b between 0 and 100 cm, or the largest measured diameter if that is more, and c between 0.01 and 0.99, see `heightBayes()` in `./func.R`

The form of model used, its number of posterior draws, and the maximum Rhat of each Bayesian model fitted, are written to `04_height/height_mod_comp.csv` and to `report/processing_choices.md`. Predicted heights in the outputs are the median prediction of the posterior draws.

The uncertainty in the parameters matters most where there are few height measurements. In a test with the Bicuar 2024 data, the standard deviation of plot AGB density did not change with all 7569 height measurements, and was 1.6 times larger in the median plot with a random sample of 150.

## Taxonomic name matching

`./02_taxa.R` matches taxonomic names to the [World Flora Online](https://www.worldfloraonline.org/) using `BIOMASS::correctTaxo()`. In an interactive R session, you are prompted to choose the correct name where a name has multiple possible matches. Choices are saved to `wfo_cache.csv`, a table with one row per submitted name, in the directory containing `raw_dir`, as you go, so they are kept if matching is interrupted, and are reused in later runs and by other product versions. A copy is written to the outputs of `./02_taxa.R`.

When processing in batch mode, with `runSiteAll()`, `runSite(batch = TRUE)` or from the command line, ambiguous names are left unmatched and are not saved to the cache. To match them, run `runSite()` for that site in an interactive R session.

## Environment 

This repository uses `renv` ([https://github.com/rstudio/renv](https://github.com/rstudio/renv)) to manage a reproducible R environment. This project only installs the specific packages used in the production scripts.

When you open this project for the first time, R will automatically detect the `renv` setup. If required, run `renv::restore()` to install all required dependencies (including specific versions from CRAN and GitHub).

The `./.renvignore` file specifies files and directories to be monitored for new packages. If you need to add a new package to the project:

1. Ensure the package is called in one of the tracked files or directories.
2. Install the package: `renv::install("package_name")` or `renv::install("user/repo@branch")` for packages on GitHub.
3. Update the lockfile: `renv::snapshot()`

The `BIOMASS` package is installed from the `BIOMASS_for_GEOTREES` branch on GitHub, which contains functions developed for GEO-TREES, e.g. `plotPolygonFit()`: `renv::install("umr-amap/BIOMASS@BIOMASS_for_GEOTREES")`. Published height-diameter models (`height_method: "custom"`) and the AGB allometries of individual stems (`stem_allometry`) need `BIOMASS::customHD()` and user-defined allometries in `BIOMASS::AGBmonteCarlo()`, from the `user_allom_custom_hd` branch, until it is merged into `BIOMASS_for_GEOTREES`.

## Tests

Tests for the functions in `./func.R` and `./run.R` are in `./tests/`. Run them from the repository root:

```r
testthat::test_dir("./tests")
```

`./tests/test_runSite.R` runs the whole pipeline on the example data of the worked example, one 25 ha plot with 5000 stems, with the format script and parameters in `./tests/fixture/`, and checks the L1, L2 and L3 datasets. It takes about one minute. Set the environment variable `INV_TEST_SKIP_RUN` to skip it.

## Reproducibility

To track data inputs and versions of the processing code, each `L`-level data output is accompanied by an [RO-Crate](https://www.researchobject.org/ro-crate/) JSON file containing this meta-data. `./11_brm.R` contains code to programmatically create these files. These files can be used to precisely reproduce each data output.

A `param.yaml` file is used to track user-defined parameters for each product version of an acquisition, such as the site name, quadrat dimensions, input and output directories, the methods used to estimate stem height, wood density, and plot polygons, taper correction, the AGB allometries of individual stems, which stems are included in AGB density, how multi-stemmed trees and trees without coordinates are treated, the diameter measurement error, crown-distributed AGB density, and author information for the RO-Crate files. A copy of this file is placed alongside each `L`-level data output. A template of `param.yaml` is located in `./templates/param.yaml`, and each parameter is described in the vignette. `./00_param.R` checks that all parameters are present and valid before processing.

If `opt_s3` is `true`, raw data are first downloaded from the AWS S3 directory `s3_dir` to `raw_dir`. This is for processing on the [MAAP](https://docs.maap-project.org/en/hub/index.html) platform.

`./version.yaml` is used to track code versions. 


