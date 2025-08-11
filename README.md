# Workflows for processing GEO-TREES plot data

This repository contains code to clean and process tree-inventory data from GEO-TREES sites

* `./func.R` - frequently used functions.
* `./wd_prep.R` - preparation of wood density dataset
* `./zz_site_run.R` - run scripts in order to process a single site
* `./05_wd.R` - extract wood density data for each measurement
* `./06_height.R` - estimate stem height for each measurement
* `./07_biomass.R` - estimate woody biomass for each measurement
* `./08_stem_out.R` - create master stem measurement table
* `./09_sub_summ.R` - summarise subplot measurements

Each directory in `./sites/` contains additional scripts to clean data from each GEO-TREES site:

* `./*/01_polys.R` - create plot polygons
* `./*/02_stem_fmt.R` - clean stem measurement data
* `./*/03_taxa.R` - correct taxonomic information
* `./*/04_subplots.R` - create subplots in each plot

Install packages from non-CRAN sources:

```r
remotes::install_github('umr-amap/BIOMASS')
```

To convert R scripts to Jupyter notebooks for deployment on MAAP:

```sh
jupyter nbconvert --to script *.ipynb
```

To prevent committing the outputs of Jupyter notebooks to this repository, add the following to `./git/hooks/pre-commit`:

```sh
#!/bin/bash

for f in $(git diff --name-only --cached); do
    if [[ $f == *.ipynb ]]; then
        jupyter nbconvert --clear-output --inplace $f
        git add $f
    fi
done

if git diff --name-only --cached --exit-code
then
    echo "No changes detected after removing notebook output"
    exit 1
fi
```

Outputs from each site include:

* TODO: `plots.csv` - Plot metadata table, where each row is a plot. 
* TODO: `census.csv` - Census metadata table, where each row is a census within a plot. 
* `stems_all.gpkg` - Stem measurements table, where each row is a stem measurement. 
* `polys.gpkg` - Plot polygons.
* `pts.gpkg` - Points locating the corners of plots, with additional columns describing the stem map coordinate system.
* `polys_sub.gpkg` - Polygons of 50x50 m (0.25 ha) subplots.
* `pts_sub.gpkg` - Points locating the corners of 50x50 m (0.25 ha) subplots.
* `sub_summ.csv` - Subplot summary statistics.
