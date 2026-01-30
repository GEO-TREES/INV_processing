# Workflows for processing GEO-TREES plot data

This repository contains code to clean and process tree-inventory data from GEO-TREES sites

* `./func.R` - frequently used functions.
* `./wd_prep.R` - preparation of wood density dataset
* `./zz_site.R` - run scripts in order to process a single site
* `./04_sub.R` - create subplots in each plot
* `./05_wd.R` - estimate wood density for each stem measurement
* `./06_height.R` - estimate stem height for each stem measurement
* `./07_agb.R` - estimate above-ground woody biomass for each stem measurement
* `./08_stem_summ.R` - create master stem measurement table
* `./09_sub_summ.R` - summarise subplot measurements

Each directory in `./sites/` contains additional scripts to clean data from each GEO-TREES site:

* `./*/01_plot.R` - create plot polygons, format plot metadata
* `./*/02_stem.R` - clean stem measurement data
* `./*/03_taxa.R` - correct taxonomic information
 
Key outputs from each site include:

* `01_plot.R`:
    * `plot.csv` - Plot metadata table, where each row is a plot. 
    * `plot_poly.gpkg` - Plot polygons.
    * `plot_pt.gpkg` - Points locating the corners of plots, with additional columns describing the stem map coordinate system.
* `08_stem_summ.R`:
    * `stem_summ.gpkg` - Combined stem-level dataset
* `09_sub_summ.R`:
    * `sub_summ.gpkg` - Combined subplot-level dataset


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

