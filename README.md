# Workflows for processing GEO-TREES plot data

This repository contains code to clean and process tree-inventory data from GEO-TREES sites

Each directory in `./sites/` contains scripts to clean data from a single GEO-TREES site.

* `./func.R` - frequently used functions.
* `00_env.R` - packages to be installed from non-CRAN sources
* `01_wd.R` - preparation of wood density dataset

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

* `plots.csv` - Plot metadata table, where each row is a plot. 
* `census.csv` - Census metadata table, where each row is a census within a plot. 
* `stems.csv` - Stem measurements table, where each row is a stem measurement. 
* `polys.gpkg` - Plot polygons.
* `pts.gpkg` - Points locating the corners of plots, with additional columns describing the stem map coordinate system.
* `polys_sub.gpkg` - Polygons of 50x50 m (0.25 ha) subplots.
* `pts_sub.gpkg` - Points locating the corners of 50x50 m (0.25 ha) subplots.
* `sub_summ.csv` - Subplot summary statistics.
