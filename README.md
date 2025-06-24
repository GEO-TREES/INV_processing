# Workflows for processing GEO-TREES plot data

Each R script (`*.R`) is designed to run on a single site containing multiple plots.

* `01_subplot_polys.R` - creates `subplot_polys.gpkg`, containing polygons defining subplots within plots. 
* `02_subplot_summ.R` - creates `subplot_summ.csv`, containing summary statistics generated for each subplot.

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

