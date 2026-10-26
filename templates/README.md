# Templates for formatting GEO-TREES L0 tree inventory data products

`./*_cols.csv` files contain column names, descriptions, units and R column classes for each of the GEO-TREES L0 tree inventory data products. These files are also used by the processing workflow to check the validity of these objects. 

* `./plot_cols.csv` - Plot meta-data table
* `./pt_cols.csv` - Plot corner coordinates table
* `./stem_cols.csv` - Stem measurements table
* `./taxon_cols.csv` - Taxonomy table
* `./height_cols.csv` - Optional height-diameter measurements table
* `./wd_cols.csv` - Optional wood density measurements table
* `./stem_dup_cols.csv` - Optional repeat diameter measurements table, used to estimate diameter measurement error
* `./subplot_cols.csv` - Optional field subplot bounds table, used to locate stems without coordinates within their field subplot

`./plot_meta.xlsx` contains a template for data owners to manually record plot-level meta-data for a site.

`./param.yaml` contains a template parameters file, read by `../zz_site.R` to process a single GEO-TREES site, with the default values of optional parameters, and commented examples of optional parameters with several values, e.g. `height_custom` and `crown_custom`. Each parameter is described in the vignette.

`./stem_codes.csv` contains descriptions of values which can be used in column `code` in the stem measurements table.