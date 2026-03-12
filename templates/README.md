# Templates for formatting GEO-TREES L0 tree inventory data products

`./*_cols.csv` files contain column names, descriptions, units and R column classes for each of the GEO-TREES L0 tree inventory data products. These files are also used by the processing workflow to check the validity of these objects. 

* `./plot_cols.csv` - Plot meta-data table
* `./pt_cols.csv` - Plot corner coordinates table
* `./stem_cols.csv` - Stem measurements table
* `./taxon_cols.csv` - Taxonomy table

`./plot_meta.xlsx` contains a template for data owners to manually record plot-level meta-data for a site.

`./param.yaml` contains a template parameters file, read by `../zz_site.R` to process a single GEO-TREES site.

`./stem_codes.csv` contains descriptions of values which can be used in column `code` in the stem measurements table.