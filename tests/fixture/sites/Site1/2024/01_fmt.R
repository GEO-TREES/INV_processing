# Format the example data of the worked example, used to test the pipeline
# from end to end, see ./tests/test_runSite.R
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2026-11-27

# The example data, ./vignettes/worked_example/data-raw/data/, are already in
# the GEO-TREES format, so this script only checks them and writes them to
# `outdir`, under the file names expected by the other steps

# Read a table with the column classes of its template
readFmt <- function(file, col_class) {
  read.csv(file.path(indir, file), colClasses = col_class)
}

stem <- readFmt("stem.csv", stem_col_class)
plot <- readFmt("plot.csv", plot_col_class)
plot_pt <- readFmt("pt.csv", pt_col_class)
height <- readFmt("hd.csv", height_col_class)
wd <- readFmt("wd.csv", wd_col_class)
subplot <- readFmt("subplot.csv", subplot_col_class)

# Check all columns in output objects
colCheck(plot, plot_cols)
colCheck(plot_pt, pt_cols)
colCheck(stem, stem_cols)
colCheck(wd, wd_cols)
colCheck(height, height_cols)
colCheck(subplot, subplot_cols)

# Check values
valCheck(
  plot = plot, 
  stem = stem, 
  wd = wd,
  pt = plot_pt,
  height = height,
  subplot = subplot)

# Write to file
write.csv(plot_pt, file.path(outdir, "plot_pt.csv"), row.names = FALSE)
write.csv(plot, file.path(outdir, "plot.csv"), row.names = FALSE)
write.csv(stem, file.path(outdir, "stem.csv"), row.names = FALSE)
write.csv(height, file.path(outdir, "height.csv"), row.names = FALSE)
write.csv(wd, file.path(outdir, "wd.csv"), row.names = FALSE)
write.csv(subplot, file.path(outdir, "subplot.csv"), row.names = FALSE)
