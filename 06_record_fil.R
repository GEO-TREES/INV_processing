# Filter stem data for quadrat summaries
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2026-10-14

# Select stems used to estimate AGB density, see `recordFilter()` in
# ./func.R. Stems without coordinates are kept, and are assigned to quadrats
# at random during AGB error propagation in ./08_agb_mc.R
record_fil <- recordFilter(stem, plot)

# Write filtered stem data to file
writeLines(record_fil, file.path(outdir, "record_fil.txt"))
