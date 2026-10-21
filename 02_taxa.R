# Correct taxonomic names
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2026-10-21

# Choices made during name matching are stored in a World Flora Online (WFO)
# cache, `wfo_path`, shared by all product versions of an acquisition. It is
# stored with the raw data, in the directory containing `raw_dir`.
#
# Ambiguous names are only offered for manual matching in interactive
# sessions. In batch runs (`options(inv.batch = TRUE)`, set by
# ./zz_site_all.R) they are left unmatched and are not saved to the cache, so
# they can be resolved in a later interactive run.

# Prompt the user only in interactive sessions
taxa_interactive <- interactive() && !isTRUE(getOption("inv.batch"))

# Combine taxa across tables
taxon_vec <- sort(unique(c(
  stem$taxon_name,
  wd$taxon_name,
  height$taxon_name)))

# Match names in chunks, saving the cache after each chunk so that choices
# are kept if interactive matching is interrupted
chunk_size <- 25
taxon_chunks <- split(taxon_vec, ceiling(seq_along(taxon_vec) / chunk_size))
taxa_list <- list()
for (i in seq_along(taxon_chunks)) {
  taxa_list[[i]] <- correctTaxo(
    genus = taxon_chunks[[i]],
    species = NULL,
    interactive = taxa_interactive,
    preferAccepted = TRUE,
    preferFuzzy = FALSE,
    sub_pattern = subPattern(),
    useCache = TRUE,
    useAPI = TRUE,
    capacity = 60,
    fill_time_s = 1,
    timeout = 60)

  saveWFOCache(wfo_path, rm_empty = !taxa_interactive)
}
taxa <- bind_rows(taxa_list)

# Fill the family of names without a matched family, e.g. stems identified
# only to family, from the optional taxon table, so that wood density can be
# estimated at family level
if (!is.null(taxon)) {
  taxon_family <- trimws(taxon$taxon_family[match(taxa$nameOriginal, taxon$taxon_name)])
  family_fill <- is.na(taxa$familyAccepted) & !is.na(taxon_family) & taxon_family != ""
  taxa$familyAccepted[family_fill] <- taxon_family[family_fill]
  message("Family filled from taxon table for ", sum(family_fill), " names")
}

# Create table of names which were changed or not matched, for review
taxa_method <- vapply(taxa$nameSubmitted, function(x) {
  m <- BIOMASS::getWFOCache()[[x]]$method
  if (is.null(m) || is.na(x)) NA_character_ else m
}, character(1), USE.NAMES = FALSE)

# Names are compared ignoring case and repeated whitespace
nameStd <- function(x) tolower(trimws(gsub("\\s+", " ", x)))

taxa_review <- taxa %>%
  mutate(
    method = taxa_method,
    n_stem = as.vector(table(stem$taxon_name)[nameOriginal]),
    n_stem = ifelse(is.na(n_stem), 0L, n_stem)) %>%
  filter(
    !is.na(nameOriginal), nameOriginal != "",
    is.na(nameAccepted) | nameStd(nameOriginal) != nameStd(nameAccepted)) %>%
  dplyr::select(nameOriginal, nameAccepted, familyAccepted, method, n_stem) %>%
  arrange(is.na(nameAccepted), desc(n_stem))

message(
  "Taxonomic names: ", nrow(taxa), " total, ",
  sum(!is.na(taxa_review$nameAccepted)), " changed, ",
  sum(is.na(taxa_review$nameAccepted)), " not matched (",
  sum(taxa_review$n_stem[is.na(taxa_review$nameAccepted)]), " stems). ",
  "See ", file.path(report_dir, "taxa_review.csv"))

# Summarise matching outcomes by number of stems, to check whether unmatched
# names affect many stems
taxa_outcome <- taxa %>%
  filter(!is.na(nameOriginal), nameOriginal != "") %>%
  mutate(
    n_stem = as.vector(table(stem$taxon_name)[nameOriginal]),
    outcome = case_when(
      !is.na(nameAccepted) & nameStd(nameOriginal) == nameStd(nameAccepted) ~ "matched, unchanged",
      !is.na(nameAccepted) ~ "matched, changed",
      !is.na(familyAccepted) ~ "family only",
      TRUE ~ "not matched"),
    outcome = factor(outcome, levels = c("matched, unchanged",
      "matched, changed", "family only", "not matched"))) %>%
  group_by(outcome, .drop = FALSE) %>%
  summarise(n_name = n(), n_stem = sum(n_stem, na.rm = TRUE), .groups = "drop") %>%
  mutate(pct_stem = round(100 * n_stem / sum(n_stem), 1))
msgTable(taxa_outcome, "Taxonomic name matching:")
write.csv(taxa_outcome, file.path(outdir, "taxa_outcome.csv"), row.names = FALSE)

taxa_unmatched <- filter(taxa_review, is.na(nameAccepted))
if (nrow(taxa_unmatched) > 0) {
  msgTable(dplyr::select(taxa_unmatched, nameOriginal, familyAccepted, n_stem),
    "Names not matched, most stems first:", max_rows = 10)
}

# Write stem taxonomic information to file
write.csv(taxa, file.path(outdir, "stem_taxa.csv"), row.names = FALSE)

# Write names for review to file, to check raw data
write.csv(taxa_review, file.path(report_dir, "taxa_review.csv"), row.names = FALSE)

# Write copy of WFO cache alongside outputs
file.copy(wfo_path, file.path(outdir, "wfo_cache.rds"), overwrite = TRUE)
