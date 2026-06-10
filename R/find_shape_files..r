find_shapefiles <- function(path, recursive = TRUE) {
  stopifnot(dir.exists(path))
  requireNamespace("data.table")

  files <- list.files(
    path,
    recursive   = recursive,
    full.names  = TRUE
  )

  shp <- files[grepl("\\.shp$", files, ignore.case = TRUE)]
  if (!length(shp)) {
    return(data.table::data.table())
  }

  dt <- data.table::data.table(
    main_path  = shp,
    container  = dirname(shp),
    dataset_id = tools::file_path_sans_ext(basename(shp))
  )

  # collect associated components
  dt[, components := lapply(seq_len(.N), function(i) {
    base <- dataset_id[i]
    folder <- container[i]
    list.files(
      folder,
      pattern = paste0("^", base, "\\."),
      full.names = TRUE,
      ignore.case = TRUE
    )
  })]

  dt[, `:=`(
    dataset_type = "shapefile",
    n_components = lengths(components)
  )]

  return(dt[])
}