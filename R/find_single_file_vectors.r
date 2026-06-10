find_single_file_vectors <- function(path, recursive = TRUE) {
  stopifnot(dir.exists(path))
  requireNamespace("data.table")

  exts <- c("gpkg", "kml", "kmz", "geojson", "gml")

  files <- list.files(
    path,
    recursive  = recursive,
    full.names = TRUE
  )

  vec <- files[
    grepl(
      paste0("\\.(", paste(exts, collapse = "|"), ")$"),
      files,
      ignore.case = TRUE
    )
  ]

  if (!length(vec)) {
    return(data.table::data.table())
  }

  data.table::data.table(
    dataset_id   = tools::file_path_sans_ext(basename(vec)),
    dataset_type = tolower(tools::file_ext(vec)),
    main_path    = vec,
    container    = dirname(vec),
    components   = as.list(vec),
    n_components = 1L
  )
}