find_raster_files <- function(path, recursive = TRUE) {
  stopifnot(dir.exists(path))
  requireNamespace("data.table")

  files <- list.files(
    path,
    recursive  = recursive,
    full.names = TRUE
  )

  ras <- files[grepl("\\.(tif|tiff|img|vrt)$", files, ignore.case = TRUE)]
  if (!length(ras)) {
    return(data.table::data.table())
  }

  data.table::data.table(
    dataset_id   = tools::file_path_sans_ext(basename(ras)),
    dataset_type = "raster_file",
    main_path    = ras,
    container    = dirname(ras),
    components   = as.list(ras),
    n_components = 1L
  )
}