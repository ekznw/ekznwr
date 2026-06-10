find_esri_raster_dirs <- function(path, recursive = TRUE) {
  stopifnot(dir.exists(path))

  all_dirs <- list.files(
    path,
    recursive = recursive,
    full.names = TRUE,
    include.dirs = TRUE
  )

  is_dir <- file.info(all_dirs)$isdir
  dirs <- all_dirs[is_dir]

  esri_grid <- dirs[file.exists(file.path(dirs, "hdr.adf"))]
  crf <- dirs[grepl("\\.crf$", dirs, ignore.case = TRUE)]

  mk_dt <- function(dirs, type) {
    if (!length(dirs)) return(NULL)

    data.table::data.table(
      dataset_id   = basename(dirs),
      dataset_type = type,
      main_path    = dirs,
      container    = dirname(dirs),
      components   = lapply(dirs, list.files, full.names = TRUE),
      n_components = lengths(lapply(dirs, list.files))
    )
  }

  data.table::rbindlist(list(
    mk_dt(esri_grid, "esri_grid"),
    mk_dt(crf, "crf")
  ), use.names = TRUE)
}