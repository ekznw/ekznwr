#' Construct an empty inventory table
#'
#' @param enriched Include logical dataset enrichment columns.
#'
#' @return Zero-row data.table with a stable inventory schema.
#' @keywords internal
inventory_empty <- function(enriched = TRUE) {
  dt <- data.table::data.table(
    mount = character(),
    path = character(),
    name = character(),
    size = numeric(),
    modtime = as.POSIXct(character()),
    isdir = logical(),
    location_path = character(),
    file_name = character(),
    data_file_rel_path_full = character(),
    file_path = character(),
    data_file_extension = character(),
    file_stem = character(),
    file_dir = character(),
    file_key = character(),
    dataset_root = character(),
    container_type = character(),
    is_container_support = logical(),
    support_type = character(),
    is_container = logical(),
    data_file_format = character(),
    file_role = character(),
    group_family = character(),
    group_key = character(),
    sidecar_count = integer(),
    sidecar_exts = character(),
    sidecar_summary = character(),
    primary_dataset = logical()
  )

  if (isTRUE(enriched)) {
    dt[, `:=`(
      logical_file_name = character(),
      logical_file_path = character(),
      logical_rel_path = character(),
      logical_dir = character(),
      logical_stem = character(),
      logical_extension = character(),
      logical_format = character(),
      logical_file_role = character(),
      logical_container_type = character(),
      logical_is_container = logical(),
      logical_size_bytes = numeric(),
      logical_size_kb = numeric(),
      logical_size_mb = numeric(),
      n_components = integer(),
      n_sidecars = integer(),
      n_primary_files = integer(),
      n_dirs = integer(),
      n_files = integer(),
      component_exts = character(),
      component_roles = character(),
      component_modtime_latest = as.POSIXct(character()),
      source_paths = character(),
      primary_row_id = integer(),
      logical_primary_reason = character()
    )]
  }

  dt[]
}


#' Add scan metadata to an inventory result
#'
#' @param dt Inventory data.table.
#' @param root Normalised scan root.
#' @param status Scan status.
#' @param include_hidden Whether hidden files were included.
#'
#' @return `dt` with inventory class and scan attributes.
#' @keywords internal
new_inventory_result <- function(
  dt,
  root,
  status = c("ok", "empty"),
  include_hidden = FALSE
) {
  status <- match.arg(status)

  data.table::setattr(
    dt,
    "scan_root",
    root
  )

  data.table::setattr(
    dt,
    "scan_status",
    status
  )

  data.table::setattr(
    dt,
    "scan_time",
    Sys.time()
  )

  data.table::setattr(
    dt,
    "scan_include_hidden",
    include_hidden
  )

  data.table::setattr(
    dt,
    "class",
    unique(c(
      "ekznwr_inventory",
      class(dt)
    ))
  )

  dt
}

#' Scan a local directory into an inventory table
#'
#' Recursively scans a local directory, classifies files, and optionally
#' enriches rows with logical dataset and group metadata.
#'
#' Invalid paths raise structured errors. A valid empty directory returns a
#' zero-row inventory with the normal inventory schema.
#'
#' @param path Character scalar containing the directory to scan.
#' @param enrich Logical. If `TRUE`, add logical dataset/group enrichment.
#' @param include_hidden Logical. If `TRUE`, include hidden files and
#'   directories.
#'
#' @return An object of class `ekznwr_inventory` and `data.table`.
#' @export
inventory_scan_local <- function(
  path,
  enrich = TRUE,
  include_hidden = FALSE
) {
  if (!is.character(path) ||
      length(path) != 1L ||
      is.na(path) ||
      !nzchar(path)) {
    cli::cli_abort(
      "{.arg path} must be one non-empty directory path."
    )
  }

  if (!is.logical(enrich) ||
      length(enrich) != 1L ||
      is.na(enrich)) {
    cli::cli_abort(
      "{.arg enrich} must be one non-missing logical value."
    )
  }

  if (!is.logical(include_hidden) ||
      length(include_hidden) != 1L ||
      is.na(include_hidden)) {
    cli::cli_abort(
      "{.arg include_hidden} must be one non-missing logical value."
    )
  }

  path <- path.expand(path)

  if (!dir.exists(path)) {
    cli::cli_abort(
      "Directory does not exist: {.path {path}}."
    )
  }

  root <- inventory_normalise_path(path)

  if (file.access(root, mode = 4L) != 0L) {
    cli::cli_abort(
      "Directory is not readable: {.path {root}}."
    )
  }

  files <- tryCatch(
    list.files(
      path = root,
      recursive = TRUE,
      full.names = TRUE,
      all.files = include_hidden,
      no.. = TRUE
    ),
    warning = function(w) {
      cli::cli_warn(
        "Directory scan warning: {conditionMessage(w)}"
      )

      list.files(
        path = root,
        recursive = TRUE,
        full.names = TRUE,
        all.files = include_hidden,
        no.. = TRUE
      )
    },
    error = function(e) {
      cli::cli_abort(c(
        "Could not scan directory: {.path {root}}.",
        "x" = conditionMessage(e)
      ))
    }
  )

  if (length(files) == 0L) {
    return(new_inventory_result(
      inventory_empty(enriched = enrich),
      root = root,
      status = "empty",
      include_hidden = include_hidden
    ))
  }

  file_paths <- inventory_normalise_path(files)
  info <- file.info(file_paths)

  dt <- data.table::data.table(
    mount = root,
    path = inventory_rel_path(
      file_paths,
      root
    ),
    name = basename(file_paths),
    size = as.numeric(info$size),
    modtime = info$mtime,
    isdir = info$isdir
  )

  dt <- inventory_classify(dt)

  if (isTRUE(enrich)) {
    dt <- inventory_enrich_groups(dt)
  }

  new_inventory_result(
    dt,
    root = root,
    status = "ok",
    include_hidden = include_hidden
  )
}