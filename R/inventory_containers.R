#' Detect Esri File Geodatabase path
#'
#' @param path Character vector.
#'
#' @return Logical vector.
is_gdb_path <- function(path) {
  grepl("\\.gdb(/|$)", path, ignore.case = TRUE)
}


#' Detect ArcInfo Grid component
#'
#' @param name File name.
#' @param path File path.
#'
#' @return Logical vector.
is_arcinfo_grid_component <- function(name, path = name) {
  grepl(
    "^(hdr|dblbnd|sta|vat|prj|w[0-9]{6}x?)\\.adf$",
    name,
    ignore.case = TRUE
  )
}


#' Detect Esri INFO component
#'
#' @param name File name.
#' @param path File path.
#'
#' @return Logical vector.
is_esri_info_component <- function(name, path) {
  grepl("/info/", path, ignore.case = TRUE) &
    grepl("arc.*\\.dir$|arc.*\\.nat$|arc.*\\.nit$|arc.*\\.dat", name,
          ignore.case = TRUE)
}


#' Add logical dataset container roots to an inventory table
#'
#' Detects rows that are components of compound GIS datasets and assigns a
#' logical dataset root. This is used for structures such as Esri File
#' Geodatabases, ArcInfo Binary Grids, raster overview folders, and Esri
#' Coverage INFO workspaces.
#'
#' @param dt A `data.table` or `data.frame` containing inventory records.
#' @param path_col Character scalar. Name of the path column to use for
#'   container detection. Defaults to `"path"`. For multi-mount inventories,
#'   this should usually be `"location_path"`.
#'
#' @return The input as a `data.table`, with `dataset_root`, `container_type`,
#'   and, where relevant, `info_root` columns added.
#'
#' @keywords internal
inventory_detect_containers <- function(dt, path_col = "path") {
  data.table::setDT(dt)
  if (!path_col %in% names(dt)) {
    stop("dt must contain path column: ", path_col)
  }

  if (!"name" %in% names(dt)) {
    dt[, name := basename(get(path_col))]
  }

  p <- dt[[path_col]]

  dt[, dataset_root := NA_character_]
  dt[, container_type := NA_character_]

  # 1. Esri File Geodatabase

  # Anything inside a .gdb folder collapses to the .gdb root.
  idx_gdb <- grepl("\\.gdb(/|$)", p, ignore.case = TRUE)
  if (any(idx_gdb, na.rm = TRUE)) {
    dt[idx_gdb, `:=`(
      dataset_root = sub(
        "^(.*?\\.gdb)(/.*)?$",
        "\\1",
        get(path_col),
        ignore.case = TRUE
      ),
      container_type = "esri_file_geodatabase"
    )]
  }

  # 2. ArcInfo Binary Grid

  # A Grid is a directory containing both its header and a wNNNNNN.adf
  # raster-data file. Once identified, every physical component inside the
  # directory belongs to the Grid, including files such as metadata.xml.
  component_dirs <- unique(dirname(
    p[!is.na(p) & is_arcinfo_grid_component(dt$name, p) %in% TRUE]
  ))
  component_dirs <- component_dirs[
    !is.na(component_dirs) & nzchar(component_dirs)
  ]

  grid_roots <- component_dirs[vapply(
    component_dirs,
    function(root) {
      component_names <- tolower(dt$name[dirname(p) == root])

      "hdr.adf" %in% component_names &&
        any(grepl("^w[0-9]{6}\\.adf$", component_names))
    },
    logical(1)
  )]

  if (length(grid_roots) > 0L) {
    for (root in grid_roots) {
      idx_grid <- (p == root | startsWith(p, paste0(root, "/"))) &
        is.na(dt$dataset_root)

      dt[idx_grid, `:=`(
        dataset_root = root,
        container_type = "esri_arcinfo_grid"
      )]
    }
  }

  # 3. Raster overview files
  #
  # Do not assign .ovr files as container roots here.
  # They are sidecar files and should be grouped later by inventory_assign_groups().
  # This avoids promoting standalone overview files to container components.

  # 4. Esri INFO workspace support
  #
  # An INFO directory is workspace support when it is a sibling of a detected
  # Grid. It can support several Grids, so its rows deliberately have no
  # dataset_root and are not attached to any one Grid.
  info_roots <- unique(file.path(dirname(grid_roots), "info"))
  idx_info <- rep(FALSE, length(p))

  if (length(info_roots) > 0L) {
    p_lower <- tolower(p)

    for (root in tolower(info_roots)) {
      idx_info <- idx_info |
        p_lower == root |
        startsWith(p_lower, paste0(root, "/"))
    }

    idx_info <- idx_info & is.na(dt$dataset_root)
  }

  if (any(idx_info, na.rm = TRUE)) {
    if (!"is_container_support" %in% names(dt)) {
      dt[, is_container_support := FALSE]
    }

    if (!"support_type" %in% names(dt)) {
      dt[, support_type := NA_character_]
    }

    dt[idx_info, `:=`(
      is_container_support = TRUE,
      support_type = "esri_info_workspace",
      container_type = "esri_info_workspace",
      dataset_root = NA_character_
    )]
  }

  dt[]
}
