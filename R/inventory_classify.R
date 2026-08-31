#' Classify inventory file format
#'
#' @param ext File extension.
#' @param path Optional path.
#'
#' @return Character scalar.
#' @export
inventory_classify_format <- function(ext, path = NULL) {
  ext <- tolower(ext)

  if (!is.null(path) && grepl("\\.gdb(/|$)", path, ignore.case = TRUE)) {
    return("esri_file_geodatabase")
  }

  if (ext %in% inventory_ext$virtual_raster) return("virtual_raster")
  if (ext %in% inventory_ext$raster_primary) return("raster")
  if (ext %in% inventory_ext$vector_primary) return("vector")
  if (ext %in% inventory_ext$style_primary) return("style")
  if (ext %in% inventory_ext$cad_primary) return("cad")
  if (ext %in% inventory_ext$tabular_primary) return("tabular")
  if (ext %in% inventory_ext$document_primary) return("document")
  if (ext %in% inventory_ext$image_primary) return("image")
  if (ext %in% inventory_ext$point_cloud_primary) return("point_cloud")
  if (ext %in% inventory_ext$multidim_raster) return("multidimensional_raster")
  if (ext %in% inventory_ext$database_primary) return("spatial_database_candidate")
  if (ext %in% inventory_ext$tile_package) return("tile_package")
  if (ext %in% inventory_ext$archive) return("archive")
  if (ext %in% inventory_ext$sidecar) return("sidecar")

  "other"
}


#' Classify inventory file role
#'
#' @param ext File extension.
#' @param path Optional file path.
#' @param is_container Logical.
#'
#' @return Character scalar.
#' @export
inventory_classify_role <- function(ext, path = NULL, is_container = FALSE) {
  ext <- tolower(ext)

  if (isTRUE(is_container)) {
    return("dataset_container")
  }

  if (!is.null(path) && grepl("\\.gdb(/|$)", path, ignore.case = TRUE)) {
    return("dataset_container")
  }

  primary_ext <- c(
    inventory_ext$raster_primary,
    inventory_ext$virtual_raster,
    inventory_ext$vector_primary,
    inventory_ext$style_primary,
    inventory_ext$cad_primary,
    inventory_ext$tabular_primary,
    inventory_ext$document_primary,
    inventory_ext$image_primary,
    inventory_ext$point_cloud_primary,
    inventory_ext$multidim_raster,
    inventory_ext$database_primary,
    inventory_ext$tile_package,
    inventory_ext$archive
  )

  if (ext %in% primary_ext) return("file")
  if (ext %in% inventory_ext$sidecar) return("sidecar")

  "other"
}

#' Apply format-specific classification overrides
#'
#' @param dt data.table.
#'
#' @return data.table.
#' @keywords internal
apply_format_overrides <- function(dt) {
  data.table::setDT(dt)

  shp_keys <- dt[data_file_extension == "shp", file_key]
  gpkg_keys <- dt[data_file_extension == "gpkg", file_key]
  sqlite_keys <- dt[data_file_extension %in% c("sqlite", "sqlite3", "db"), file_key]
  tif_keys <- dt[data_file_extension %in% c("tif", "tiff"), file_key]
  ecw_keys <- dt[data_file_extension == "ecw", file_key]
  img_keys <- dt[data_file_extension == "img", file_key]
  idrisi_raster_keys <- dt[data_file_extension == "rst", file_key]
  idrisi_vector_keys <- dt[data_file_extension == "vct", file_key]
  saga_sdat_keys <- dt[data_file_extension == "sdat", file_key]
  mapinfo_tab_keys <- dt[data_file_extension == "tab", file_key]
  hdr_keys <- dt[data_file_extension == "hdr", file_key]
  las_keys <- dt[data_file_extension %in% c("las", "laz"), file_key]

  dt[
    data_file_extension %in% c("gpkg-shm", "gpkg-wal", "gpkg-journal") &
      file_key %in% gpkg_keys,
    `:=`(
      data_file_format = "sidecar",
      file_role = "sidecar"
    )
  ]

  dt[
    grepl("^(sqlite|sqlite3|db)-(shm|wal|journal)$", data_file_extension) &
      file_key %in% sqlite_keys,
    `:=`(
      data_file_format = "sidecar",
      file_role = "sidecar"
    )
  ]

  dt[
    data_file_extension %in% c(
      "tif.aux.xml", "tiff.aux.xml", "tif.xml", "tiff.xml",
      "tif.vat.dbf", "tiff.vat.dbf",
      "tif.vat.cpg", "tiff.vat.cpg",
      "ovr", "tfw", "tifw", "wld", "prj", "rrd"
    ) &
      file_key %in% tif_keys,
    `:=`(
      data_file_format = "sidecar",
      file_role = "sidecar"
    )
  ]

  dt[
    data_file_extension %in% c("tif.vat.dbf", "tif.vat.cpg", "tiff.vat.dbf", "tiff.vat.cpg"),
    file_role := "sidecar"
  ]
  dt[
    data_file_extension == "ecw",
    `:=`(
      data_file_format = "raster",
      file_role = "file"
    )
  ]
  dt[
    data_file_extension %in% c("ecw.aux.xml", "ecw.xml", "ecw.ovr", "aux.xml", "ovr") &
      file_key %in% ecw_keys,
    `:=`(
      data_file_format = "sidecar",
      file_role = "sidecar"
    )
  ]
  dt[
    data_file_extension == "img",
    `:=`(
      data_file_format = "raster",
      file_role = "file"
    )
  ]

  dt[
    data_file_extension %in% c("ige", "rrd", "img.aux.xml", "ovr", "prj") &
      file_key %in% img_keys,
    `:=`(
      data_file_format = "sidecar",
      file_role = "sidecar"
    )
  ]

  dt[
    data_file_extension == "rst",
    `:=`(
      data_file_format = "raster",
      file_role = "file"
    )
  ]

  dt[
    data_file_extension == "vct",
    `:=`(
      data_file_format = "vector",
      file_role = "file"
    )
  ]

  dt[
    data_file_extension %in% c("rdc", "ref", "smp", "avl") &
      file_key %in% idrisi_raster_keys,
    `:=`(
      data_file_format = "sidecar",
      file_role = "sidecar"
    )
  ]

  dt[
    data_file_extension %in% c("vdc", "ref", "smp", "avl") &
      file_key %in% idrisi_vector_keys,
    `:=`(
      data_file_format = "sidecar",
      file_role = "sidecar"
    )
  ]

  dt[
    data_file_extension == "sdat",
    `:=`(
      data_file_format = "raster",
      file_role = "file"
    )
  ]

  dt[
    data_file_extension %in% c("sgrd", "mgrd", "prj", "sdat.aux.xml", "sdat.ovr") &
      file_key %in% saga_sdat_keys,
    `:=`(
      data_file_format = "sidecar",
      file_role = "sidecar"
    )
  ]

  dt[
    data_file_extension == "tab",
    `:=`(
      data_file_format = "vector",
      file_role = "file"
    )
  ]

  dt[
    data_file_extension %in% c("dat", "map", "id", "ind") &
      file_key %in% mapinfo_tab_keys,
    `:=`(
      data_file_format = "sidecar",
      file_role = "sidecar"
    )
  ]

  dt[
    data_file_extension %in% c("bil", "bip", "bsq"),
    `:=`(
      data_file_format = "raster",
      file_role = "file"
    )
  ]

  dt[
    data_file_extension == "dat" &
      file_key %in% hdr_keys &
      !file_key %in% mapinfo_tab_keys,
    `:=`(
      data_file_format = "raster",
      file_role = "file"
    )
  ]

  envi_primary_keys <- dt[
    data_file_extension %in% c("bil", "bip", "bsq") |
      (data_file_extension == "dat" & data_file_format == "raster"),
    file_key
  ]

  dt[
    data_file_extension %in% c("hdr", "stx", "clr", "prj") &
      file_key %in% envi_primary_keys,
    `:=`(
      data_file_format = "sidecar",
      file_role = "sidecar"
    )
  ]

  dt[
    data_file_extension == "dat" &
      !file_key %in% mapinfo_tab_keys &
      !file_key %in% hdr_keys &
      data_file_format == "other",
    `:=`(
      data_file_format = "binary_or_unknown_data",
      file_role = "file"
    )
  ]

  dt[
    data_file_extension %in% c("las", "laz"),
    `:=`(
      data_file_format = "point_cloud",
      file_role = "file"
    )
  ]

  dt[
    data_file_extension %in% c("lax", "lasx", "prj") &
      file_key %in% las_keys,
    `:=`(
      data_file_format = "sidecar",
      file_role = "sidecar"
    )
  ]

  dt[
    data_file_extension == "vrt",
    `:=`(
      data_file_format = "virtual_raster",
      file_role = "file"
    )
  ]

  dt[
    data_file_extension %in% c("nc", "nc4", "hdf", "h5", "he5"),
    `:=`(
      data_file_format = "multidimensional_raster",
      file_role = "file"
    )
  ]

  dt[
    data_file_extension %in% c("tpk", "tpkx", "vtpk", "mmpk", "mbtiles"),
    `:=`(
      data_file_format = "tile_package",
      file_role = "file"
    )
  ]

  dt[
    data_file_extension %in% c("zip", "7z", "tar", "gz", "tgz"),
    `:=`(
      data_file_format = "archive",
      file_role = "file"
    )
  ]

  dt[
    data_file_extension == "sgrd" &
      !file_key %in% saga_sdat_keys,
    `:=`(
      data_file_format = "raster",
      file_role = "file"
    )
  ]

  dt[
    data_file_extension %in% c("dbf", "shx", "cpg", "sbn", "sbx", "qpj", "shp.xml") &
      file_key %in% shp_keys,
    `:=`(
      data_file_format = "sidecar",
      file_role = "sidecar"
    )
  ]

  dt[
    data_file_extension == "prj" &
      file_key %in% shp_keys,
    `:=`(
      data_file_format = "sidecar",
      file_role = "sidecar"
    )
  ]

  dt[]
}

#' Classify files in an inventory table
#'
#' Adds dataset classification fields to an existing inventory table.
#' The input may come from a local filesystem scan, `rclone lsjson`, a NAS scan,
#' or any other inventory process that produces file-level records.
#'
#' This function does not perform the filesystem scan itself. Instead, it
#' classifies already-scanned records into logical file roles and formats,
#' including primary datasets, sidecar files, and components of compound GIS
#' datasets such as Esri File Geodatabases and ArcInfo Binary Grids.
#'
#' If a mount/source column is present, the function constructs a mount-aware
#' `location_path` using `file.path(mount, path)`. This prevents files from
#' different mounts or storage sources from being incorrectly grouped together
#' when they share the same relative path, file name, or file stem. The original
#' `path` column is preserved as the path relative to the source.
#'
#' @param dt A `data.frame` or `data.table` containing file inventory records.
#'   The table must contain a path column. File name, size, directory flag, and
#'   mount/source columns are optional but recommended.
#'
#' @param path_col Character scalar. Name of the column containing the file path
#'   within the scanned source. Defaults to `"path"`.
#'
#' @param name_col Character scalar. Name of the column containing the file or
#'   directory name. Defaults to `"name"`. If missing from `dt`, it is derived
#'   from `basename(path)`.
#'
#' @param size_col Character scalar. Name of the column containing file size in
#'   bytes. Defaults to `"size"`. If missing from `dt`, `size` is set to
#'   `NA_real_`.
#'
#' @param isdir_col Character scalar. Name of the logical column indicating
#'   whether a record is a directory. Defaults to `"isdir"`. If missing from
#'   `dt`, `isdir` is set to `FALSE`.
#'
#' @param mount_col Character scalar. Name of the column identifying the source,
#'   mount, drive, remote, or storage location. Defaults to `"mount"`. If present,
#'   it is used to build `location_path`; if absent, `location_path` is set equal
#'   to `path`.
#'
#' @return A `data.table` containing the original inventory records plus GIS
#'   classification columns, including:
#'
#'   \describe{
#'     \item{`location_path`}{Mount-aware path used internally for grouping and
#'       classification.}
#'     \item{`file_name`}{Normalised file name derived from the input name
#'       column.}
#'     \item{`data_file_rel_path_full`}{Original path within the scanned source.}
#'     \item{`file_path`}{Path used for classification, usually equal to
#'       `location_path`.}
#'     \item{`data_file_extension`}{Normalised file extension, including compound
#'       GIS sidecar extensions such as `tif.aux.xml` and `shp.xml`.}
#'     \item{`file_stem`}{Cleaned logical file stem used for sidecar grouping.}
#'     \item{`file_dir`}{Directory component of `location_path`.}
#'     \item{`file_key`}{Grouping key based on directory and cleaned file stem.}
#'     \item{`dataset_root`}{Logical dataset root for recognised compound
#'       datasets, such as `.gdb` folders or ArcInfo Grid directories.}
#'     \item{`container_type`}{Detected container type, where applicable.}
#'     \item{`is_container`}{Logical flag indicating whether the row belongs to a
#'       recognised compound GIS dataset container.}
#'     \item{`data_file_format`}{Classified file or dataset format, for example
#'       `raster`, `vector`, `esri_file_geodatabase`, `esri_arcinfo_grid`,
#'       `gis_sidecar`, or `other`.}
#'     \item{`file_role`}{Role of the record, such as `file`, `sidecar`,
#'       `dataset_container_component`, or `other`.}
#'     \item{`primary_dataset`}{Logical flag indicating whether the row represents
#'       a primary dataset candidate.}
#'     \item{`group_family`, `group_key`, `sidecar_count`, `sidecar_exts`,
#'       `sidecar_summary`}{Sidecar grouping and summary fields.}
#'   }
#'
#' @details
#' The function is designed for cached or precomputed inventories. It is useful
#' when the scan step is handled elsewhere, for example by `rclone lsjson`, GNU
#' `find`, or another storage inventory process.
#'
#' For single-folder scans, `path` alone is usually sufficient for grouping.
#' For multi-source scans, such as inventories combining Teams drives, OneDrive
#' remotes, NAS paths, and local SMB mounts, a mount/source column should be
#' supplied. In that case, sidecar grouping and compound dataset detection are
#' based on `location_path` rather than raw `path`.
#'
#' @examples
#' inv <- data.table::data.table(
#'   mount = c("gis_smb", "gis_smb", "teams_xdrive"),
#'   path = c(
#'     "landcover/salc1314wdd/hdr.adf",
#'     "landcover/salc1314wdd/w001001.adf",
#'     "roads/roads.shp"
#'   ),
#'   name = c("hdr.adf", "w001001.adf", "roads.shp"),
#'   size = c(1000, 2000, 5000),
#'   isdir = FALSE
#' )
#'
#' classified <- inventory_classify(inv)
#'
#' classified[, .(
#'   mount,
#'   path,
#'   location_path,
#'   dataset_root,
#'   data_file_format,
#'   file_role
#' )]
#'
#' @export
inventory_classify <- function(
  dt,
  path_col = "path",
  name_col = "name",
  size_col = "size",
  isdir_col = "isdir",
  mount_col = "mount"
) {
  dt <- data.table::as.data.table(data.table::copy(dt))

  if (!path_col %in% names(dt)) {
    stop("Input must contain path column: ", path_col)
  }

  if (path_col != "path") data.table::setnames(dt, path_col, "path")

  if (name_col %in% names(dt) && name_col != "name") {
    data.table::setnames(dt, name_col, "name")
  }

  if (size_col %in% names(dt) && size_col != "size") {
    data.table::setnames(dt, size_col, "size")
  }

  if (isdir_col %in% names(dt) && isdir_col != "isdir") {
    data.table::setnames(dt, isdir_col, "isdir")
  }

  if (!"name" %in% names(dt)) dt[, name := basename(path)]
  if (!"size" %in% names(dt)) dt[, size := NA_real_]
  if (!"isdir" %in% names(dt)) dt[, isdir := FALSE]

  # ---- mount-aware global path ----
  if (mount_col %in% names(dt)) {
    if (mount_col != "mount") {
      data.table::setnames(dt, mount_col, "mount")
    }
    dt[, location_path := file.path(mount, path)]
  } else {
    dt[, mount := NA_character_]
    dt[, location_path := path]
  }

  dt[, file_name := name]
  dt[, data_file_rel_path_full := path]
  dt[, file_path := location_path]

  dt[, data_file_extension := inventory_file_extension(file_name)]
  dt[, file_stem := inventory_clean_file_stem(file_name)]

  # Important: use location_path here, not path
  dt[, file_dir := dirname(location_path)]
  dt[is.na(file_dir) | file_dir == ".", file_dir := ""]

  dt[, file_key := paste(file_dir, file_stem, sep = "::")]

  # Container root detection must also be mount-aware
  dt <- inventory_detect_containers(dt, path_col = "location_path")
  if (!"is_container_support" %in% names(dt)) {
    dt[, is_container_support := FALSE]
  }
  if (!"support_type" %in% names(dt)) {
    dt[, support_type := NA_character_]
  }
  dt[, is_container := !is.na(dataset_root) & !is_container_support]

  dt[
    is.na(container_type) & grepl("\\.gdb$", location_path, ignore.case = TRUE),
    container_type := "esri_file_geodatabase"
  ]

  dt[, data_file_format := mapply(
    inventory_classify_format,
    data_file_extension,
    location_path,
    USE.NAMES = FALSE
  )]

  dt[
    container_type == "esri_file_geodatabase",
    data_file_format := "esri_file_geodatabase"
  ]

  dt[
    container_type == "esri_arcinfo_grid",
    data_file_format := "esri_arcinfo_grid"
  ]

  dt[
    container_type == "esri_coverage",
    data_file_format := "esri_coverage"
  ]
  dt[
    is_container_support == TRUE,
    data_file_format := "container_support"
  ]

  dt[, file_role := data.table::fifelse(
    is_container_support,
    "container_support",
    data.table::fifelse(
      is_container,
      "dataset_container_component",
      data.table::fifelse(
        inventory_detect_sidecar(file_path),
        "sidecar",
        data.table::fifelse(data_file_format == "other", "other", "file")
      )
    )
  )]
  # Apply domain-specific rules/overrides after initial format and role assignment.
  dt <- inventory_apply_rules(dt)

  # Assign group family, group_key, and sidecar summary after roles are stable.
  dt <- inventory_assign_groups(dt)

  # Calculate initial primary flag; final representative primary is refined
  # by inventory_enrich_groups().
  dt[, primary_dataset := file_role %in% c(
    "file",
    "dataset_container",
    "dataset_container_component"
  ) &
    !data_file_format %in% c("sidecar")]
  dt[
    data_file_format %in% c("archive"),
    primary_dataset := FALSE
  ]

  dt[]
}
