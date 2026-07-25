#' Detect inventory sidecar files
#'
#' @param path Character vector.
#'
#' @return Logical vector.
#' @export
inventory_detect_sidecar <- function(path) {
  vapply(path, inventory_detect_sidecar_one, logical(1))
}

inventory_detect_sidecar_one <- function(path) {
  ext <- tolower(tools::file_ext(path))

  grepl("\\.shp\\.xml$", path, ignore.case = TRUE) ||
    grepl("\\.aux\\.xml$", path, ignore.case = TRUE) ||
    grepl("\\.tif\\.xml$", path, ignore.case = TRUE) ||
    grepl("\\.tif\\.vat\\.dbf$", path, ignore.case = TRUE) ||
    grepl("\\.ecw\\.(aux\\.xml|xml|ovr)$", path, ignore.case = TRUE) ||
    grepl("\\.gpkg-(shm|wal|journal)$", path, ignore.case = TRUE) ||
    grepl("\\.(sqlite|sqlite3|db)-(shm|wal|journal)$", path, ignore.case = TRUE) ||
    ext %in% c(
      "aux", "ovr", "tfw", "tifw", "jgw", "wld", "qpj",
      "shx", "sbn", "sbx", "cpg",
      "rdc", "vdc", "ref", "smp", "avl",
      "sgrd", "mgrd",
      "ige", "rrd",
      "hdr", "stx", "clr",
      "map", "id", "ind",
      "lax", "lasx"
    )
}


#' Add inventory sidecar grouping
#'
#' @param dt data.table produced by classify_gis_inventory().
#'
#' @return data.table.
inventory_assign_groups <- function(dt) {
  data.table::setDT(dt)

  if ("message" %in% names(dt)) {
    return(dt)
  }

  required <- c("file_name", "data_file_extension", "file_role")
  missing <- setdiff(required, names(dt))
  if (length(missing) > 0) {
    stop("Missing required columns: ", paste(missing, collapse = ", "))
  }

  if (!"file_stem" %in% names(dt)) {
    dt[, file_stem := inventory_clean_file_stem(file_name)]
  }

  if (!"file_dir" %in% names(dt)) {
    dt[, file_dir := dirname(data_file_rel_path_full %||% path)]
    dt[is.na(file_dir) | file_dir == ".", file_dir := ""]
  }

  dt[, group_family := NA_character_]

  dt[file_role %in% c("file", "dataset_container"),
    group_family := data_file_extension]

  dt[
    is_container == TRUE & container_type == "esri_file_geodatabase",
    group_family := "gdb"
  ]

  dt[
    is_container == TRUE & container_type == "esri_arcinfo_grid",
    group_family := "adf"
  ]

  dt[file_role != "sidecar" & data_file_extension %in% c("tif", "tiff"),
    group_family := data_file_extension]
  dt[file_role != "sidecar" & data_file_extension == "ecw",
    group_family := "ecw"]

  dt[file_role != "sidecar" & data_file_extension == "sdat", group_family := "sdat"]
  dt[file_role != "sidecar" & data_file_extension == "img", group_family := "img"]
  dt[file_role != "sidecar" & data_file_extension == "rst", group_family := "rst"]
  dt[file_role != "sidecar" & data_file_extension == "vct", group_family := "vct"]
  dt[file_role != "sidecar" & data_file_extension == "shp", group_family := "shp"]
  dt[file_role != "sidecar" & data_file_extension == "gpkg", group_family := "gpkg"]
  dt[file_role != "sidecar" & data_file_extension == "tab", group_family := "tab"]

  dt[file_role != "sidecar" & data_file_extension %in% c("sqlite", "sqlite3", "db"),
    group_family := data_file_extension]

  dt[file_role != "sidecar" & data_file_extension %in% c("las", "laz"),
    group_family := data_file_extension]

  primary_lookup <- unique(
    dt[
      file_role %in% c("file", "dataset_container"),
      .(file_dir, file_stem, group_family)
    ]
  )

  choose_existing_family <- function(this_dir, this_stem, candidates) {
    hits <- primary_lookup[
      file_dir == this_dir &
        file_stem == this_stem &
        group_family %in% candidates,
      group_family
    ]
    if (length(hits) > 0) hits[1] else candidates[1]
  }

  # TIFF sidecars
  idx <- which(
    dt$file_role == "sidecar" &
      grepl(
        "\\.(tif|tiff)(\\.aux\\.xml|\\.xml|\\.vat\\.dbf|\\.vat\\.cpg)$",
        dt$file_name,
        ignore.case = TRUE
      )
  )
  if (length(idx) > 0) dt[idx, group_family := "tif"]

  idx <- which(
    dt$file_role == "sidecar" &
      grepl("\\.ecw\\.(aux\\.xml|xml|ovr)$",
            dt$file_name,
            ignore.case = TRUE)
  )

  if (length(idx) > 0) {
    dt[idx, group_family := "ecw"]
  }

  # Shapefile sidecars
  idx <- which(
    dt$file_role == "sidecar" &
      grepl("\\.shp\\.xml$|\\.(shx|sbn|sbx|qpj)$",
            dt$file_name,
            ignore.case = TRUE)
  )
  if (length(idx) > 0) dt[idx, group_family := "shp"]

  idx <- which(
    dt$file_role == "sidecar" &
      dt$data_file_extension %in% c("dbf", "cpg") &
      mapply(function(d, s) {
        any(dt$file_dir == d &
              dt$file_stem == s &
              dt$data_file_extension == "shp")
      }, dt$file_dir, dt$file_stem)
  )
  if (length(idx) > 0) dt[idx, group_family := "shp"]

  # GeoPackage sidecars
  idx <- which(
    dt$file_role == "sidecar" &
      grepl("\\.gpkg-(shm|wal|journal)$", dt$file_name, ignore.case = TRUE)
  )
  if (length(idx) > 0) dt[idx, group_family := "gpkg"]

  # SQLite sidecars
  idx <- which(
    dt$file_role == "sidecar" &
      grepl("\\.(sqlite|sqlite3|db)-(shm|wal|journal)$",
            dt$file_name,
            ignore.case = TRUE)
  )
  if (length(idx) > 0) {
    dt[idx, group_family := sub(
      ".*\\.(sqlite|sqlite3|db)-(shm|wal|journal)$",
      "\\1",
      file_name,
      ignore.case = TRUE
    )]
  }

  # SAGA
  idx <- which(
    dt$file_role == "sidecar" &
      grepl("\\.sdat\\.aux\\.xml$|\\.sdat\\.ovr$|\\.(sgrd|mgrd)$",
            dt$file_name,
            ignore.case = TRUE)
  )
  if (length(idx) > 0) dt[idx, group_family := "sdat"]

  # ERDAS
  idx <- which(
    dt$file_role == "sidecar" &
      grepl("\\.img\\.aux\\.xml$|\\.(ige|rrd)$",
            dt$file_name,
            ignore.case = TRUE)
  )
  if (length(idx) > 0) dt[idx, group_family := "img"]

  # IDRISI
  idx <- which(
    dt$file_role == "sidecar" &
      grepl("\\.(rdc|ref)$", dt$file_name, ignore.case = TRUE)
  )
  if (length(idx) > 0) dt[idx, group_family := "rst"]

  idx <- which(
    dt$file_role == "sidecar" &
      grepl("\\.vdc$", dt$file_name, ignore.case = TRUE)
  )
  if (length(idx) > 0) dt[idx, group_family := "vct"]

  # MapInfo
  idx <- which(
    dt$file_role == "sidecar" &
      grepl("\\.(dat|map|id|ind)$", dt$file_name, ignore.case = TRUE)
  )
  if (length(idx) > 0) dt[idx, group_family := "tab"]

  # Point cloud
  idx <- which(
    dt$file_role == "sidecar" &
      grepl("\\.(lax|lasx)$", dt$file_name, ignore.case = TRUE)
  )
  if (length(idx) > 0) {
    dt[idx, group_family := mapply(
      choose_existing_family,
      file_dir,
      file_stem,
      MoreArgs = list(candidates = c("las", "laz"))
    )]
  }

  # Ambiguous .prj
  idx <- which(
    dt$file_role == "sidecar" &
      is.na(dt$group_family) &
      dt$data_file_extension == "prj"
  )
  if (length(idx) > 0) {
    dt[idx, group_family := mapply(
      choose_existing_family,
      file_dir,
      file_stem,
      MoreArgs = list(
        candidates = c(
          "shp", "sdat", "img", "adf", "tif", "tiff",
          "rst", "vct", "las", "laz"
        )
      )
    )]
  }

  # Generic .aux.xml
  idx <- which(
    dt$file_role == "sidecar" &
      is.na(dt$group_family) &
      grepl("\\.aux\\.xml$", dt$file_name, ignore.case = TRUE)
  )
  if (length(idx) > 0) {
    dt[idx, group_family := mapply(
      choose_existing_family,
      file_dir,
      file_stem,
      MoreArgs = list(
        candidates = c("sdat", "img", "adf", "tif", "tiff", "ecw", "rst")
      )
    )]
  }

  # Generic .ovr
  idx <- which(
    dt$file_role == "sidecar" &
      is.na(dt$group_family) &
      dt$data_file_extension == "ovr"
  )
  if (length(idx) > 0) {
    dt[idx, group_family := mapply(
      choose_existing_family,
      file_dir,
      file_stem,
      MoreArgs = list(
        candidates = c("sdat", "img", "adf", "tif", "tiff", "ecw", "rst")
      )
    )]
  }

  idx <- which(dt$file_role == "sidecar" & is.na(dt$group_family))
  if (length(idx) > 0) {
    dt[idx, group_family := mapply(
      choose_existing_family,
      file_dir,
      file_stem,
      MoreArgs = list(
        candidates = c(
          "shp", "gpkg", "sdat", "img", "adf",
          "tif", "tiff", "ecw", "rst", "vct", "tab",
          "sqlite", "sqlite3", "db", "las", "laz"
        )
      )
    )]
  }

  dt[is.na(group_family) | group_family == "", group_family := data_file_extension]

  dt[, group_key := paste(file_dir, file_stem, group_family, sep = "::")]

  # Container datasets must group by dataset_root, not by internal component stem.
  dt[
    !is.na(dataset_root) & dataset_root != "",
    group_key := paste(dataset_root, container_type, sep = "::")
  ]
  # Attach standalone sidecars to detected container roots where possible.
  #
  # Example:
  #   root/dem_30_aea        = ArcInfo Grid folder detected as dataset_root
  #   root/dem_30_aea.ovr    = sidecar file beside the grid folder
  #
  # The .ovr should share the ArcInfo Grid group_key, but should remain a sidecar.
  container_lookup <- unique(
    dt[
      !is.na(dataset_root) &
        dataset_root != "" &
        !is.na(container_type) &
        container_type != "",
      .(dataset_root, container_type)
    ]
  )
  if (nrow(container_lookup) > 0) {
    side_idx <- which(
      dt$file_role == "sidecar" &
        (is.na(dt$dataset_root) | dt$dataset_root == "")
    )
    if (length(side_idx) > 0) {
      side_candidate_root <- file.path(
        dt$file_dir[side_idx],
        dt$file_stem[side_idx]
      )
      hit_idx <- match(side_candidate_root, container_lookup$dataset_root)
      has_hit <- !is.na(hit_idx)
      if (any(has_hit)) {
        rows <- side_idx[has_hit]
        matched_roots <- side_candidate_root[has_hit]
        matched_types <- container_lookup$container_type[hit_idx[has_hit]]
        dt[rows, `:=`(
          dataset_root = matched_roots,
          container_type = matched_types,
          group_key = paste(matched_roots, matched_types, sep = "::"),
          group_family = data.table::fifelse(
            matched_types == "esri_file_geodatabase",
            "gdb",
            data.table::fifelse(
              matched_types == "esri_arcinfo_grid",
              "adf",
              data.table::fifelse(
                matched_types == "esri_coverage",
                "coverage",
                group_family
              )
            )
          )
        )]
      }
    }
  }
  dt[
    !is.na(dataset_root) & dataset_root != "" &
      container_type == "esri_file_geodatabase",
    group_family := "gdb"
  ]
  dt[
    !is.na(dataset_root) & dataset_root != "" &
      container_type == "esri_arcinfo_grid",
    group_family := "adf"
  ]
  dt[
    !is.na(dataset_root) & dataset_root != "" &
      container_type == "esri_coverage",
    group_family := "coverage"
  ]

  sidecar_summary <- dt[
    file_role == "sidecar",
    .(
      sidecar_count = .N,
      sidecar_exts = paste(sort(unique(data_file_extension)), collapse = ", ")
    ),
    by = group_key
  ]

  dt <- merge(
    dt,
    sidecar_summary,
    by = "group_key",
    all.x = TRUE,
    sort = FALSE
  )

  dt[is.na(sidecar_count), sidecar_count := 0L]
  dt[is.na(sidecar_exts), sidecar_exts := ""]
  dt[, sidecar_summary := data.table::fifelse(
    sidecar_count > 0,
    paste0(sidecar_count, " file(s): ", sidecar_exts),
    "None"
  )]

  dt[]
}