#' Enrich a classified inventory with logical dataset columns
#'
#' Keeps one row per scanned file or component while adding logical
#' dataset-level names, paths, sizes, component counts, and primary-row flags.
#'
#' @param dt A data frame or data.table returned by [inventory_classify()].
#'
#' @return A data.table containing the original rows and logical dataset fields.
#' @export
inventory_enrich_groups <- function(dt) {
  dt <- data.table::as.data.table(
    data.table::copy(dt)
  )

  inventory_require_columns(
    dt,
    required = c(
      "group_key",
      "file_name",
      "file_dir",
      "file_stem",
      "data_file_extension",
      "data_file_rel_path_full"
    ),
    caller = "inventory_enrich_groups"
  )

  owned_cols <- c(
    "logical_file_name",
    "logical_file_path",
    "logical_rel_path",
    "logical_dir",
    "logical_stem",
    "logical_extension",
    "logical_format",
    "logical_file_role",
    "logical_container_type",
    "logical_is_container",
    "logical_size_bytes",
    "logical_size_kb",
    "logical_size_mb",
    "n_components",
    "n_sidecars",
    "n_primary_files",
    "n_dirs",
    "n_files",
    "component_exts",
    "component_roles",
    "component_modtime_latest",
    "source_paths",
    "sidecar_count",
    "sidecar_exts",
    "sidecar_summary",
    "primary_dataset",
    "primary_row_id",
    "logical_primary_reason"
  )

  owned_cols <- intersect(
    owned_cols,
    names(dt)
  )

  if (length(owned_cols) > 0L) {
    dt[, (owned_cols) := NULL]
  }

  dt[, .row_id_inventory := .I]

  # Required safety defaults.
  if (!"size" %in% names(dt)) {
    dt[, size := 0]
  }

  if (!"modtime" %in% names(dt)) {
    dt[, modtime := as.POSIXct(
      NA_real_,
      origin = "1970-01-01",
      tz = "UTC"
    )]
  }

  if (!"isdir" %in% names(dt)) {
    dt[, isdir := FALSE]
  }

  if (!"file_role" %in% names(dt)) {
    dt[, file_role := "other"]
  }

  if (!"data_file_format" %in% names(dt)) {
    dt[, data_file_format := "other"]
  }

  if (!"is_container" %in% names(dt)) {
    dt[, is_container := FALSE]
  }

  if (!"container_type" %in% names(dt)) {
    dt[, container_type := NA_character_]
  }

  if (!"dataset_root" %in% names(dt)) {
    dt[, dataset_root := NA_character_]
  }

  if (!"mount" %in% names(dt)) {
    dt[, mount := NA_character_]
  }

  if (!"location_path" %in% names(dt)) {
    dt[, location_path := data_file_rel_path_full]
  }

  if (!"file_path" %in% names(dt)) {
    dt[, file_path := location_path]
  }

  dt[, size_num := suppressWarnings(as.numeric(size))]
  dt[is.na(size_num), size_num := 0]

  # Directory entry sizes are filesystem metadata and should not contribute
  # to the logical dataset size.
  dt[, size_counted := data.table::fifelse(
    isdir %in% TRUE,
    0,
    size_num
  )]

  dt[
    is.na(group_key) | group_key == "",
    group_key := paste(
      file_dir,
      file_stem,
      data_file_extension,
      sep = "::"
    )
  ]

  excluded_roles <- c(
    "sidecar",
    "container_support"
  )

  excluded_formats <- c(
    "sidecar",
    "archive",
    "container_support"
  )

  strong_primary_roles <- c(
    "file",
    "dataset_container",
    "dataset_container_component"
  )

  dt[, primary_eligible :=
    !file_role %in% excluded_roles &
    !data_file_format %in% excluded_formats
  ]

  dt[, primary_rank := data.table::fifelse(
    primary_eligible &
      file_role %in% strong_primary_roles,
    1L,
    data.table::fifelse(
      primary_eligible,
      2L,
      3L
    )
  )]

  dt[, logical_path_candidate := data.table::fifelse(
    !is.na(dataset_root) & dataset_root != "",
    dataset_root,
    file_path
  )]

  dt[, logical_name_candidate := data.table::fifelse(
    !is.na(dataset_root) & dataset_root != "",
    basename(dataset_root),
    file_name
  )]

  dt[, logical_rel_candidate := data_file_rel_path_full]

  dt[
    !is.na(dataset_root) &
      dataset_root != "" &
      !is.na(mount) &
      mount != "",
    logical_rel_candidate := inventory_rel_path(
      dataset_root,
      mount
    )
  ]

  dt[, logical_dir_candidate := dirname(
    logical_rel_candidate
  )]

  dt[
    is.na(logical_dir_candidate) |
      logical_dir_candidate == ".",
    logical_dir_candidate := ""
  ]

  dt[, logical_stem_candidate := data.table::fifelse(
    !is.na(dataset_root) & dataset_root != "",
    tools::file_path_sans_ext(
      basename(dataset_root)
    ),
    file_stem
  )]

  dt[, logical_extension_candidate :=
    data.table::fifelse(
      !is.na(container_type) &
        container_type == "esri_file_geodatabase",
      "gdb",
      data.table::fifelse(
        !is.na(container_type) &
          container_type == "esri_arcinfo_grid",
        "adf",
        data.table::fifelse(
          !is.na(container_type) &
            container_type == "esri_coverage",
          "coverage",
          data_file_extension
        )
      )
    )
  ]

  dt[, logical_role_candidate := data.table::fifelse(
    !is.na(container_type) &
      container_type != "" &
      !container_type %in% "esri_info_workspace",
    "dataset_container",
    file_role
  )]

  dt[, logical_is_container_candidate :=
    logical_role_candidate == "dataset_container"
  ]

  primary_lookup <- dt[
    order(
      group_key,
      primary_rank,
      .row_id_inventory
    ),
    .SD[1L],
    by = group_key
  ][
    ,
    .(
      group_key,
      primary_row_id = .row_id_inventory,
      logical_file_name = logical_name_candidate,
      logical_file_path = logical_path_candidate,
      logical_rel_path = logical_rel_candidate,
      logical_dir = logical_dir_candidate,
      logical_stem = logical_stem_candidate,
      logical_extension = logical_extension_candidate,
      logical_format = data_file_format,
      logical_file_role = logical_role_candidate,
      logical_container_type = container_type,
      logical_is_container = logical_is_container_candidate,
      logical_primary_reason = data.table::fifelse(
        primary_rank == 1L,
        "primary_file",
        data.table::fifelse(
          primary_rank == 2L,
          "fallback_first_available",
          "no_eligible_primary"
        )
      )
    )
  ]

  aggregate_lookup <- dt[
    ,
    .(
      logical_size_bytes = sum(
        size_counted,
        na.rm = TRUE
      ),
      logical_size_kb = round(
        sum(size_counted, na.rm = TRUE) / 1024,
        1
      ),
      logical_size_mb = round(
        sum(size_counted, na.rm = TRUE) / 1024^2,
        3
      ),
      n_components = .N,
      n_sidecars = sum(
        file_role == "sidecar",
        na.rm = TRUE
      ),
      sidecar_count = sum(
        file_role == "sidecar",
        na.rm = TRUE
      ),
      n_primary_files = sum(
        primary_eligible &
          file_role %in% strong_primary_roles,
        na.rm = TRUE
      ),
      n_dirs = sum(
        isdir %in% TRUE,
        na.rm = TRUE
      ),
      n_files = sum(
        !isdir %in% TRUE,
        na.rm = TRUE
      ),
      component_exts = paste(
        sort(unique(
          data_file_extension[
            !is.na(data_file_extension) &
              data_file_extension != ""
          ]
        )),
        collapse = ", "
      ),
      component_roles = paste(
        sort(unique(
          file_role[
            !is.na(file_role) &
              file_role != ""
          ]
        )),
        collapse = ", "
      ),
      sidecar_exts = paste(
        sort(unique(
          data_file_extension[
            file_role == "sidecar" &
              !is.na(data_file_extension) &
              data_file_extension != ""
          ]
        )),
        collapse = ", "
      ),
      component_modtime_latest = {
        x <- modtime[!is.na(modtime)]

        if (length(x) > 0L) {
          max(x)
        } else {
          as.POSIXct(
            NA_real_,
            origin = "1970-01-01",
            tz = "UTC"
          )
        }
      },
      source_paths = paste(
        sort(unique(
          file_path[
            !is.na(file_path) &
              file_path != ""
          ]
        )),
        collapse = "; "
      )
    ),
    by = group_key
  ]

  dt <- merge(
    dt,
    aggregate_lookup,
    by = "group_key",
    all.x = TRUE,
    sort = FALSE
  )

  dt <- merge(
    dt,
    primary_lookup,
    by = "group_key",
    all.x = TRUE,
    sort = FALSE
  )

  # A group receives one primary row only when it has an eligible row.
  dt[, primary_dataset :=
    .row_id_inventory == primary_row_id &
    primary_eligible
  ]

  dt[
    file_role %in% excluded_roles |
      data_file_format %in% excluded_formats,
    primary_dataset := FALSE
  ]

  dt[, sidecar_summary := data.table::fifelse(
    sidecar_count > 0L,
    paste0(
      sidecar_count,
      " file(s): ",
      sidecar_exts
    ),
    "None"
  )]

  data.table::setorder(
    dt,
    .row_id_inventory
  )

  helper_cols <- c(
    "size_num",
    "size_counted",
    "primary_eligible",
    "primary_rank",
    ".row_id_inventory",
    "logical_path_candidate",
    "logical_name_candidate",
    "logical_rel_candidate",
    "logical_dir_candidate",
    "logical_stem_candidate",
    "logical_extension_candidate",
    "logical_role_candidate",
    "logical_is_container_candidate"
  )

  helper_cols <- intersect(
    helper_cols,
    names(dt)
  )

  dt[, (helper_cols) := NULL]

  dt[]
}