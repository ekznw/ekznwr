#' Read metadata tables from a Word document
#'
#' Extracts and reshapes tables in a `.docx` document.
#'
#' @param this_file Path to a Word document.
#'
#' @return A named list of data tables extracted from the document.
#' @export
read_word_tables <- function(
  this_file = NULL # document path
) {
  "cell_id" <- "col_ids" <- "col_span" <- "content_type" <- "description" <-
    "field" <- "field_name" <- "grp" <- "h1" <- "newcol" <- "row_id" <-
    "row_span" <- "section" <- "text" <- "V1" <- "value" <- NULL
  "." <- ".N" <- ".SD" <- ":=" <- "%ilike%" <- NULL
  doc <- officer::read_docx(this_file)

  # Get all table rows
  tbls <- officer::docx_summary(doc) |> data.table::setDT()
  tbls <- tbls[content_type %ilike% "table"]
  # Split by table_id
  tables_list <- split(tbls, by = "table_index", keep.by = FALSE)

  cleaned_tables <- lapply(seq_along(tables_list), function(i) {
    # Convert to data.table
    tbl <- data.table::data.table(tables_list[[i]])
    tbl <- tbl[, col_span := as.integer(col_span)][,
      .(row_id, row_span, cell_id, col_span, text)
    ]

    # Expand horizontal spans
    tbl[, col_ids := lapply(seq_len(.N), function(i) {
      cell_id[i]:(cell_id[i] + col_span[i] - 1)
    })]

    # Expand vertical spans properly
    tbl_expanded <- tbl[rep(seq_len(.N), times = row_span)]

    # Expand col_ids and text
    dt_expanded <- tbl_expanded[, .(
      col_id = unlist(col_ids),
      text = rep(text, lengths(col_ids))
    ), by = row_id]

    # Reshape to wide table
    dt_wide <- data.table::dcast(dt_expanded, row_id ~ col_id,
      value.var = "text", fill = NA,
      fun.aggregate = function(x) paste(x, collapse = " ")
    )
    tryCatch({
      dt_wide <- dt_wide[, row_id := NULL]
      names(dt_wide) <- paste0("V", names(dt_wide))
      dt_wide <- dt_wide[, V1 := gsub(
        pattern = "\t", replacement = "", trimws(V1)
      )][, V1 := gsub(pattern = " {2,}", replacement = " ", V1)]

      # get column with values
      col_charn <- apply(dt_wide, 2, function(x) {
        mean(nchar(x), na.rm = TRUE)
      })
      col_charn <- col_charn[!names(col_charn) %in% "V1"]
      col_catd <- apply(dt_wide, 2, function(x) {
        all(any(x %ilike% "categorization|categorisation|metadata revision"))
      })
      col_catd <- col_catd[!names(col_catd) %in% "V1"]
      col_charn <- col_charn[which(col_catd == TRUE)]
      vcol <- names(col_charn)[which.max(col_charn)]
      idx <- which(names(dt_wide) %in% vcol):ncol(dt_wide)
      dt_wide <- dt_wide[,
        newcol := apply(
          .SD, 1, function(x) paste(stats::na.omit(x), collapse = " \n ")
        ),
        .SDcols = idx
      ]
      # extract attribute description
      attr_rows <- which(dt_wide$V1 %ilike% "attribute description"):(
        which(dt_wide$V1 %ilike% "metadata revision") - 1L
      )
      attr <- dt_wide[attr_rows]
      data.table::setnames(attr, old = c("V1", "V2", "newcol"),
        new = c("field_name", "alias", "description")
      )
      attr <- attr[!field_name %in% "Attribute Description",
        .(field_name, alias, description)
      ]
      dt_wide <- dt_wide[!attr_rows]
      data.table::setnames(
        dt_wide, old = c("V1", "newcol"), new = c("field", "value"),
        skip_absent = TRUE
      )
      dt_wide <- dt_wide[, .(field, value)]

      # identify header rows
      dt_wide[, h1 := data.table::fifelse(field %in% value, TRUE, FALSE)]
      # 1. Make sure section is only set on header rows initially
      dt_wide[h1 == TRUE, section := field]

      # 2. Create a group index for each header "block"
      dt_wide[, grp := cumsum(h1)]

      # 3. Extract the header values in order
      headers <- dt_wide[h1 == TRUE, field]

      # 4. Fill section for rows after the first header
      dt_wide[grp > 0, section := headers[grp[grp > 0]]]

      # 5. (Optional) Drop the helper column
      dt_wide <- dt_wide[h1 == FALSE][
        !trimws(value) %in% "Description", .(section, field, value)
      ]
      list(main = dt_wide, attr_dsc = attr)
    }, error = function(e) {
      cli::cli_alert("Non-standard/additional table")
      list(dt_wide)
    })
  })
  flat <- purrr::list_flatten(cleaned_tables)

  nm <- names(flat)
  if (is.null(nm)) nm <- rep("", length(flat))         # handle NULL names
  idx <- which(is.na(nm) | nm == "")                   # unnamed positions

  # Assign xtra01, xtra02, ... in order of appearance
  nm[idx] <- sprintf("xtra%02d", seq_along(idx))

  # Make sure final names are unique (won't change order)
  names(flat) <- make.unique(nm, sep = "_")
  flat
}

# internal escape regex patterns
escape_regex <- function(x) {
  gsub("([\\^\\$\\.\\|\\(\\)\\[\\]\\{\\}\\+\\*\\?\\\\])", "\\\\\\1", x)
}

# internal for safe ilike that escapes regex
safe_ilike <- function(x, pattern) {
  pattern <- escape_regex(pattern)
  x[x %ilike% pattern]
}

# Internal function to 'map' zip files in a parent directory. This will save the contents in the parent repo.
map_dir <- function(parent_dir = NULL) {
  # check if parent_dir exists
  if (!file.exists(parent_dir)) {
    cli::cli_abort("Parent directory not found: {parent_dir}")
  }

  cache_file <- file.path(parent_dir, ".zipflst.rds")
  if (file.exists(cache_file)) {
    info <- file.info(cache_file)
    age_secs <- as.numeric(Sys.time() - info$mtime, units = "secs")

    if (
      age_secs <= getOption(
        "ekznwr.max_map_age",
        default = 60 * 60 * 24
      )
    ) {
      # Cached file is recent enough: use it
      flst <- readRDS(cache_file)
    } else {
      # Cached file is too old: rebuild
      cli::cli_inform(
        c("i" = "Please wait: mapping archived files in the parent repo ...")
      )
      flst <- list.files(
        path = parent_dir,
        pattern = "\\.zip$",
        recursive = TRUE
      )
      saveRDS(flst, cache_file)
    }
  } else {
    # No cache yet: build it
    flst <- list.files(
      path = parent_dir,
      pattern = "\\.zip$",
      recursive = TRUE
    )
    saveRDS(flst, cache_file)
  }
  flst
}

# internal function to normalize text to snake case
# and general neatening up
normalize <- function(x) {
  x <- tolower(x)
  x <- gsub("[()]", "", x)
  x <- gsub("[^a-z0-9]+", " ", x)
  x <- trimws(x)
  x
}

#' Check TIFF raster files
#'
#' Attempts to read and process each supplied TIFF with [terra::rast()].
#' Files that cannot be read are deleted.
#'
#' @param tifs Character vector of TIFF file paths.
#'
#' @return A logical vector indicating whether each file was read
#'   successfully.
#' @export
check_tifs <- function(
  tifs = NULL
) {
  sapply(tifs, function(f) {
    tryCatch({
      # Try reading raster
      r <- terra::rast(f)
      # force read each raster cell -- if error it will complain
      terra::app(r, fun = function(x) x, cores = 1)
      terra::minmax(r)
      # If successful, return filename (or do further processing)
      TRUE
    }, error = function(e) {
      message("Error reading ", f, ": ", e$message)
      message("Deleting corrupted file: ", f)
      # Delete the file
      unlink(f)
      # Return NULL to keep list clean
      FALSE
    })
  }, USE.NAMES = FALSE)
}

#' Null coalescing helper
#'
#' @name null-coalesce
#' @aliases %||%
#' @param x A value to inspect.
#' @param y A fallback value.
#'
#' @return `y` when `x` is `NULL`, empty, or entirely `NA`; otherwise `x`.
#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

concat_project_code <- function(
  root = ".",
  r_folder = "R",
  app_file = "app.R",
  output_prefix = "combined_code",
  version_file = "VERSION.txt",
  wanted = NULL
) {
    # STEP 1: HANDLE VERSIONING
  version_path <- file.path(root, version_file)
  if (!file.exists(version_path)) {
    version <- "1.0.0"
  } else {
    version <- readLines(version_path, warn = FALSE)[1]
  }
  # split version
  parts <- strsplit(version, "\\.")[[1]]
  major <- as.integer(parts[1])
  minor <- as.integer(parts[2])
  patch <- as.integer(parts[3])
  # increment patch
  patch <- patch + 1
  new_version <- paste(major, minor, patch, sep = ".")
  # save updated version
  writeLines(new_version, version_path)
  message("Version updated to: v", new_version)
  # STEP 2: PATHS
  app_path <- file.path(root, app_file)
  r_path   <- file.path(root, r_folder)
  if (!file.exists(app_path)) {
    stop("app.R not found in root: ", root)
  }
  if (!dir.exists(r_path)) {
    stop("R folder not found in root: ", root)
  }
  # STEP 3: OUTPUT FILE
  output_file <- paste0(output_prefix, "_v", new_version, ".txt")
  con <- file(output_file, open = "w")
  # STEP 4: HELPER FUNCTION
  write_file <- function(path) {
    cat(
      "\n\n############################################################\n",
      file = con
    )
    cat(
      paste0("# FILE: ", path, "\n"),
      file = con
    )
    cat(
      "############################################################\n\n",
      file = con
    )
    lines <- readLines(path, warn = FALSE)
    writeLines(lines, con)
  }
  # STEP 5: WRITE HEADER
  cat(
    paste0(
      "================\n",
      "# COMBINED PROJECT CODE\n",
      "# Version: v", new_version, "\n",
      "# Generated: ", Sys.time(), "\n",
      "================\n\n"
    ),
    file = con
  )
  # STEP 6: WRITE FILES
  write_file(app_path)
  r_files <- list.files(
    r_path,
    pattern = "\\.R$",
    full.names = TRUE
  )
  if (!is.null(wanted)) r_files <- r_files[r_files %ilike% wanted]
  r_files <- sort(r_files)
  for (f in r_files) {
    write_file(f)
  }
  close(con)
  message("Combined file written to: ", output_file)
}

#' Bundle project code as text files
#'
#' Copies an application's R source files into a bundle directory and changes
#' their extensions to `.txt`.
#'
#' @param root Project root directory.
#' @param r_folder Source directory, relative to `root`.
#' @param app_file Application entry-point file, relative to `root`.
#' @param output_ext Output extension. Currently reserved for future use.
#' @param output_folder Bundle directory, relative to `root`.
#' @param wanted Optional filter reserved for future use.
#'
#' @return Invisibly returns the result of the final file-copy operation.
#' @export
bundle_project_code <- function(
  root = ".",
  r_folder = "R",
  app_file = "app.R",
  output_ext = "txt",
  output_folder = "bundle",
  wanted = NULL
) {
  # make/check bundle dir
  unlink(file.path(root, output_folder, "R"), recursive = TRUE)
  dir.create(file.path(root, output_folder, "R"), recursive = TRUE,
    showWarnings = FALSE)

  # get files
  fs <- list.files(file.path(root, r_folder), pattern = "\\.R$")

  # copy to bundle dir
  file.copy(
    file.path(root, r_folder, fs),
    gsub("\\.R$", "\\.txt", file.path(root, output_folder, "R", fs)),
      overwrite = TRUE
  )
  # copy app file
  if (file.exists(file.path(root, app_file))) {
    file.copy(
      file.path(root, app_file),
      gsub("\\.R$", "\\.txt", file.path(root, output_folder, app_file)),
        overwrite = TRUE
    )
  }
}
