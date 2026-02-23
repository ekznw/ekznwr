#' Short internal functions
#'
#' Turns parameters into a list
#' An alias for list in pipe processing.
#' Paul J. Gordijn
#' @noRd
#' 
#' 
#' @export
# internal function to read metadata from docx tables
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
          .SD, 1, function(x) paste(na.omit(x), collapse = " \n ")
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

#' @export
# internal escape regex patterns
escape_regex <- function(x) {
  gsub("([\\^\\$\\.\\|\\(\\)\\[\\]\\{\\}\\+\\*\\?\\\\])", "\\\\\\1", x)
}

#' @export
# internal for safe ilike that escapes regex
safe_ilike <- function(x, pattern) {
  pattern <- escape_regex(pattern)
  x[x %ilike% pattern]
}

#' @export
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

    if (age_secs <= getOption("max_map_age")) {
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

#' @export
# internal function to normalize text to snake case
# and general neatening up
normalize <- function(x) {
  x <- tolower(x)
  x <- gsub("[()]", "", x)
  x <- gsub("[^a-z0-9]+", " ", x)
  x <- trimws(x)
  x
}

#' @export
# internal function to check list of tifs for validity
# returns a logical vector indicating which have errors
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
