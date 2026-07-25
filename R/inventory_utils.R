#' Require columns in an inventory table
#'
#' @param dt A data frame or data.table.
#' @param required Character vector of required columns.
#' @param caller Calling function name.
#'
#' @return `dt`, invisibly.
#' @keywords internal
inventory_require_columns <- function(dt, required, caller) {
  missing <- setdiff(required, names(dt))

  if (length(missing) > 0L) {
    cli::cli_abort(
      "{.fn {caller}} requires column{?s}: {missing}."
    )
  }

  invisible(dt)
}

#' Normalise a filesystem path for inventory processing
#'
#' @param x Character vector of paths.
#'
#' @return Character vector with forward slashes.
#' @keywords internal
inventory_normalise_path <- function(x) {
  normalizePath(path.expand(x), mustWork = FALSE, winslash = "/")
}

#' Derive a relative path without regex escaping
#'
#' @param path Character vector of full paths.
#' @param root Character scalar or vector of root paths.
#'
#' @return Character vector of paths relative to root where possible.
#' @keywords internal
inventory_rel_path <- function(path, root) {
  path <- inventory_normalise_path(path)
  root <- inventory_normalise_path(root)

  if (length(root) == 1L && length(path) > 1L) {
    root <- rep(root, length(path))
  }

  root <- sub("/+$", "", root)
  out <- path

  prefix <- paste0(root, "/")

  idx <- !is.na(path) &
    !is.na(root) &
    startsWith(path, prefix)

  out[idx] <- substring(path[idx], nchar(prefix[idx]) + 1L)

  idx_same <- !is.na(path) &
    !is.na(root) &
    path == root

  out[idx_same] <- ""

  out
}

#' Clean inventory file stems
#'
#' Removes known sidecar suffixes to derive a logical dataset stem.
#'
#' @param file_name Character vector of file names.
#'
#' @return Character vector.
#' @export
inventory_clean_file_stem <- function(file_name) {
  vapply(file_name, clean_gis_file_stem_one, character(1))
}

clean_gis_file_stem_one <- function(file_name) {
  x <- file_name

  if (grepl("\\.(tif|tiff|ecw)\\.", x, ignore.case = TRUE)) {
    x <- sub("\\.(tif|tiff|ecw).*?$", "", x, ignore.case = TRUE)
    return(x)
  }

  x <- sub("\\.shp\\.xml$", "", x, ignore.case = TRUE)
  x <- sub("\\.sdat\\.aux\\.xml$", "", x, ignore.case = TRUE)
  x <- sub("\\.sdat\\.ovr$", "", x, ignore.case = TRUE)
  x <- sub("\\.img\\.aux\\.xml$", "", x, ignore.case = TRUE)
  x <- sub("\\.aux\\.xml$", "", x, ignore.case = TRUE)
  x <- sub("\\.gpkg-(shm|wal|journal)$", "", x, ignore.case = TRUE)
  x <- sub("\\.(sqlite|sqlite3|db)-(shm|wal|journal)$", "", x, ignore.case = TRUE)
  x <- sub("\\.(rdc|vdc|ref|smp|avl)$", "", x, ignore.case = TRUE)
  x <- sub("\\.(sdat|sgrd|mgrd|prj)$", "", x, ignore.case = TRUE)
  x <- sub("\\.(ige|rrd)$", "", x, ignore.case = TRUE)
  x <- sub("\\.(hdr|stx|clr)$", "", x, ignore.case = TRUE)
  x <- sub("\\.(dat|map|id|ind)$", "", x, ignore.case = TRUE)
  x <- sub("\\.(lax|lasx)$", "", x, ignore.case = TRUE)

  tools::file_path_sans_ext(x)
}


#' Derive compound file extension
#'
#' @param path Character vector of file paths.
#'
#' @return Character vector of normalised extensions.
#' @export
inventory_file_extension <- function(path) {
  ext <- tolower(tools::file_ext(path))

  ext[grepl("\\.tif\\.vat\\.dbf$", path, ignore.case = TRUE)] <- "tif.vat.dbf"
  ext[grepl("\\.tif\\.vat\\.cpg$", path, ignore.case = TRUE)] <- "tif.vat.cpg"
  ext[grepl("\\.tif\\.aux\\.xml$", path, ignore.case = TRUE)] <- "tif.aux.xml"
  ext[grepl("\\.tiff\\.aux\\.xml$", path, ignore.case = TRUE)] <- "tiff.aux.xml"
  ext[grepl("\\.sdat\\.aux\\.xml$", path, ignore.case = TRUE)] <- "sdat.aux.xml"
  ext[grepl("\\.sdat\\.ovr$", path, ignore.case = TRUE)] <- "sdat.ovr"
  ext[grepl("\\.tif\\.xml$", path, ignore.case = TRUE)] <- "tif.xml"
  ext[grepl("\\.img\\.aux\\.xml$", path, ignore.case = TRUE)] <- "img.aux.xml"
  ext[grepl("\\.shp\\.xml$", path, ignore.case = TRUE)] <- "shp.xml"
  ext[grepl("\\.ecw\\.aux\\.xml$", path, ignore.case = TRUE)] <- "ecw.aux.xml"
  ext[grepl("\\.ecw\\.xml$", path, ignore.case = TRUE)] <- "ecw.xml"
  ext[grepl("\\.ecw\\.ovr$", path, ignore.case = TRUE)] <- "ecw.ovr"

  idx <- grepl("\\.gpkg-(shm|wal|journal)$", path, ignore.case = TRUE)
  ext[idx] <- paste0(
    "gpkg-",
    sub(".*\\.gpkg-", "", path[idx], ignore.case = TRUE)
  )

  idx <- grepl("\\.(sqlite|sqlite3|db)-(shm|wal|journal)$", path, ignore.case = TRUE)
  ext[idx] <- tolower(
    sub(".*\\.(sqlite|sqlite3|db)-", "\\1-", path[idx], ignore.case = TRUE)
  )

  idx_aux_xml <- grepl("\\.aux\\.xml$", path, ignore.case = TRUE) &
    !grepl("\\.(sdat|tif|tiff|img)\\.aux\\.xml$", path, ignore.case = TRUE)

  ext[idx_aux_xml] <- "aux.xml"

  ext
}