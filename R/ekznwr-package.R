#' ekznwr: Data management and GIS utilities
#'
#' Tools for scanning, classifying, grouping, locating, and extracting
#' organisational datasets and geospatial archives.
#'
#' @keywords internal
"_PACKAGE"

#' Package options
#'
#' `ekznwr` uses the following options:
#'
#' - `ekznwr.extract_dir`: directory used when extracting archives. Defaults
#'   to a session-specific temporary directory.
#' - `ekznwr.max_map_age`: maximum age in seconds of a cached archive map.
#'   Defaults to 24 hours.
#'
#' Set options with [options()], for example:
#'
#' ```
#' options(ekznwr.extract_dir = "~/ekznwr_extracts")
#' ```
#'
#' @name ekznwr_options
NULL
