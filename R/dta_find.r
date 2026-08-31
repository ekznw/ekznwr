#' @title Get data file path
#' @description Searches within a parent directory for a data file or a
#'   directory-backed Esri dataset given its name and a search key.
#' @param parent_dir The parent directory within which to search for the data file.
#' @param search_key The search key to use to find the exact data file. NBS! This is only applied to the filename, not the directory/path (see `dir_filter`). Ideally provide the exact file name of the data file. Else 'regex' wildcards can be used.
#' @param dir_filter Parameter used to filter files by directory regex.
#' @param wanted Regex search key used to filter *for* wanted files.
#' @param unwanted Regex search key used to filter *out* unwanted files.
#' @param ... Reserved for future use.
#' @return The full path for the specified data set. In addition to regular
#'   files, Esri File Geodatabase (`.gdb`) and ArcInfo Binary Grid directories
#'   can be returned. If the data is not found or possible duplicates are
#'   detected, the function will prompt the user accordingly.
#' @author Paul J. Gordijn
#' @export
#' @import data.table
dta_find <- function(
  parent_dir = "~",
  search_key = NULL,
  dir_filter = NULL,
  wanted = NULL,
  unwanted = NULL,
  ...
) {
  # arg evaluation
  if (is.null(parent_dir)) cli::cli_abort("Specify {.var parent_dir}!")

  # check if the parent dir exists
  if (!dir.exists(parent_dir)) {
    cli::cli_abort("Parent directory ({parent_dir}) does not exist.")
  }

  if (is.null(search_key)) {
    cli::cli_abort(
      "The {.var search_key} must be defined."
    )
  }

  # Get files and directories. Only directories representing datasets that
  # can be opened by their directory path are retained as candidates.
  entries <- list.files(
    path = parent_dir,
    full.names = TRUE,
    recursive = TRUE,
    include.dirs = TRUE
  )

  entry_is_dir <- dir.exists(entries)
  files <- entries[!entry_is_dir]
  directories <- entries[entry_is_dir]

  gdb_directories <- directories[
    grepl("\\.gdb$", basename(directories), ignore.case = TRUE)
  ]

  grid_directories <- directories[vapply(
    directories,
    function(directory) {
      component_names <- tolower(basename(files[dirname(files) == directory]))

      "hdr.adf" %in% component_names &&
        any(grepl("^w[0-9]{6}\\.adf$", component_names))
    },
    logical(1)
  )]

  fs <- sort(unique(c(files, gdb_directories, grid_directories)))
  fs <- fs[grepl(search_key, basename(fs))]

  if (!is.null(dir_filter)) fs <- fs[fs %ilike% dir_filter]
  if (!is.null(wanted)) fs <- fs[fs %ilike% wanted]
  if (!is.null(unwanted)) fs <- fs[!fs %ilike% unwanted]
  if (length(fs) == 0) {
    cli::cli_inform(c(
      "i" = "No match for data set.",
      " " = "Check the correct {.var parent_dir} is being searched, and",
      " " = "refine your search key: {search_key}."
    ))
    stop()
  } else if (length(fs) > 1) {
    cli::cli_inform(c(
      "i" = "Refine your search key: {search_key}.",
      " " = "{length(fs)} matching path{?s} printed (& returned) below."
    ))
    print(fs)
    return(fs)
  }
  fs
}
