#' @title Get data file path
#' @description Searches within a parent directory for a data file given its extension and search key.
#' @param parent_dir The parent directory within which to search for the data file.
#' @param search_key The search key to use to find the exact data file. NBS! This is only applied to the filename, not the directory/path (see `dir_filter`). Ideally provide the exact file name of the data file. Else 'regex' wildcards can be used.
#' @param dir_filter Parameter used to filter files by directory regex.
#' @author Paul J. Gordijn
#' @export
#' @import data.table
dta_find <- function(
  parent_dir = "~",
  search_key = NULL,
  dir_filter = NULL
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

  # get file mapping for the parent dir
  fs <- list.files(
    path = parent_dir,
    pattern = search_key,
    full.names = TRUE,
    recursive = TRUE
  )
  if (!is.null(dir_filter)) fs <- fs[fs %ilike% dir_filter]
  if (length(fs) == 0) {
    cli::cli_inform(c(
      "i" = "No match for data file.",
      " " = "Check the correct {.var parent_dir} is being searched, and",
      " " = "refine your search key: {search_key}."
    ))
    stop()
  } else if (length(fs) > 1) {
    cli::cli_inform(c(
      "i" = "Refine your search key: {search_key}.",
      " " = "{length(fs)} file{?s} printed below."
    ))
    print(fs)
    stop()
  }
  fs
}