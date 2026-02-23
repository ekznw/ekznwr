#' @title Generates file path from extracted data
#' @description Uses the result of a successful running of `dta_extract()` to generate a file path.
#' @param extract_i Result from `dta_extract()`.
#' @param name String used to match the file name of a file from the extracted archive.
#' @param row_n Integer specifying the item as presented in the order of the zip_contents table in the extracted archive.
#' @param last_slash Removes the last slash, if it exists, on the returned file path. Defaults to `TRUE`.
#' @author Paul J. Gordijn
#' @export
#' @import data.table
dta_fp <- function(
  extract_i = NULL,
  name = NULL,
  row_n = NULL,
  last_slash = TRUE
) {
  "Name" <- NULL
  # check the extract info has required list items
  if (!all(
    "arc_file" %in% names(extract_i),
    "arc_file_contents" %in% names(extract_i),
    "parent_dir" %in% names(extract_i),
    "extract_dir" %in% names(extract_i)
  )) {
    cli::cli_alert_warning(
      "Missing information from the {.var extract_i}."
    )
  }

  zc <- extract_i$arc_file_contents

  # if param name is present use it to filter the Name field
  if (!is.null(name)) {
    zc_fltr <- zc[Name %in% name]
    row_n <- NULL
  }
  # if there is a row number use that
  if (!is.null(row_n)) {
    if (row_n > nrow(zc)) {
      cli::cli_abort(
        "Row number {.var row_n} greater number of rows in table."
      )
    }
    zc_fltr <- zc[row_n]
  }

  if (nrow(zc_fltr) == 0 || nrow(zc_fltr) > 1) {
    cli::cli_inform(c(
      "i" = "Please refine search key: {name} for a match.",
      " " = "Single match required."
    ))
    return(zc)
  }

  # return file path
  fp <- file.path(extract_i$extract_dir,
    basename(extract_i$arc_file), zc_fltr$Name[1]
  )
  gsub("[/\\\\]+$", "", fp)
}