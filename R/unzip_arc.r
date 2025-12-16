#' @title Extract data
#' @description
#' Extract files from a zipped data archive into a temporary (or other) location.
#' @param repo Keyword or regular expression used to search for the repository.
#' @param repo_dir File path indicating where the repository is located. Defaults to the users 'Documents' folder.
#' @param file Search key (strong) to filter for in the `repo_dir`.
#' @param sub_file Search key to filter for files in the zip archive to be extracted (i.e., not all contents of a zip archive need to be extracted). If NULL then all contents will be extracted.
#' @param extract_dir Directory in which to extract files in the (compressed) data archive. Defalts to `temp_dir()`.
#' @author Paul J. Gordijn
#' @export
dta_extract <- function(
  repo_dir = file.path(path.expand("~"), "Documents"),
  repo = "\\.zip$",
  file = NULL,
  sub_file = NULL,
  extract_dir = tempdir()
) {
  "%ilike%" <- NULL
  # detect repository
  dirs <- list.dirs(path = repo_dir, full.names = TRUE, recursive = FALSE)
  dirs <- dirs[dirs %ilike% repo]
  if (length(dirs == 0)) {
    cli::cli_abort("No {.var repo} folder at: {repo_dir}")
    cli::cli_inform(c("i" = "Sync the repo folder to your local documents."))
  } else if (length(dirs) > 1) {
    cli::cli_inform(c(
      "x" = paste0("Please refine the repo search key to closely match",
      " the data repository directory basename."
      ),
      "i" = "{length(dirs)} match{?es} detected.",
      "-" = "{head(dirs)}"
    ))
    cli::cli_abort("Exact directory match required!")
  }
  data_dir <- file.path(repo_dir, repo)
  # return the data repo dir if there is no repo file specified for extaction
  if (!is.null(file)) return(data_dir)

  # search for data and extract
  data_dir_ls <- list.files(path = data_dir, recursive = TRUE)
  data_dir_ls <- data_dir_ls[data_dir_ls %ilike% file]
  if (length(data_dir_ls) == 0) {
    cli::cli_abort(c(
      "No {.var file} file found in the data repository.",
      "i" = "Search key used: {.val {file}}",
      "i" = "Expected a ZIP archive (.zip) in {.path {data_dir}}"
    ))
  }

  # Check that matched files are ZIP archives
  is_zip <- grepl("\\.zip$", data_dir_ls, ignore.case = TRUE)

  if (!any(is_zip)) {
    cli::cli_abort(c(
      "Matched {length(data_dir_ls)} file{?s}, but none { ?is/are } ZIP archives.",
      "i" = "Matched file{?s}: {.path {basename(data_dir_ls)}}",
      "i" = "Expected a {.file .zip} archive"
    ))
  }

  # narrow search down to one zip archive
  data_dir_ls[is_zip]
}
