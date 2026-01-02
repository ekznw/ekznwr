#' @title Extract data
#' @description
#' Extract files from a zipped data archive into a temporary (or other) location.
#' \preformatted{
#' **repo_parent_dir** \\
#'               **|__repo** \\
#'                     **|__zip_arc** \\
#'                              **|__files**
#' }
#' @param repo Keyword or regular expression used to search for the repository inside the `repo_parent_dir`.
#' @param zip_arc Search key (string) to filter for in the `repo`.
#' @param files Search key to filter for files in the zip archive to be extracted (i.e., not all contents of a zip archive need to be extracted). If NULL then all contents will be extracted.
#' @param repo_parent_dir File path indicating where the repository is located. Defaults to the users 'Documents' folder.
#' @param extract_dir Directory in which to extract files in the (compressed) data archive. Defalts to `temp_dir()`.
#' @author Paul J. Gordijn
#' @export
#' @import data.table
dta_extract <- function(
  repo_parent_dir = file.path(path.expand("~"), "Documents"),
  repo = NULL,
  zip_arc = "*.\\.zip$",
  files = NULL
) {
  ":=" <- "%chin%" <- "%ilike%" <- NULL
  "ext" <- "Name" <- NULL

  # get extract dir from options
  extract_dir <- getOption("extract_dir")

  # return object
  arc_ext <- list(
    repo_parent_dir = NULL,
    repo = NULL,
    zip_arc = NULL,
    files = NULL,
    extract_dir = NULL,
    zip_contents = NULL
  )

  # detect repository
  dirs <- list.dirs(path = repo_parent_dir, full.names = TRUE, recursive = FALSE)
  dirs <- dirs[dirs %ilike% repo]
  if (length(dirs) == 0) {
    cli::cli_abort("No {.var repo} folder at: {repo_parent_dir}")
    cli::cli_inform(c("i" = "Sync the repo folder to your local documents."))
    return(arc_ext)
  } else if (length(dirs) > 1) {
    cli::cli_inform(c(
      "x" = paste0("Please refine the repo search key to closely match",
      " the data repository directory basename."
      ),
      "i" = "{length(dirs)} match{?es} detected.",
      " " = "{head(dirs)}",
      "x" = "Exact directory match required!"
    ))
    return(arc_ext)
  } else {
    repo <- basename(dirs[1])
  }

  data_dir <- file.path(repo_parent_dir, repo)
  arc_ext <- list(
    repo_parent_dir = repo_parent_dir,
    repo = repo,
    zip_arc = NULL,
    files = NULL,
    extract_dir = extract_dir,
    zip_contents = NULL,
    zip_arc_list = NULL
  )

  # check for zipped folders
  zips <- list.files(path = data_dir, full.names = TRUE,
    recursive = TRUE
  )
  zips <- zips[zips %ilike% "*.\\.zip$"]
  arc_ext$zip_arc_list <- zips
  zips <- zips[zips %ilike% zip_arc]

  if (length(zips) > 1 || length(zips) == 0) {
    cli::cli_inform(c(
      "i" = "In repo folder: {data_dir}",
      " " = "More than one or no zip file archive names match your search key: {zip_arc} (see below).",
      ">" = "Refine your search key ({.var zip_arc}) to select only one zip archive.",
      " " = "E.g., 'landcover_v1*.\\.zip$' to search for a zip archive with text 'landcover_v1**random text**.zip"
    ))
    print(zips)
    return(arc_ext)
  }

  # set the zip_arc to the first one in the list
  zip_arc <- zips[1]
  # update the arc_ext
  arc_ext$zip_arc <- zip_arc

  # okay now extract zip file if no sub files are wanted
  lsf <- utils::unzip(
    zipfile = zip_arc,
    list = TRUE
  )
  lsf <- data.table::as.data.table(lsf)
  arc_ext$zip_contents <- lsf
  if (is.null(files)) files <- paste(lsf$Name, collapse = "|")
  lsfltr <- lsf[Name %ilike% files]
  if (nrow(lsfltr) == 0) {
    cli::cli_inform(c(
      "i" = "Not file matches for your files: {files} in {zip_arc}",
      " " = "Available files:"
    ))
    print(lsf)
    arc_ext$zip_contents$ext <- FALSE
    return(arc_ext)
  }
  already_there <- list.files(
    path = extract_dir,
    recursive = TRUE
  )
  already_there <- already_there[
    already_there %ilike% tools::file_path_sans_ext(basename(zip_arc))
  ]

  # extract unextracted files
  utils::unzip(
    zipfile = zip_arc,
    files = lsfltr$Name[!lsfltr$Name %chin% already_there],
    exdir = extract_dir
  )

  # update table showing what files have been extracted
  already_there <- list.files(
    path = extract_dir,
    recursive = TRUE
  )
  already_there <- already_there[
    already_there %ilike% tools::file_path_sans_ext(basename(zip_arc))
  ]
  arc_ext$zip_contents[, ext := data.table::fifelse(
    Name %chin% already_there, TRUE, FALSE
  )]
  invisible(arc_ext)
}
