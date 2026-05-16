#' @title Extract archive data
#' @description
#' Extract files from a zipped data archive into a temporary (or other) location.
#' \preformatted{
#' **parent_dir** \\
#'               **|__miscellaneous repo's** \\
#'                     **|__arc_file** \\
#'                              **|__arc_file_contents**
#' }
#' @param parent_dir File path wherein to search for archived data sets. Defaults to the users 'Documents' folder.
#' @param arc_file Search key (string) used to filter by when searching for the archived data in the `parent_dir`.
#' @param arc_file_filter Search key to filter for files within the (zip) archive to be extracted (i.e., not all contents of the archive need to be extracted). If NULL then all contents will be extracted.
#' @author Paul J. Gordijn
#' @export
#' @import data.table
dta_arc_extr <- function(
  parent_dir = file.path(path.expand("~"), "Documents"),
  arc_file = NULL,
  arc_file_filter = NULL
) {
  ":=" <- "%chin%" <- "%ilike%" <- NULL
  "ext" <- "Name" <- NULL

  # get extract dir from options
  extract_dir <- getOption("extract_dir")

  # type of archive files this function deals with
  arc_types <- "*.\\.zip$"

  # null return object
  arc_ext <- list(
    parent_dir = NULL,
    arc_file = NULL,
    arc_file_filter = NULL,
    arc_file_contents = NULL,
    extract_dir = NULL,
    arc_extracted = FALSE
  )

  # call map_dir to generate or read the archive map
  arc_flst <- ekznwr::map_dir(parent_dir = parent_dir)

  # update the arc_ext metadata
  arc_ext <- list(
    parent_dir = parent_dir,
    extract_dir = extract_dir
  )

  # filter the list of archive files
  arc <- arc_flst[arc_flst %ilike% arc_types]
  arc_ext$arc_file <- arc
  arc_filtered <- arc[arc %ilike% arc_file]

  if (length(arc_filtered) > 1 || length(arc_filtered) == 0) {
    cli::cli_inform(c(
      "i" = "In {.var parent_dir} (parent directory): {parent_dir}",
      " " = "More than one or no archive file names match your search key: {arc_file_filter} (see below).",
      ">" = "Refine your search key ({.var arc_file_filter}) to select only one zip archive.",
      " " = "E.g., 'landcover_v1*.\\.zip$' to search for a zip archive with text 'landcover_v1**random text**.zip"
    ))
    print(arc_filtered)
    return(arc_ext)
  }

  # set the arc_filtered to the first one in the list
  arc_filtered <- arc_filtered[1]
  # update the arc_ext
  arc_ext$arc_file <- arc_filtered

  # list contents of the zip file
  lsf <- utils::unzip(
    zipfile = file.path(parent_dir, arc_ext$arc_file),
    list = TRUE
  )
  lsf <- data.table::as.data.table(lsf)
  arc_ext$arc_file_contents <- lsf
  if (is.null(arc_file_filter)) {
    arc_file_filter <- paste(lsf$Name, collapse = "|")
  }
  arc_ext$arc_file_filter <- arc_file_filter
  lsfltr <- lsf[Name %ilike% arc_file_filter]
  if (nrow(lsfltr) == 0) {
    cli::cli_inform(c(
      "i" = paste0("Not file matches for your files: {arc_file_filter}",
        " in {arc_ext$arc_file}"
      ),
      " " = "Available files:"
    ))
    print(lsf)
    arc_ext$arc_file_contents$ext <- FALSE
    return(arc_ext)
  }

  # check if there is already an archive with the arc_file name
  if (
    file.exists(file.path(extract_dir, basename(arc_ext$arc_file), ".arc_ext"))
  ) {
    # read archive metadata
    arc_ext_orig <- readRDS(
      file.path(extract_dir, basename(arc_ext$arc_file), ".arc_ext")
    )

    t <- all.equal(lsf[, .(Name, Length, Date)],
      arc_ext_orig$arc_file_contents[, .(Name, Length, Date)]
    )
  } else {
    t <- NULL
  }
  if (!is.null(t)) {
    if (t == TRUE) {
      arc_ext$msg <- "Err ekz:01. Duplicate archive LIKE contents."
      arc_ext
      cli::cli_inform(
        c("x" = arc_ext$msg, " " = "Archive {arc_ext$arc_file} not extracted.")
      )
    } else {
      arc_ext$msg <- "Err ekz:02. Duplicate archive DISSIMLAR contents."
      cli::cli_inform(
        c("x" = arc_ext$msg, " " = "Archive {arc_ext$arc_file} not extracted.")
      )
    }
    return(arc_ext)
  }

  already_there <- list.files(
    path = file.path(extract_dir, basename(arc_ext$arc_file)),
    recursive = TRUE
  )
  already_there <- already_there[
    already_there %ilike% tools::file_path_sans_ext(basename(arc_ext$arc_file))
  ]

  # extract unextracted files
  utils::unzip(
    zipfile = file.path(parent_dir, arc_ext$arc_file),
    files = lsfltr$Name[!lsfltr$Name %chin% already_there],
    exdir = file.path(extract_dir, basename(arc_ext$arc_file))
  )
  arc_ext$arc_extracted <- TRUE

  # update table showing what files have been extracted
  already_there <- list.files(
    path = file.path(extract_dir, basename(arc_ext$arc_file)),
    recursive = TRUE
  )
  already_there <- already_there[
    already_there %ilike% tools::file_path_sans_ext(basename(arc_ext$arc_file))
  ]
  arc_ext$arc_file_contents[, ext := data.table::fifelse(
    Name %chin% already_there, TRUE, FALSE
  )]
  # save the lsf if there isn't one already, and there is an archive folder
  #  for the dataset
  if (all(file.exists(file.path(extract_dir, basename(arc_ext$arc_file))),
    !file.exists(file.path(extract_dir, basename(arc_ext$arc_file), ".arc_ext"))
  )) {
    saveRDS(
      arc_ext, file.path(extract_dir, basename(arc_ext$arc_file), ".arc_ext")
    )
  }
  invisible(arc_ext)
}