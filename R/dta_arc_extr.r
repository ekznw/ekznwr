#' Normalise an archive member path
#'
#' @param x Character vector.
#'
#' @return Character vector using forward slashes.
#' @keywords internal
archive_member_normalise <- function(x) {
  x <- gsub("\\\\", "/", x)
  sub("^\\./+", "", x)
}


#' Check whether archive member paths are safe
#'
#' Rejects absolute paths, Windows drive paths, and parent-directory traversal.
#'
#' @param x Character vector of archive member paths.
#'
#' @return Logical vector.
#' @keywords internal
archive_member_is_safe <- function(x) {
  x <- archive_member_normalise(x)

  vapply(x, function(member) {
    if (is.na(member) || !nzchar(member)) {
      return(FALSE)
    }

    if (grepl("^/", member) ||
        grepl("^[A-Za-z]:/", member)) {
      return(FALSE)
    }

    parts <- strsplit(
      member,
      split = "/",
      fixed = TRUE
    )[[1L]]

    !any(parts == "..")
  }, logical(1))
}


#' Match a regular expression with a structured error
#'
#' @param x Character vector.
#' @param pattern Character scalar.
#' @param argument Argument name used in an error message.
#'
#' @return Logical vector.
#' @keywords internal
archive_regex_match <- function(x, pattern, argument) {
  tryCatch(
    grepl(
      pattern = pattern,
      x = x,
      ignore.case = TRUE,
      perl = TRUE
    ),
    error = function(e) {
      cli::cli_abort(c(
        "Invalid regular expression supplied to {.arg {argument}}.",
        "x" = conditionMessage(e)
      ))
    }
  )
}


#' Construct a stable archive-content signature
#'
#' @param x Archive listing returned by `utils::unzip(list = TRUE)`.
#'
#' @return data.table.
#' @keywords internal
archive_listing_signature <- function(x) {
  x <- data.table::as.data.table(data.table::copy(x))

  required <- c("Name", "Length", "Date")
  missing <- setdiff(required, names(x))

  if (length(missing) > 0L) {
    cli::cli_abort(
      "Archive listing is missing field{?s}: {missing}."
    )
  }

  out <- x[
    ,
    .(
      Name = archive_member_normalise(Name),
      Length = as.numeric(Length),
      Date = as.character(Date)
    )
  ]

  data.table::setorder(out, Name, Length, Date)
  out[]
}


#' Compare two archive listings
#'
#' @param x,y Archive listing tables.
#'
#' @return Logical scalar.
#' @keywords internal
archive_listings_equal <- function(x, y) {
  isTRUE(all.equal(
    archive_listing_signature(x),
    archive_listing_signature(y),
    check.attributes = FALSE
  ))
}


#' Test whether an archive member exists in an extraction directory
#'
#' @param archive_dir Extraction directory.
#' @param member Archive member name.
#'
#' @return Logical scalar.
#' @keywords internal
archive_member_exists <- function(archive_dir, member) {
  member <- archive_member_normalise(member)
  member <- sub("/+$", "", member)

  if (!nzchar(member)) {
    return(dir.exists(archive_dir))
  }

  file.exists(file.path(archive_dir, member))
}

#' Extract files from a ZIP archive
#'
#' Searches for one ZIP archive under `parent_dir` and extracts either all
#' archive members or members matching `arc_file_filter`.
#'
#' Archive members are extracted into a directory beneath
#' `getOption("ekznwr.extract_dir")`. The archive's relative path is retained,
#' without its `.zip` extension, to avoid collisions between archives with the
#' same basename in different source directories.
#'
#' @param parent_dir Directory within which ZIP archives are searched.
#' @param arc_file Character scalar. Regular expression that must match exactly
#'   one ZIP archive path.
#' @param arc_file_filter Optional character scalar. Regular expression used to
#'   select archive members. If `NULL`, all archive members are selected.
#' @param overwrite Logical. If `TRUE`, replace an existing extraction when the
#'   stored archive listing differs from the current archive.
#'
#' @return Invisibly returns a list describing the archive, selected members,
#'   extraction directory, and extraction status.
#' @export
dta_arc_extr <- function(
  parent_dir = file.path(path.expand("~"), "Documents"),
  arc_file = NULL,
  arc_file_filter = NULL,
  overwrite = FALSE
) {
  if (!is.character(parent_dir) ||
      length(parent_dir) != 1L ||
      is.na(parent_dir) ||
      !nzchar(parent_dir)) {
    cli::cli_abort(
      "{.arg parent_dir} must be one non-empty directory path."
    )
  }

  parent_dir <- path.expand(parent_dir)

  if (!dir.exists(parent_dir)) {
    cli::cli_abort(
      "Parent directory does not exist: {.path {parent_dir}}."
    )
  }

  parent_dir <- normalizePath(
    parent_dir,
    mustWork = TRUE,
    winslash = "/"
  )

  if (!is.character(arc_file) ||
      length(arc_file) != 1L ||
      is.na(arc_file) ||
      !nzchar(arc_file)) {
    cli::cli_abort(
      "{.arg arc_file} must be one non-empty regular expression."
    )
  }

  if (!is.null(arc_file_filter) &&
      (!is.character(arc_file_filter) ||
        length(arc_file_filter) != 1L ||
        is.na(arc_file_filter) ||
        !nzchar(arc_file_filter))) {
    cli::cli_abort(
      "{.arg arc_file_filter} must be NULL or one non-empty regular expression."
    )
  }

  if (!is.logical(overwrite) ||
      length(overwrite) != 1L ||
      is.na(overwrite)) {
    cli::cli_abort(
      "{.arg overwrite} must be one non-missing logical value."
    )
  }

  extract_dir <- getOption(
    "ekznwr.extract_dir",
    default = file.path(tempdir(), "ekznw_dta")
  )

  extract_dir <- path.expand(extract_dir)

  if (!dir.exists(extract_dir)) {
    dir.create(
      extract_dir,
      recursive = TRUE,
      showWarnings = FALSE
    )
  }

  if (!dir.exists(extract_dir)) {
    cli::cli_abort(
      "Could not create archive extraction directory: {.path {extract_dir}}."
    )
  }

  extract_dir <- normalizePath(
    extract_dir,
    mustWork = TRUE,
    winslash = "/"
  )

  archive_map <- map_dir(parent_dir = parent_dir)

  zip_files <- archive_map[
    grepl("\\.zip$", archive_map, ignore.case = TRUE)
  ]

  archive_matches <- zip_files[
    archive_regex_match(
      zip_files,
      pattern = arc_file,
      argument = "arc_file"
    )
  ]

  if (length(archive_matches) == 0L) {
    cli::cli_abort(c(
      "No ZIP archive matched {.arg arc_file}.",
      "i" = "Parent directory: {.path {parent_dir}}",
      "i" = "Pattern: {.val {arc_file}}"
    ))
  }

  if (length(archive_matches) > 1L) {
    examples <- paste(
      utils::head(archive_matches, 10L),
      collapse = ", "
    )

    cli::cli_abort(c(
      "{.arg arc_file} matched more than one ZIP archive.",
      "i" = "{length(archive_matches)} archives matched.",
      "i" = "Matches include: {examples}"
    ))
  }

  archive_rel_path <- archive_member_normalise(
    archive_matches[[1L]]
  )

  archive_path <- file.path(
    parent_dir,
    archive_rel_path
  )

  archive_path <- normalizePath(
    archive_path,
    mustWork = TRUE,
    winslash = "/"
  )

  archive_rel_no_ext <- tools::file_path_sans_ext(
    archive_rel_path
  )

  archive_dir <- file.path(
    extract_dir,
    archive_rel_no_ext
  )

  # Support extraction folders produced by the previous implementation.
  legacy_archive_dir <- file.path(
    extract_dir,
    basename(archive_rel_path)
  )

  if (!dir.exists(archive_dir) &&
      dir.exists(legacy_archive_dir)) {
    archive_dir <- legacy_archive_dir
  }

  archive_dir <- normalizePath(
    archive_dir,
    mustWork = FALSE,
    winslash = "/"
  )

  dir.create(
    archive_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  metadata_file <- file.path(
    archive_dir,
    ".arc_ext.rds"
  )

  legacy_metadata_file <- file.path(
    archive_dir,
    ".arc_ext"
  )

  archive_contents <- utils::unzip(
    zipfile = archive_path,
    list = TRUE
  )

  archive_contents <- data.table::as.data.table(
    archive_contents
  )

  if (nrow(archive_contents) == 0L) {
    cli::cli_abort(
      "ZIP archive contains no members: {.path {archive_path}}."
    )
  }

  archive_contents[
    ,
    Name := archive_member_normalise(Name)
  ]

  if (is.null(arc_file_filter)) {
    selected_idx <- rep(TRUE, nrow(archive_contents))
  } else {
    selected_idx <- archive_regex_match(
      archive_contents$Name,
      pattern = arc_file_filter,
      argument = "arc_file_filter"
    )
  }

  selected <- archive_contents[selected_idx]

  if (nrow(selected) == 0L) {
    cli::cli_abort(c(
      "No archive members matched {.arg arc_file_filter}.",
      "i" = "Archive: {.path {archive_path}}",
      "i" = "Pattern: {.val {arc_file_filter}}"
    ))
  }

  safe <- archive_member_is_safe(selected$Name)

  if (!all(safe)) {
    unsafe_members <- paste(
      selected$Name[!safe],
      collapse = ", "
    )

    cli::cli_abort(c(
      "Archive extraction was stopped because unsafe paths were detected.",
      "x" = "Unsafe member{?s}: {unsafe_members}"
    ))
  }

  existing_metadata_file <- if (file.exists(metadata_file)) {
    metadata_file
  } else if (file.exists(legacy_metadata_file)) {
    legacy_metadata_file
  } else {
    NULL
  }

  if (!is.null(existing_metadata_file)) {
    previous <- tryCatch(
      readRDS(existing_metadata_file),
      error = function(e) {
        cli::cli_abort(c(
          "Could not read existing archive metadata.",
          "i" = "Metadata file: {.path {existing_metadata_file}}",
          "x" = conditionMessage(e)
        ))
      }
    )

    previous_contents <- previous$arc_file_contents

    listing_matches <- !is.null(previous_contents) &&
      archive_listings_equal(
        archive_contents,
        previous_contents
      )

    if (!listing_matches && !isTRUE(overwrite)) {
      cli::cli_abort(c(
        "The archive contents differ from the previously extracted archive.",
        "i" = "Extraction directory: {.path {archive_dir}}",
        "i" = "Use {.code overwrite = TRUE} to replace the existing extraction."
      ))
    }

    if (!listing_matches && isTRUE(overwrite)) {
      unlink(
        archive_dir,
        recursive = TRUE,
        force = TRUE
      )

      dir.create(
        archive_dir,
        recursive = TRUE,
        showWarnings = FALSE
      )
    }
  }

  already_extracted <- vapply(
    selected$Name,
    archive_member_exists,
    logical(1),
    archive_dir = archive_dir
  )

  members_to_extract <- selected$Name[!already_extracted]

  if (length(members_to_extract) > 0L) {
    utils::unzip(
      zipfile = archive_path,
      files = members_to_extract,
      exdir = archive_dir,
      overwrite = FALSE
    )
  }

  archive_contents[
    ,
    ext := vapply(
      Name,
      archive_member_exists,
      logical(1),
      archive_dir = archive_dir
    )
  ]

  arc_ext <- list(
    parent_dir = parent_dir,
    arc_file = archive_rel_path,
    arc_file_path = archive_path,
    arc_file_filter = arc_file_filter,
    arc_file_contents = archive_contents,
    extract_dir = extract_dir,
    archive_dir = archive_dir,
    metadata_file = metadata_file,
    selected_members = selected$Name,
    extracted_members = members_to_extract,
    arc_extracted = length(members_to_extract) > 0L
  )

  if (arc_ext$arc_extracted) {
    arc_ext$msg <- paste(
      length(members_to_extract),
      "archive member(s) extracted."
    )

    cli::cli_inform(c(
      "v" = arc_ext$msg,
      "i" = "Extraction directory: {.path {archive_dir}}"
    ))
  } else {
    arc_ext$msg <- "All selected archive members were already extracted."

    cli::cli_inform(c(
      "i" = arc_ext$msg,
      "i" = "Extraction directory: {.path {archive_dir}}"
    ))
  }

  saveRDS(
    arc_ext,
    metadata_file
  )

  invisible(arc_ext)
}