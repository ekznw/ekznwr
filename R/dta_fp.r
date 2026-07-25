#' Generate a path to an extracted archive member
#'
#' Uses the result returned by [dta_arc_extr()] to generate the path to one
#' extracted archive member.
#'
#' Supply exactly one of `name` or `row_n`.
#'
#' @param extract_i Result returned by [dta_arc_extr()].
#' @param name Character scalar. Regular expression used to match an archive
#'   member name.
#' @param row_n Integer scalar identifying a row in
#'   `extract_i$arc_file_contents`.
#' @param last_slash Logical. If `TRUE`, remove trailing path separators from
#'   the returned path.
#'
#' @return Character scalar containing the extracted file or directory path.
#' @export
dta_fp <- function(
  extract_i = NULL,
  name = NULL,
  row_n = NULL,
  last_slash = TRUE
) {
  if (!is.list(extract_i)) {
    cli::cli_abort(
      "{.arg extract_i} must be a result returned by {.fn dta_arc_extr}."
    )
  }

  if (!"arc_file_contents" %in% names(extract_i)) {
    cli::cli_abort(
      "{.arg extract_i} does not contain {.field arc_file_contents}."
    )
  }

  if (!is.logical(last_slash) ||
      length(last_slash) != 1L ||
      is.na(last_slash)) {
    cli::cli_abort(
      "{.arg last_slash} must be one non-missing logical value."
    )
  }

  selector_count <- sum(
    !is.null(name),
    !is.null(row_n)
  )

  if (selector_count != 1L) {
    cli::cli_abort(
      "Supply exactly one of {.arg name} or {.arg row_n}."
    )
  }

  contents <- data.table::as.data.table(
    data.table::copy(extract_i$arc_file_contents)
  )

  if (!"Name" %in% names(contents)) {
    cli::cli_abort(
      "{.field arc_file_contents} must contain a {.field Name} column."
    )
  }

  if (nrow(contents) == 0L) {
    cli::cli_abort(
      "{.field arc_file_contents} contains no archive members."
    )
  }

  if (!is.null(name)) {
    if (!is.character(name) ||
        length(name) != 1L ||
        is.na(name) ||
        !nzchar(name)) {
      cli::cli_abort(
        "{.arg name} must be one non-empty character value."
      )
    }

    matched <- tryCatch(
      grepl(
        pattern = name,
        x = contents$Name,
        ignore.case = TRUE,
        perl = TRUE
      ),
      error = function(e) {
        cli::cli_abort(
          c(
            "Invalid regular expression supplied to {.arg name}.",
            "x" = conditionMessage(e)
          )
        )
      }
    )

    selected <- contents[matched]

    if (nrow(selected) != 1L) {
      examples <- paste(
        utils::head(selected$Name, 10L),
        collapse = ", "
      )

      cli::cli_abort(c(
        "The {.arg name} selector must match exactly one archive member.",
        "i" = "{nrow(selected)} matches were found.",
        "i" = if (nrow(selected) > 0L) {
          "Matches include: {examples}"
        } else {
          "No archive members matched {.val {name}}."
        }
      ))
    }
  } else {
    if (!is.numeric(row_n) ||
        length(row_n) != 1L ||
        is.na(row_n) ||
        row_n != as.integer(row_n)) {
      cli::cli_abort(
        "{.arg row_n} must be one whole number."
      )
    }

    row_n <- as.integer(row_n)

    if (row_n < 1L || row_n > nrow(contents)) {
      cli::cli_abort(
        c(
          "{.arg row_n} is outside the archive table.",
          "i" = "Valid rows are 1 to {nrow(contents)}."
        )
      )
    }

    selected <- contents[row_n]
  }

  member <- selected$Name[[1L]]
  member <- archive_member_normalise(member)

  if (!archive_member_is_safe(member)) {
    cli::cli_abort(
      "The selected archive member contains an unsafe path: {.path {member}}."
    )
  }

  # New dta_arc_extr() results provide archive_dir directly.
  archive_dir <- extract_i$archive_dir

  # Backward compatibility with older dta_arc_extr() results.
  if (is.null(archive_dir) ||
      length(archive_dir) == 0L ||
      is.na(archive_dir) ||
      !nzchar(archive_dir)) {
    required <- c("extract_dir", "arc_file")
    missing <- setdiff(required, names(extract_i))

    if (length(missing) > 0L) {
      cli::cli_abort(
        "{.arg extract_i} is missing field{?s}: {missing}."
      )
    }

    archive_file <- extract_i$arc_file[[1L]]

    modern_dir <- file.path(
      extract_i$extract_dir,
      tools::file_path_sans_ext(archive_file)
    )

    legacy_dir <- file.path(
      extract_i$extract_dir,
      basename(archive_file)
    )

    archive_dir <- if (dir.exists(modern_dir)) {
      modern_dir
    } else {
      legacy_dir
    }
  }

  fp <- file.path(archive_dir, member)

  if (isTRUE(last_slash)) {
    fp <- sub("[/\\\\]+$", "", fp)
  }

  fp
}