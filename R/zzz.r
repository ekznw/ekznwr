
.onAttach <- function(libname, pkgname) {

  if (!interactive()) return()

  desc <- utils::packageDescription(pkgname)

  packageStartupMessage(
    desc$Package, " v", desc$Version,
    " — ", desc$Title,
    "\n", desc$URL
  )
}

.onLoad <- function(libname, ekznwr) {
  op <- options()
  op.ekznwr <- list(
    extract_dir = file.path(tempdir(), "ekznw_dta"),
    max_map_age = 60 * 60 * 24
  )
  toset <- !(names(op.ekznwr) %in% names(op))
  if (any(toset)) options(op.ekznwr[toset])
  invisible()
}