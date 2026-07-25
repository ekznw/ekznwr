
.onAttach <- function(libname, pkgname) {

  if (!interactive()) return()

  desc <- utils::packageDescription(pkgname)

  packageStartupMessage(
    desc$Package, " v", desc$Version,
    " - ", desc$Title,
    "\n", desc$URL
  )
}

.onLoad <- function(libname, pkgname) {
  defaults <- list(
    ekznwr.extract_dir = file.path(tempdir(), "ekznw_dta"),
    ekznwr.max_map_age = 60 * 60 * 24
  )

  current <- options()

  to_set <- !names(defaults) %in% names(current)

  if (any(to_set)) {
    options(defaults[to_set])
  }

  invisible()
}
