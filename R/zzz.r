.onAttach <- function(libname, ekznwr) {
  packageStartupMessage(
    cat(
      cli::style_blurred(c(cli::col_white("Loading "))),
      cli::style_bold(cli::col_green("ekznwr ")),
      cli::col_white("v0.0.1  "),
      cli::style_blurred(c("|> https://github.com/ekznw/ekznwr\n"))
    )
  )
}
.onLoad <- function(libname, ekznwr) {
  op <- options()
  op.ekznwr <- list(
    extract_dir = file.path(tempdir(), "ekznw_dta")
  )
  toset <- !(names(op.ekznwr) %in% names(op))
  if (any(toset)) options(op.ekznwr[toset])
  invisible()
}