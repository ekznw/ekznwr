#' Check TIFF raster files
#'
#' Attempts to read and process each supplied TIFF with [terra::rast()].
#' Files that cannot be read are deleted.
#'
#' @param tifs Character vector of TIFF file paths.
#'
#' @return A logical vector indicating whether each file was read
#'   successfully.
#' @export
check_tifs <- function(
  tifs = NULL
) {
  sapply(tifs, function(f) {
    tryCatch({
      # Try reading raster
      r <- terra::rast(f)
      # force read each raster cell -- if error it will complain
      terra::app(r, fun = function(x) x, cores = 1)
      terra::minmax(r)
      # If successful, return filename (or do further processing)
      TRUE
    }, error = function(e) {
      message("Error reading ", f, ": ", e$message)
      message("Deleting corrupted file: ", f)
      # Delete the file
      unlink(f)
      # Return NULL to keep list clean
      FALSE
    })
  }, USE.NAMES = FALSE)
}

#' Match CRS's
#' 
#' Match the CRS of a target terra object with the CRS of an object with
#' known CRS
#'
#' @param x Terra spatial object.
#' @param target Spatial object with defined target CRS.
#'
#' @return x projected to the CRS of the target. If x or the target have
#' no or undefined CRS's x is returned as is.
#' @export
crs_match_terra <- function(x, target) {
  if (is.na(terra::crs(x)) || !nzchar(terra::crs(x))) {
    stop("An input vector has no CRS; do not assign one without verifying it.")
  }
  if (!terra::same.crs(x, target)) {
    x <- terra::project(x, terra::crs(target))
  }
  x
}