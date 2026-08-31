#' Convert an ArcGIS Pro layer file (.lyrx) to a QGIS style (.qml)
#'
#' Converts a deliberately limited, well-defined subset of ArcGIS Pro CIM
#' layer symbology to QGIS QML. The initial implementation supports:
#' \itemize{
#'   \item one feature layer;
#'   \item a single-field CIMUniqueValueRenderer;
#'   \item polygon CIMPolygonSymbol symbols;
#'   \item CIMSolidFill and CIMSolidStroke symbol layers;
#'   \item CIMRGBColor, CIMHSVColor, CIMCMYKColor and CIMGrayColor colours.
#' }
#'
#' Unsupported renderer or symbol constructs fail explicitly rather than being
#' silently approximated.
#'
#' @param lyrx Path to an ArcGIS Pro \code{.lyrx} file.
#' @param qml Output path. By default the input extension is replaced by
#'   \code{.qml}.
#' @param layer Layer index in \code{layerDefinitions}; defaults to 1.
#' @param overwrite Logical. Overwrite an existing output file?
#'
#' @return The normalized output path, invisibly.
#'
#' @importFrom jsonlite fromJSON
#' @importFrom xml2 xml_add_child xml_new_root xml_root write_xml
#' @export
lyrx_to_qml <- function(lyrx,
                        qml = sub("\\.lyrx$", ".qml", lyrx, ignore.case = TRUE),
                        layer = 1L,
                        overwrite = FALSE) {
  if (!is.character(lyrx) || length(lyrx) != 1L || !nzchar(lyrx)) {
    stop("`lyrx` must be a single file path.", call. = FALSE)
  }
  if (!file.exists(lyrx)) {
    stop("LYRX file does not exist: ", lyrx, call. = FALSE)
  }
  if (!grepl("\\.lyrx$", lyrx, ignore.case = TRUE)) {
    stop("`lyrx` must have a .lyrx extension.", call. = FALSE)
  }
  if (identical(qml, lyrx)) {
    stop("`qml` must differ from the input path.", call. = FALSE)
  }
  if (file.exists(qml) && !isTRUE(overwrite)) {
    stop("Output exists; use `overwrite = TRUE`: ", qml, call. = FALSE)
  }

  cim <- jsonlite::fromJSON(lyrx, simplifyVector = FALSE)

  defs <- cim$layerDefinitions
  if (is.null(defs) || length(defs) < layer) {
    stop("Requested layer definition does not exist.", call. = FALSE)
  }

  def <- defs[[layer]]

  if (!identical(def$type, "CIMFeatureLayer")) {
    stop(
      "Unsupported layer type: ",
      .ekznwr_chr(def$type, "<missing>"),
      ". Only CIMFeatureLayer is currently supported.",
      call. = FALSE
    )
  }

  renderer <- def$renderer
  if (is.null(renderer)) {
    stop("No renderer found in the selected layer definition.", call. = FALSE)
  }
  if (!identical(renderer$type, "CIMUniqueValueRenderer")) {
    stop(
      "Unsupported renderer: ",
      .ekznwr_chr(renderer$type, "<missing>"),
      ". Current support is limited to CIMUniqueValueRenderer.",
      call. = FALSE
    )
  }

  fields <- unlist(renderer$fields, use.names = FALSE)
  if (length(fields) != 1L) {
    stop(
      "Current conversion supports exactly one unique-value field; found ",
      length(fields), ".",
      call. = FALSE
    )
  }
  field <- as.character(fields[[1]])

  groups <- renderer$groups %||% list()
  classes <- unlist(
    lapply(groups, function(x) x$classes %||% list()),
    recursive = FALSE,
    use.names = FALSE
  )

  if (!length(classes)) {
    stop("The renderer contains no classes.", call. = FALSE)
  }

  # QML is intentionally style-only. No ArcGIS data source is carried across.
  doc <- xml2::xml_new_root(
    "qgis",
    version = "3.44.0",
    styleCategories = "Symbology",
    labelsEnabled = "0"
  )
  root <- xml2::xml_root(doc)

  rnode <- xml2::xml_add_child(
    root,
    "renderer-v2",
    type = "categorizedSymbol",
    attr = field,
    symbollevels = "0",
    enableorderby = "0",
    forceraster = "0",
    referencescale = "-1"
  )

  categories <- xml2::xml_add_child(rnode, "categories")
  symbols <- xml2::xml_add_child(rnode, "symbols")

  for (i in seq_along(classes)) {
    cls <- classes[[i]]

    values <- cls$values %||% list()
    if (length(values) != 1L) {
      stop(
        "Class ", i,
        " contains multiple CIMUniqueValue records; this is not yet supported.",
        call. = FALSE
      )
    }

    field_values <- unlist(values[[1]]$fieldValues, use.names = FALSE)
    if (length(field_values) != 1L) {
      stop(
        "Class ", i,
        " contains ", length(field_values),
        " field values; expected exactly one.",
        call. = FALSE
      )
    }

    value <- as.character(field_values[[1]])
    label <- .ekznwr_chr(cls$label, value)
    visible <- if (is.null(cls$visible)) TRUE else isTRUE(cls$visible)

    xml2::xml_add_child(
      categories,
      "category",
      render = if (visible) "true" else "false",
      type = "string",
      symbol = as.character(i - 1L),
      value = value,
      label = label
    )

    props <- .ekznwr_cim_polygon_symbol(cls$symbol, class_index = i)

    snode <- xml2::xml_add_child(
      symbols,
      "symbol",
      name = as.character(i - 1L),
      type = "fill",
      alpha = "1",
      force_rhr = "0",
      clip_to_extent = "1"
    )

    lnode <- xml2::xml_add_child(
      snode,
      "layer",
      class = "SimpleFill",
      enabled = "1",
      locked = "0",
      pass = "0"
    )

    onode <- xml2::xml_add_child(lnode, "Option", type = "Map")

    opts <- c(
      color = props$fill_color,
      style = "solid",
      outline_color = props$outline_color,
      outline_style = props$outline_style,
      outline_width = format(props$outline_width_mm, scientific = FALSE, trim = TRUE),
      outline_width_unit = "MM",
      joinstyle = props$joinstyle,
      offset = "0,0",
      offset_unit = "MM"
    )

    for (nm in names(opts)) {
      xml2::xml_add_child(
        onode,
        "Option",
        type = "QString",
        name = nm,
        value = unname(opts[[nm]])
      )
    }
  }

  xml2::xml_add_child(rnode, "rotation")
  xml2::xml_add_child(rnode, "sizescale")

  dir.create(dirname(qml), recursive = TRUE, showWarnings = FALSE)
  xml2::write_xml(doc, qml, options = "format")

  invisible(normalizePath(qml, mustWork = TRUE))
}


`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) y else x
}


.ekznwr_chr <- function(x, default = "") {
  if (is.null(x) || !length(x) || is.na(x[[1]])) {
    return(default)
  }
  as.character(x[[1]])
}


.ekznwr_cim_polygon_symbol <- function(symbol_ref, class_index = NA_integer_) {
  symbol <- symbol_ref$symbol

  if (is.null(symbol) || !identical(symbol$type, "CIMPolygonSymbol")) {
    stop(
      "Class ", class_index,
      " does not contain a supported CIMPolygonSymbol.",
      call. = FALSE
    )
  }

  layers <- symbol$symbolLayers %||% list()
  enabled <- Filter(function(x) is.null(x$enable) || isTRUE(x$enable), layers)

  unsupported <- vapply(
    enabled,
    function(x) !.ekznwr_chr(x$type) %in% c("CIMSolidFill", "CIMSolidStroke"),
    logical(1)
  )

  if (any(unsupported)) {
    bad <- unique(vapply(
      enabled[unsupported],
      function(x) .ekznwr_chr(x$type, "<missing>"),
      character(1)
    ))
    stop(
      "Class ", class_index,
      " contains unsupported symbol layer(s): ",
      paste(bad, collapse = ", "),
      call. = FALSE
    )
  }

  fills <- Filter(function(x) identical(x$type, "CIMSolidFill"), enabled)
  strokes <- Filter(function(x) identical(x$type, "CIMSolidStroke"), enabled)

  if (length(fills) > 1L || length(strokes) > 1L) {
    stop(
      "Class ", class_index,
      " contains stacked fills or strokes; this is not yet supported.",
      call. = FALSE
    )
  }

  fill_color <- if (length(fills)) {
    .ekznwr_cim_color_rgba(fills[[1]]$color)
  } else {
    "0,0,0,0"
  }

  if (length(strokes)) {
    stroke <- strokes[[1]]
    outline_color <- .ekznwr_cim_color_rgba(stroke$color)
    outline_style <- "solid"

    # ArcGIS CIM symbol dimensions are conventionally expressed in points.
    # QGIS SimpleFill stores outline width here in millimetres.
    width_pt <- as.numeric(stroke$width %||% 0)
    outline_width_mm <- width_pt * 25.4 / 72

    joinstyle <- switch(
      tolower(.ekznwr_chr(stroke$joinStyle, "round")),
      round = "round",
      miter = "miter",
      bevel = "bevel",
      "round"
    )
  } else {
    outline_color <- "0,0,0,0"
    outline_style <- "no"
    outline_width_mm <- 0
    joinstyle <- "bevel"
  }

  list(
    fill_color = fill_color,
    outline_color = outline_color,
    outline_style = outline_style,
    outline_width_mm = outline_width_mm,
    joinstyle = joinstyle
  )
}


.ekznwr_cim_color_rgba <- function(color) {
  if (is.null(color)) {
    return("0,0,0,0")
  }

  type <- .ekznwr_chr(color$type)
  values <- as.numeric(unlist(color$values, use.names = FALSE))

  clamp <- function(x, lo, hi) pmin(hi, pmax(lo, x))
  alpha255 <- function(a100) round(clamp(a100, 0, 100) * 255 / 100)

  rgba <- switch(
    type,

    CIMRGBColor = {
      if (length(values) < 3L) {
        stop("Invalid CIMRGBColor.", call. = FALSE)
      }
      c(
        round(clamp(values[1:3], 0, 255)),
        alpha255(if (length(values) >= 4L) values[4] else 100)
      )
    },

    CIMHSVColor = {
      if (length(values) < 3L) {
        stop("Invalid CIMHSVColor.", call. = FALSE)
      }
      h <- (values[1] %% 360) / 360
      s <- clamp(values[2], 0, 100) / 100
      v <- clamp(values[3], 0, 100) / 100
      a <- clamp(if (length(values) >= 4L) values[4] else 100, 0, 100) / 100

      x <- grDevices::col2rgb(
        grDevices::hsv(h = h, s = s, v = v, alpha = a),
        alpha = TRUE
      )
      as.numeric(x[, 1])
    },

    CIMCMYKColor = {
      if (length(values) < 4L) {
        stop("Invalid CIMCMYKColor.", call. = FALSE)
      }
      cmyk <- clamp(values[1:4], 0, 100) / 100
      cc <- cmyk[1]
      mm <- cmyk[2]
      yy <- cmyk[3]
      kk <- cmyk[4]

      c(
        round(255 * (1 - cc) * (1 - kk)),
        round(255 * (1 - mm) * (1 - kk)),
        round(255 * (1 - yy) * (1 - kk)),
        alpha255(if (length(values) >= 5L) values[5] else 100)
      )
    },

    CIMGrayColor = {
      if (length(values) < 1L) {
        stop("Invalid CIMGrayColor.", call. = FALSE)
      }
      gray <- round(255 * clamp(values[1], 0, 100) / 100)
      c(
        gray, gray, gray,
        alpha255(if (length(values) >= 2L) values[2] else 100)
      )
    },

    stop("Unsupported CIM colour type: ", type, call. = FALSE)
  )

  paste(as.integer(round(rgba)), collapse = ",")
}
