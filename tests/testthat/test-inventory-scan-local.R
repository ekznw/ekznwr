write_test_arcinfo_grid <- function(root, grid_name = "30mdemwddfin") {
  put_raw <- function(x, at, value) {
    x[at + seq_along(value) - 1L] <- value
    x
  }

  int32_be <- function(x) {
    writeBin(as.integer(x), raw(), endian = "big")
  }

  double_be <- function(x) {
    writeBin(as.double(x), raw(), endian = "big")
  }

  workspace <- file.path(root, "workspace")
  grid_dir <- file.path(workspace, grid_name)
  info_dir <- file.path(workspace, "info")
  dir.create(grid_dir, recursive = TRUE)
  dir.create(info_dir)

  header <- raw(308L)
  header <- put_raw(
    header,
    1L,
    c(charToRaw("GRID1.2"), as.raw(0L))
  )
  header <- put_raw(header, 17L, int32_be(1L))
  header <- put_raw(header, 21L, int32_be(0L))
  header <- put_raw(header, 257L, double_be(1))
  header <- put_raw(header, 265L, double_be(1))
  header <- put_raw(header, 273L, double_be(-128))
  header <- put_raw(header, 281L, double_be(-23))
  header <- put_raw(header, 289L, int32_be(1L))
  header <- put_raw(header, 293L, int32_be(1L))
  header <- put_raw(header, 297L, int32_be(256L))
  header <- put_raw(header, 301L, int32_be(1L))
  header <- put_raw(header, 305L, int32_be(16L))
  writeBin(header, file.path(grid_dir, "hdr.adf"))

  writeBin(
    c(double_be(0), double_be(0), double_be(1), double_be(1)),
    file.path(grid_dir, "dblbnd.adf")
  )
  writeBin(
    c(double_be(1), double_be(1), double_be(1), double_be(0)),
    file.path(grid_dir, "sta.adf")
  )

  file_header <- raw(100L)
  file_header <- put_raw(
    file_header,
    1L,
    as.raw(c(0L, 0L, 0x27L, 0x0aL, 0xffL, 0xffL, 0xfcL, 0x08L))
  )

  grid_data <- c(
    file_header,
    as.raw(c(0L, 2L, 0L, 1L, 1L, 0L))
  )
  grid_data <- put_raw(
    grid_data,
    25L,
    int32_be(length(grid_data) / 2L)
  )
  writeBin(grid_data, file.path(grid_dir, "w001001.adf"))

  grid_index <- c(file_header, int32_be(50L), int32_be(2L))
  grid_index <- put_raw(
    grid_index,
    25L,
    int32_be(length(grid_index) / 2L)
  )
  writeBin(grid_index, file.path(grid_dir, "w001001x.adf"))

  writeLines("<metadata />", file.path(grid_dir, "metadata.xml"))
  writeBin(as.raw(1:4), file.path(info_dir, "arc0002r.001"))
  writeBin(as.raw(5:8), file.path(info_dir, "arc.dir"))

  for (suffix in c(".aux", ".aux.xml", ".ovr", ".rrd")) {
    writeBin(raw(1L), file.path(workspace, paste0(grid_name, suffix)))
  }

  grid_dir
}


test_that("empty directories return a zero-row inventory", {
  scan_dir <- tempfile("empty-inventory-")
  dir.create(scan_dir)

  result <- inventory_scan_local(scan_dir)

  expect_s3_class(
    result,
    "ekznwr_inventory"
  )

  expect_s3_class(
    result,
    "data.table"
  )

  expect_equal(nrow(result), 0L)
  expect_equal(
    attr(result, "scan_status"),
    "empty"
  )

  expect_false(
    "message" %in% names(result)
  )
})


test_that("missing directories produce a structured error", {
  missing_dir <- tempfile("missing-inventory-")

  expect_error(
    inventory_scan_local(missing_dir),
    "does not exist"
  )
})


test_that("GIS styling files are classified as styles", {
  scan_dir <- tempfile("style-inventory-")
  dir.create(scan_dir)

  style_extensions <- c(
    "qml", "qlr", "sld",
    "lyr", "lyrx", "style", "stylx"
  )
  style_paths <- file.path(
    scan_dir,
    paste0("example_", style_extensions, ".", style_extensions)
  )
  file.create(style_paths)

  result <- inventory_scan_local(scan_dir)

  expect_setequal(result$data_file_extension, style_extensions)
  expect_true(all(result$data_file_format == "style"))
  expect_true(all(result$file_role == "file"))
  expect_true(all(result$primary_dataset))
})


test_that("inventory extension categories do not overlap", {
  extensions <- unlist(inventory_ext, use.names = FALSE)

  expect_length(extensions, length(unique(extensions)))
})


test_that("ArcInfo Grid and INFO workspace components are grouped structurally", {
  scan_dir <- tempfile("arcinfo-inventory-")
  dir.create(scan_dir)
  grid_dir <- write_test_arcinfo_grid(scan_dir)

  result <- inventory_scan_local(scan_dir)
  grid_components <- result[
    grepl("^workspace/30mdemwddfin/", path)
  ]
  info_components <- result[
    grepl("^workspace/info/", path)
  ]
  external_sidecars <- result[
    grepl("^workspace/30mdemwddfin\\.(aux|aux\\.xml|ovr|rrd)$", path)
  ]

  expect_equal(
    nrow(result),
    length(list.files(scan_dir, recursive = TRUE))
  )
  expect_true(all(grid_components$dataset_root == grid_dir))
  expect_true(all(
    grid_components$container_type == "esri_arcinfo_grid"
  ))
  expect_equal(
    grid_components[path == "workspace/30mdemwddfin/metadata.xml", file_role],
    "dataset_container_component"
  )

  expect_true(all(
    info_components$container_type == "esri_info_workspace"
  ))
  expect_true(all(info_components$file_role == "container_support"))
  expect_true(all(
    info_components$data_file_format == "container_support"
  ))
  expect_false(any(info_components$primary_dataset))
  expect_true(all(is.na(info_components$dataset_root)))

  expect_true(all(external_sidecars$dataset_root == grid_dir))
  expect_true(all(
    external_sidecars$container_type == "esri_arcinfo_grid"
  ))
  expect_true(all(external_sidecars$file_role == "sidecar"))

  primary <- result[primary_dataset %in% TRUE]
  expect_equal(nrow(primary), 1L)
  expect_equal(primary$logical_file_name, "30mdemwddfin")
  expect_equal(primary$logical_file_path, grid_dir)
  expect_equal(primary$logical_container_type, "esri_arcinfo_grid")
})


test_that("a lone ADF file is not sufficient evidence of a Grid", {
  result <- inventory_classify(data.table::data.table(
    path = "not-a-grid/lone.adf",
    name = "lone.adf"
  ))

  expect_true(is.na(result$dataset_root))
  expect_true(is.na(result$container_type))
  expect_equal(result$data_file_format, "other")
})


test_that("ArcInfo Grid logical path opens directly with terra", {
  skip_if_not_installed("terra")

  scan_dir <- tempfile("arcinfo-terra-")
  dir.create(scan_dir)
  grid_dir <- write_test_arcinfo_grid(scan_dir)
  result <- inventory_scan_local(scan_dir)
  primary <- result[primary_dataset %in% TRUE]

  expect_equal(primary$logical_file_name, "30mdemwddfin")
  expect_equal(primary$logical_file_path, grid_dir)
  expect_s4_class(
    terra::rast(primary$logical_file_path),
    "SpatRaster"
  )
})
