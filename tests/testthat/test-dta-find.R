test_that("dta_find returns regular files", {
  search_dir <- tempfile("dta-find-files-")
  dir.create(search_dir)
  writeLines("value", file.path(search_dir, "observations.csv"))

  expect_equal(
    dta_find(search_dir, "^observations\\.csv$"),
    file.path(search_dir, "observations.csv")
  )
})


test_that("dta_find returns Esri File Geodatabase directories", {
  search_dir <- tempfile("dta-find-gdb-")
  gdb_dir <- file.path(search_dir, "biodiversity.gdb")
  dir.create(gdb_dir, recursive = TRUE)
  writeBin(raw(1L), file.path(gdb_dir, "a00000001.gdbtable"))

  expect_equal(
    dta_find(search_dir, "^biodiversity\\.gdb$"),
    gdb_dir
  )
})


test_that("dta_find returns structurally identifiable ArcInfo Grids", {
  search_dir <- tempfile("dta-find-grid-")
  grid_dir <- file.path(search_dir, "elevation_grid")
  incomplete_dir <- file.path(search_dir, "incomplete_grid")
  dir.create(grid_dir, recursive = TRUE)
  dir.create(incomplete_dir)
  writeBin(raw(1L), file.path(grid_dir, "hdr.adf"))
  writeBin(raw(1L), file.path(grid_dir, "w001001.adf"))
  writeBin(raw(1L), file.path(incomplete_dir, "hdr.adf"))

  expect_equal(
    dta_find(search_dir, "^elevation_grid$"),
    grid_dir
  )

  expect_error(
    dta_find(search_dir, "^incomplete_grid$")
  )
})
