test_that("dta_fp requires exactly one selector", {
  extract_i <- list(
    arc_file = "example.zip",
    archive_dir = tempdir(),
    arc_file_contents = data.table::data.table(
      Name = c(
        "data/file.tif",
        "docs/readme.txt"
      )
    )
  )

  expect_error(
    dta_fp(extract_i)
  )

  expect_error(
    dta_fp(
      extract_i,
      name = "file",
      row_n = 1L
    )
  )
})


test_that("dta_fp selects archive members by row or name", {
  archive_dir <- tempfile("archive-")
  dir.create(archive_dir)

  extract_i <- list(
    arc_file = "example.zip",
    archive_dir = archive_dir,
    arc_file_contents = data.table::data.table(
      Name = c(
        "data/file.tif",
        "docs/readme.txt"
      )
    )
  )

  expect_equal(
    dta_fp(extract_i, row_n = 1L),
    file.path(
      archive_dir,
      "data/file.tif"
    )
  )

  expect_equal(
    dta_fp(
      extract_i,
      name = "readme\\.txt$"
    ),
    file.path(
      archive_dir,
      "docs/readme.txt"
    )
  )
})


test_that("unsafe archive member paths are rejected", {
  extract_i <- list(
    arc_file = "example.zip",
    archive_dir = tempdir(),
    arc_file_contents = data.table::data.table(
      Name = "../../outside.txt"
    )
  )

  expect_error(
    dta_fp(extract_i, row_n = 1L),
    "unsafe"
  )
})