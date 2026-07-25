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