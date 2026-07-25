test_that("classification and enrichment preserve rows and order", {
  fixture <- make_inventory_fixture()

  result <- fixture |>
    inventory_classify() |>
    inventory_enrich_groups()

  expect_equal(nrow(result), nrow(fixture))
  expect_equal(result$path, fixture$path)
  expect_true("logical_dir" %in% names(result))
})


test_that("logical groups contain at most one primary row", {
  result <- make_inventory_fixture() |>
    inventory_classify() |>
    inventory_enrich_groups()

  primary_counts <- result[
    primary_dataset %in% TRUE,
    .N,
    by = group_key
  ]

  expect_true(all(primary_counts$N == 1L))
})


test_that("sidecars and archives are not logical primary rows", {
  result <- make_inventory_fixture() |>
    inventory_classify() |>
    inventory_enrich_groups()

  expect_false(any(
    result$file_role == "sidecar" &
      result$primary_dataset,
    na.rm = TRUE
  ))

  expect_false(any(
    result$data_file_format == "archive" &
      result$primary_dataset,
    na.rm = TRUE
  ))
})


test_that("shapefile components share one group", {
  result <- make_inventory_fixture() |>
    inventory_classify() |>
    inventory_enrich_groups()

  roads_a <- result[
    mount == "/mnt/a" &
      grepl("^roads/roads", path)
  ]

  expect_length(
    unique(roads_a$group_key),
    1L
  )

  expect_equal(
    sum(roads_a$primary_dataset),
    1L
  )

  expect_true(all(
    roads_a[
      data_file_extension != "shp",
      file_role
    ] == "sidecar"
  ))
})


test_that("mounts prevent cross-source grouping", {
  result <- make_inventory_fixture() |>
    inventory_classify() |>
    inventory_enrich_groups()

  group_a <- unique(result[
    mount == "/mnt/a" &
      path == "roads/roads.shp",
    group_key
  ])

  group_b <- unique(result[
    mount == "/mnt/b" &
      path == "roads/roads.shp",
    group_key
  ])

  expect_false(identical(group_a, group_b))
})


test_that("ArcInfo grid components and overview share a group", {
  result <- make_inventory_fixture() |>
    inventory_classify() |>
    inventory_enrich_groups()

  grid <- result[
    grepl("terrain/grid", path)
  ]

  expect_length(
    unique(grid$group_key),
    1L
  )

  expect_true(all(
    grid$logical_extension == "adf"
  ))
})


test_that("enrichment is idempotent", {
  first <- make_inventory_fixture() |>
    inventory_classify() |>
    inventory_enrich_groups()

  second <- inventory_enrich_groups(first)

  expect_equal(
    second$group_key,
    first$group_key
  )

  expect_equal(
    second$primary_dataset,
    first$primary_dataset
  )

  expect_false(any(
    grepl("\\.[xy]$", names(second))
  ))
})