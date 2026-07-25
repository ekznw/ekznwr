test_that("safe archive member paths are accepted", {
  expect_true(all(
    ekznwr:::archive_member_is_safe(c(
      "file.txt",
      "folder/file.tif",
      "folder/nested/data.csv"
    ))
  ))
})


test_that("archive traversal paths are rejected", {
  expect_false(any(
    ekznwr:::archive_member_is_safe(c(
      "../file.txt",
      "folder/../../file.txt",
      "/absolute/file.txt",
      "C:/absolute/file.txt"
    ))
  ))
})