test_that("Can extract table from table character vector", {
  given <- readLines(
    test_path("examples", "table1.txt")
  )
  expected <- data.frame(
    Title = c("first", "second", "third"),
    Thing = c("1", "2", "3"),
    Another = c(
      "This will be a multi-line description.",
      "This is another multi-line description.",
      "And this is slightly jagged."
    )
  )
  expect_identical(
    extract_table(given),
    expected
  )
})
