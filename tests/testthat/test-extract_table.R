test_that("Can extract table from table character vector", {
  given <- readLines(
    test_path("examples", "table1.txt")
  )
  expected <- data.frame(
    Title = c("first", "second", "third"),
    Thing = c("1", "2", "3"),
    Another = c(
      "This will be a multi-line description.",
      "This is another multi-line description with three lines.",
      "And this is slightly jagged."
    )
  )
  expect_identical(
    extract_table(given),
    expected
  )
})

test_that("Can extract table from listy character vector", {
  given <- readLines(
    test_path("examples", "table2.txt")
  )
  expected <- data.frame(
    Year = "2022",
    Info = "This information is table like but it isn't really a table nor is it delimited per se.",
    Name = "Jon Harmon"
  )
  expect_identical(
    extract_table(given, orientation = "vertical"),
    expected
  )
})

test_that("Not confused by weird breaks", {
  given <- readLines(
    test_path("examples", "table2b.txt")
  )
  expected <- data.frame(
    Year = "2022",
    Info = "This information is table like but it isn't really a table nor is it delimited per se.",
    Name = "Jon Harmon"
  )
  expect_identical(
    extract_table(given, orientation = "vertical"),
    expected
  )
})

test_that("Works without leading spaces", {
  given <- readLines(
    test_path("examples", "table2c.txt")
  )
  expected <- data.frame(
    Year = "2022",
    Info = "This information is table like but it isn't really a table nor is it delimited per se.",
    Name = "Jon Harmon"
  )
  expect_identical(
    extract_table(given, orientation = "vertical"),
    expected
  )
})
