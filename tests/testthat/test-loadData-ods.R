test_that("loadData imports ODS sheets with and without column names", {
  path <- tempfile(fileext = ".ods")
  on.exit(unlink(path))
  first <- data.frame(id = 1:4, value = 11:14, label = letters[1:4],
                      extra = 21:24, last = 31:34)
  second <- data.frame(id = 5:8, value = 15:18)
  readODS::write_ods(list(First = first, Second = second), path)

  expect_equal(as.data.frame(loadData(path, type = "ods", verbose = FALSE)), first)
  expect_equal(
    as.data.frame(loadData(path, type = "ods", sheet = "Second", verbose = FALSE)),
    second
  )
  expect_equal(
    as.data.frame(loadData(path, type = "ods", sheet = 2, verbose = FALSE)),
    second
  )

  readODS::write_ods(first, path, col_names = FALSE)
  loaded <- loadData(path, type = "ods", colNames = FALSE, verbose = FALSE)
  expect_equal(dim(loaded), c(4L, 5L))
  expect_equal(unname(as.list(loaded)), unname(as.list(first)))
})

test_that("ODS row limits count data rows and preserve every column", {
  path <- tempfile(fileext = ".ods")
  on.exit(unlink(path))
  data <- data.frame(id = 1:4, value = 11:14, label = letters[1:4],
                     extra = 21:24, last = 31:34)

  for (colNames in c(TRUE, FALSE)) {
    readODS::write_ods(data, path, col_names = colNames)
    for (nrows in list(2L, 10L, NA_integer_, NULL, 0L, -1L, 1.5)) {
      loaded <- loadData(path, type = "ods", nrows = nrows,
                         colNames = colNames, verbose = FALSE)
      expected <- if (identical(nrows, 2L)) data[1:2, ] else data
      expect_equal(dim(loaded), dim(expected))
      expect_equal(unname(as.list(loaded)), unname(as.list(expected)))
    }
  }
})

test_that("ODS resources are accepted and ODT resources are rejected", {
  resource <- data.frame(name = "table", format = c("odt", "ods", "csv"),
                         url = c("table.odt", "table.ods", "table.csv"))
  filtered <- filterValidFileType(resource, name = "table")
  expect_equal(filtered$format, c("ods", "csv"))
  expect_equal(selectSingleFile(filtered)$format, "ods")
  expect_error(filterValidFileType(resource[1, ], name = "table"),
               "No resource found")
  expect_error(loadData("table.odt", type = "odt", verbose = FALSE),
               "ods")
})
