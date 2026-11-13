test_that("loadData detects XLSX contents despite an XLS filename", {
  path <- tempfile(fileext = ".xls")
  on.exit(unlink(path))
  first <- data.frame(id = 1:4, value = 11:14, label = letters[1:4])
  second <- data.frame(id = 5:8, value = 15:18)
  readPath <- tempfile(fileext = ".xlsx")
  on.exit(unlink(readPath), add = TRUE)
  openxlsx::write.xlsx(list(First = first, Second = second), readPath)
  expect_true(file.copy(readPath, path))

  for (type in c("xls", "xlsx")) {
    expect_equal(loadData(path, type = type, verbose = FALSE), first)
    expect_equal(loadData(path, type = type, sheet = "Second", verbose = FALSE),
                 second)
    expect_equal(loadData(path, type = type, sheet = 2, verbose = FALSE), second)
    expect_equal(loadData(path, type = type, nrows = 3, verbose = FALSE),
                 first[1:2, ])
    expect_equal(loadData(path, type = type, colNames = FALSE, verbose = FALSE),
                 openxlsx::read.xlsx(readPath, sheet = 1, colNames = FALSE))
    expect_error(loadData(path, type = type, sheet = "Missing", verbose = FALSE),
                 'Cannot find sheet named "Missing"', fixed = TRUE)
  }
})

test_that("loadData preserves native Excel readers and detects mislabeled XLS", {
  xlsx <- tempfile(fileext = ".xlsx")
  xls <- tempfile(fileext = ".xlsx")
  on.exit(unlink(c(xlsx, xls)))
  data <- data.frame(id = 1:4, value = 11:14)
  openxlsx::write.xlsx(data, xlsx)
  expect_equal(loadData(xlsx, type = "xlsx", verbose = FALSE), data)

  nativeXls <- readxl::readxl_example("datasets.xls")
  expected <- readxl::read_xls(nativeXls, n_max = 3)
  expect_true(file.copy(nativeXls, xls))
  for (path in c(nativeXls, xls)) {
    for (type in c("xls", "xlsx")) {
      expect_equal(loadData(path, type = type, nrows = 3, verbose = FALSE),
                   expected)
    }
  }
})

test_that("loadData rejects unknown Excel contents and surfaces parsing errors", {
  path <- tempfile(fileext = ".xls")
  on.exit(unlink(path))
  writeLines("This is not an Excel workbook.", path)
  for (type in c("xls", "xlsx")) {
    expect_error(loadData(path, type = type, verbose = FALSE),
                 "Could not determine Excel format")
  }

  writeBin(as.raw(c(0xD0, 0xCF, 0x11, 0xE0, 0xA1, 0xB1, 0x1A, 0xE1)), path)
  for (type in c("xls", "xlsx")) {
    expect_error(loadData(path, type = type, verbose = FALSE), "libxls")
  }
})
