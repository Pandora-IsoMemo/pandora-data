test_that("Test getData() from xlsx", {
  testLoaded <-
    try(getData(name = "14CARHU - Radiocarbon Dates of Helsinki University"),
        silent = TRUE)
  skipIfTransientApiFailure(testLoaded, "getData() from xlsx")
  
  if (foundResource(testLoaded)) {
    expect_true(nrow(testLoaded) > 2000)
    expect_true(all(
      c(
        "14CARHU.dates.under.the.OASIS.database.v1.0.(21/10/2015)",
        "X2",
        "X3",
        "X4",
        "X5"
      )
      %in% colnames(testLoaded)
    ))
  }
})

test_that("Test getData() from csv if isOldROnWindows() machine", {
  # following tests show encryption issues with Windows
  if (isOldROnWindows()) {
    # error on old windows
    expectNonTransientError(
      getData(name = "Isotopic measurements in CSV format"),
      "getData() from csv on old Windows"
    )
    
    # error without specific encoding
    expectNonTransientError(
      getData(name = "MAIA Humans CSV",
              options = dataOptions(sep = ";")),
      "getData() from csv without encoding on old Windows"
    )
    
    # no error with windows encoding
    testLoaded <- try(
      getData(name = "MAIA Humans CSV",
              options = dataOptions(sep = ";",
                                    fileEncoding = "windows-1252")),
      silent = TRUE
    )
    skipIfTransientApiFailure(testLoaded, "getData() from csv with Windows encoding")
    if (foundResource(testLoaded)) expect_true(nrow(testLoaded) > 2000)
    
    # less rows on old windows
    testLoaded <- try(
      getData(name = "IsoMedIta Humans 21-12-22 - CSV",
              options = dataOptions(sep = ";")),
      silent = TRUE
    )
    skipIfTransientApiFailure(testLoaded, "getData() from IsoMedIta csv on old Windows")
    if (foundResource(testLoaded)) expect_true(nrow(testLoaded) < 2000)
  }
})

test_that("Test getData() from csv if newer Windows machine", { 
  if (!isOldROnWindows() && Sys.info()["sysname"] == "Windows") {
    # no error without specific encoding
    testLoaded <- try(getData(name = "Isotopic measurements in CSV format"),
                      silent = TRUE)
    skipIfTransientApiFailure(testLoaded, "getData() from csv on newer Windows")
    if (foundResource(testLoaded)) expect_true(nrow(testLoaded) > 2000)
    
    testLoaded <- try(
      getData(name = "MAIA Humans CSV",
              options = dataOptions(sep = ";")),
      silent = TRUE
    )
    skipIfTransientApiFailure(testLoaded, "getData() from MAIA csv on newer Windows")
    if (foundResource(testLoaded)) expect_true(nrow(testLoaded) > 2000)
    
    # no error with windows encoding
    testLoaded <- try(
      getData(name = "MAIA Humans CSV",
              options = dataOptions(sep = ";",
                                    fileEncoding = "windows-1252")),
      silent = TRUE
    )
    skipIfTransientApiFailure(testLoaded, "getData() from MAIA csv with Windows encoding")
    if (foundResource(testLoaded)) expect_true(nrow(testLoaded) > 2000)
    
    # more rows if newer windows
    testLoaded <- try(
      getData(name = "IsoMedIta Humans 21-12-22 - CSV",
              options = dataOptions(sep = ";")),
      silent = TRUE
    )
    skipIfTransientApiFailure(testLoaded, "getData() from IsoMedIta csv on newer Windows")
    if (foundResource(testLoaded)) expect_true(nrow(testLoaded) > 2000)
  }
  })

test_that("Test getData() from csv if linux or mac", {
  if (!Sys.info()["sysname"] == "Windows") { # linux or mac
    # no error without specific encoding
    testLoaded <- try(
      getData(name = "MAIA Humans CSV",
              options = dataOptions(sep = ";")),
      silent = TRUE
    )
    skipIfTransientApiFailure(testLoaded, "getData() from MAIA csv on Linux or macOS")
    if (foundResource(testLoaded)) expect_true(nrow(testLoaded) > 2000)
    
    # less data with windows encoding
    testLoaded <-
      try(getData(name = "MAIA Humans CSV",
                  options = dataOptions(sep = ";",
                                        fileEncoding = "windows-1252")),
          silent = TRUE)
    skipIfTransientApiFailure(testLoaded, "getData() from MAIA csv with Windows encoding on Linux or macOS")
    
    if (foundResource(testLoaded)) {
      expect_true(nrow(testLoaded) > 700)
      expect_true(nrow(testLoaded) < 800)
    }
  }
})

test_that("Test getData() from csv", {
  testLoaded <- try(
    getData(name = "CIMA Animals 29.05.2021 CSV",
            options = dataOptions(sep = ";")),
    silent = TRUE
  )
  skipIfTransientApiFailure(testLoaded, "getData() from CIMA animals csv")
  if (foundResource(testLoaded)) expect_true(nrow(testLoaded) > 4000)
  
  testLoaded <- try(
    getData(name = "CIMA Plants 29.05.2021 CSV",
            options = dataOptions(sep = ";")),
    silent = TRUE
  )
  skipIfTransientApiFailure(testLoaded, "getData() from CIMA plants csv")
  if (foundResource(testLoaded)) expect_true(nrow(testLoaded) > 100)
  
  
  testLoaded <- try(
    getData(name = "Zanadamu CSV format",
            options = dataOptions(fileEncoding = "ISO-8859-1")),
    silent = TRUE
  )
  skipIfTransientApiFailure(testLoaded, "getData() from Zanadamu csv")
  if (foundResource(testLoaded)) expect_true(nrow(testLoaded) > 200)
  
  # run only for TDD:
  # test random files to check if errors are caught
  # allResources <- getResources()
  # for (i in 1:10) {
  #   testResource <- allResources[sample(nrow(allResources), 1), ]
  #   getData(name = testResource[["name"]])
  # }
  
  expectNonTransientError(
    getData(name = "Amalthea Bibliography 05.03.2021"),
    "getData() from Amalthea Bibliography"
  )
  expectNonTransientError(
    getData(name = "Isotòpia Humans csv (19.09.2023)"),
    "getData() from Isotopia Humans csv"
  )
})

test_that("Test loadData()", {
  testthat::skip_if_offline()

  testResource <-
    try(getResources(fileType = "xlsx",
                     network = "IsoMemo",
                     pattern = "14carhu"),
        silent = TRUE)
  skipIfTransientApiFailure(testResource, "getResources() for loadData() test")
  if (nrow(testResource) == 0) {
    testthat::skip("Skipping loadData() test because no matching remote resource was found.")
  }

  testLoaded <-
    try(loadData(path = testResource[1, "url"], type = testResource[1, "format"]), silent = TRUE)
  skipIfTransientApiFailure(testLoaded, "loadData()")

  expect_true(nrow(testLoaded) > 2000)
  expect_true(all(
    c(
      "14CARHU.dates.under.the.OASIS.database.v1.0.(21/10/2015)",
      "X2",
      "X3",
      "X4",
      "X5"
    )
    %in% colnames(testLoaded)
  ))
})

test_that("Test loadText()", {
  textResource <- "https://pandoradata.earth/dataset/46fe7fc7-55a4-493d-91e8-c9abffbabcca/resource/f4b0a2b4-8f65-463d-aff4-2a31490abc78/download/oxcal_basic_code.txt"

  testLoaded <- try(loadText(path = textResource), silent = TRUE)
  if (inherits(testLoaded, "try-error")) {
    testthat::skip(paste("Skipping text test because the source could not be read:", testLoaded))
  }

  expect_true(is.character(testLoaded))
  expect_true(length(testLoaded) > 0)
  expect_true(any(nchar(testLoaded) > 0))

  testLoadedCollapsed <- try(loadText(path = textResource, collapse = TRUE),
                             silent = TRUE)
  skipIfTransientApiFailure(testLoadedCollapsed, "loadText() collapsed")

  expect_true(is.character(testLoadedCollapsed))
  expect_equal(length(testLoadedCollapsed), 1)
  expect_true(nchar(testLoadedCollapsed) > 0)
})
