test_that("Test getData()", {
  testLoaded <-
    getData(name = "14CARHU - Radiocarbon Dates of Helsinki University")
  
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
  
  # following tests show encryption issues with Windows
  if (isOldROnWindows()) {
    # error without specific encoding
    expect_error(getData(name = "MAIA Humans CSV",
                         options = dataOptions(sep = ";")))
    
    # no error with windows encoding
    expect_true(nrow(getData(name = "MAIA Humans CSV",
                             options = dataOptions(sep = ";",
                                                   fileEncoding = "windows-1252"))) > 2000)
  } else if (Sys.info()["sysname"] == "Windows") {
    # no error without specific encoding
    expect_true(nrow(getData(name = "MAIA Humans CSV",
                             options = dataOptions(sep = ";"))) > 2000)
    
    # no error with windows encoding
    expect_true(nrow(getData(name = "MAIA Humans CSV",
                             options = dataOptions(sep = ";",
                                                   fileEncoding = "windows-1252"))) > 2000)
  } else { # linux or mac
    # no error without specific encoding
    expect_true(nrow(getData(name = "MAIA Humans CSV",
                             options = dataOptions(sep = ";"))) > 2000)
    
    # less data with windows encoding
    testLoaded <-
      getData(name = "MAIA Humans CSV",
              options = dataOptions(sep = ";",
                                    fileEncoding = "windows-1252"))
    
    expect_true(nrow(testLoaded) > 700)
    expect_true(nrow(testLoaded) < 800)
  }
  
  expect_true(nrow(getData(name = "CIMA Animals 29.05.2021 CSV",
                           options = dataOptions(sep = ";"))) > 4000)
  
  expect_true(nrow(getData(name = "CIMA Plants 29.05.2021 CSV",
                           options = dataOptions(sep = ";"))) > 100)
  
  expect_true(nrow(getData(name = "SAAID_V.2.0_2023 Animals (CSV)")) > 2000)
  
  expect_true(nrow(getData(name = "Zanadamu CSV format",
                           options = dataOptions(fileEncoding = "ISO-8859-1"))) > 200)
  
  if (isOldROnWindows()) {
    expect_error(getData(name = "Isotopic measurements in CSV format"))
  } else {
    expect_true(nrow(getData(name = "Isotopic measurements in CSV format")) > 2000)
  }
  
  if (isOldROnWindows()) {
    expect_true(nrow(getData(name = "IsoMedIta Humans 21-12-22 - CSV",
                             options = dataOptions(sep = ";"))) < 2000)
  } else {
    expect_true(nrow(getData(name = "IsoMedIta Humans 21-12-22 - CSV",
                             options = dataOptions(sep = ";"))) > 2000)
  }
  
  # run only for TDD:
  # test random files to check if errors are caught
  # allResources <- getResources()
  # for (i in 1:10) {
  #   testResource <- allResources[sample(nrow(allResources), 1), ]
  #   getData(name = testResource[["name"]])
  # }
  
  expect_error(getData(name = "Amalthea Bibliography 05.03.2021"))
  expect_error(getData(name = "Isotòpia Humans csv (19.09.2023)"))
})

test_that("Test loadData()", {
  testResource <-
    getResources(fileType = "xlsx",
                 network = "IsoMemo",
                 pattern = "14carhu")
  testLoaded <-
    loadData(path = testResource[1, "url"], type = testResource[1, "format"])
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
