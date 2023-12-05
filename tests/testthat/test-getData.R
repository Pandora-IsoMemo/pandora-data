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
