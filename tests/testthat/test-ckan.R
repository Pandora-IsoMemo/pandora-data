test_that("Test getResourceList()", {
  expect_equal(
    getResourceList(fileType = c("csv"), pattern = "victoria"),
    list(
      `Select Pandora resource ...` = "",
      `AustArch: A Database of 14C and Luminescence Ages from Archaeological Sites in Australia` = c(
        `Austarch 1-3 and IDASQ 28Nov13-1 (CSV)` = 
          "https://archaeologydataservice.ac.uk/catalogue/adsdata/arch-1661-1/dissemination/csv/Austarch_1-3_and_IDASQ_28Nov13-1.csv", 
        `Austarch 1-3 and IDASQ 28Nov13-1 Citation (CSV)` = 
          "https://archaeologydataservice.ac.uk/catalogue/adsdata/arch-1661-1/dissemination/csv/Austarch_1-3_and_IDASQ_28Nov13-1_Citation.csv"
      ))
  )
})

test_that("Test getRepositoryList()", {
  expect_true(
    all(c(
      `Select Pandora repository ...` = "",
      `Vitis vinifera seeds in Eastern Mediterranean (up to the 7th c. CE)` = "vitis-vinifera-seeds-in-eastern-mediterranean-up-to-the-7th-c-ce",
      Zanadamu = "zanadamu",
      `AfriArch isotopic dataset` = "afriarch-isotopic-dataset"
    ) %in% getRepositoryList(sort = FALSE))
  )
  
  expect_equal(
    c(
      `Select Pandora repository ...` = "", 
      `AustArch: A Database of 14C and Luminescence Ages from Archaeological Sites in Australia` = "austarch-a-database-of-14c-and-luminescence-ages-from-archaeological-sites-in-australia"
    ),
    getRepositoryList(pattern = "victor", network = "isomemo", sort = FALSE)
  )
})

test_that("Test getNetworkList()", {
  expect_equal(
    getNetworkList(sort = TRUE),
    c(`IsoMemo Network` = "isomemo-group")
  )
})

test_that("Test filterPattern()", {
  testFiles <- getCKANFiles()
  
  expect_true(length(filterPattern(testFiles, pattern = "Roman")) < length(testFiles))
  expect_equal(filterPattern(testFiles, pattern = "Roman"),
               filterPattern(testFiles, pattern = "rOmAn"))
  expect_length(filterPattern(testFiles, pattern = "cjyvfljdosijvckjnlsfnsdkfnak"), 0)
})

test_that("Test filterCKANGroup()", {
  testFiles <- getCKANFiles() %>%
    filterCKANFileList()
  
  expect_true(length(filterCKANGroup(testFiles, ckanGroup = "isomemo-group")) < length(testFiles))
  expect_equal(names(filterCKANGroup(testFiles, ckanGroup = "isomemo-group")[[1]]),
               c("title", "resources", "groups"))
})
