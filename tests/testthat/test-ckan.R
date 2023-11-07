test_that("Test getResources()", {
  expect_equal(
    getResources(fileType = c("csv"), pattern = "victoria"),
    structure(list(repository = c(
      "austarch-a-database-of-14c-and-luminescence-ages-from-archaeological-sites-in-australia", 
      "austarch-a-database-of-14c-and-luminescence-ages-from-archaeological-sites-in-australia"),
      name = c("Austarch 1-3 and IDASQ 28Nov13-1", "Austarch 1-3 and IDASQ 28Nov13-1 Citation\t"), 
      format = c("csv", "csv"), 
      url = c(
        "https://archaeologydataservice.ac.uk/catalogue/adsdata/arch-1661-1/dissemination/csv/Austarch_1-3_and_IDASQ_28Nov13-1.csv", 
        "https://archaeologydataservice.ac.uk/catalogue/adsdata/arch-1661-1/dissemination/csv/Austarch_1-3_and_IDASQ_28Nov13-1_Citation.csv")
    ), class = "data.frame", row.names = c(NA, -2L))
  )
})

test_that("Test getRepositories()", {
  testRepos <- getRepositories(order = FALSE)
  expect_true(
    all(c("isomedita-a-stable-isotope-database-for-medieval-italy", 
          "northern-hemisphere-modern-leaf-wax-ddn-alkane-dataset", 
          "base-de-datos-iber-crono") %in% testRepos$name)
  )
  expect_true(
    all(c("Equine Biometry from Medieval and Modern sites in the Czech Republic", 
          "Tooth Formation Age Dataset for Early Childhood Bioarchaeological and Medical Studies", 
          "Database of equine osteological remains from Greece and Cyprus"
    ) %in% testRepos$title)
  )
  
  testRepos <- getRepositories(pattern = "victor", network = "isomemo", order = FALSE)
  expect_true(
    all(c("zanadamu", 
          "afriarch-isotopic-dataset", 
          "lowland-maya-radiocarbon-dates", 
          "austarch-a-database-of-14c-and-luminescence-ages-from-archaeological-sites-in-australia"
    ) %in% testRepos$name)
  )
  
  expect_true(
    all(c("Zanadamu", "AfriArch isotopic dataset", "MeosRAD v1.4: Lowland Maya Radiocarbon Dates", 
          "AustArch: A Database of 14C and Luminescence Ages from Archaeological Sites in Australia"
    ) %in% testRepos$title)
  )
})

test_that("Test getNetworks()", {
  expect_equal(
    getNetworks(),
    structure(list(name = "isomemo-group", 
                   display_name = "IsoMemo Network", 
                   description = "IsoMemo is a network of autonomous isotopic databases."), 
              class = "data.frame", row.names = 1L)
  )
})

test_that("Test filterPattern()", {
  testRes <- callAPI(action = "current_package_list_with_resources", limit = 1000)
  
  expect_true(nrow(filterPattern(testRes, pattern = "Roman")) < nrow(testRes))
  expect_equal(filterPattern(testRes, pattern = "Roman"),
               filterPattern(testRes, pattern = "rOmAn"))
  expect_true(nrow(filterPattern(testRes, pattern = "cjyvfljdosijvckjnlsfnsdkfnak")) == 0)
})
