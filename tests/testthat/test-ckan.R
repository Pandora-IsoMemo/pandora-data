skipIfTransientApiFailure <- function(obj, context) {
  if (inherits(obj, "try-error")) {
    err <- as.character(obj[[1]])
    if (grepl("HTTP 429|HTTP 5[0-9]{2}|timed out|timeout|could not resolve host|failed to connect|connection|SSL|temporarily unavailable|received HTML", err, ignore.case = TRUE)) {
      testthat::skip(paste("Skipping", context, "because API/source is temporarily unavailable:", err))
    }
    testthat::fail(paste(context, "failed unexpectedly:", err))
  }

  errAttr <- attr(obj, "error")
  if (is.null(errAttr)) errAttr <- attr(obj, "errorApi")

  if (!is.null(errAttr)) {
    err <- as.character(errAttr)
    if (grepl("HTTP 429|HTTP 5[0-9]{2}|timed out|timeout|could not resolve host|failed to connect|connection|SSL|temporarily unavailable|received HTML", err, ignore.case = TRUE)) {
      testthat::skip(paste("Skipping", context, "because API/source is temporarily unavailable:", err))
    }
    testthat::fail(paste(context, "failed unexpectedly:", err))
  }

  invisible(obj)
}

test_that("Test getResources()", {
  testthat::skip_if_offline()

  testResNone <- try(getResources(repository = "aghfjdhfjgkhj"), silent = TRUE)
  skipIfTransientApiFailure(testResNone, "getResources()")
  expect_true(nrow(testResNone) == 0)

  testRes <- try(getResources(fileType = c("csv"), pattern = "victoria"), silent = TRUE)
  skipIfTransientApiFailure(testRes, "getResources()")
  
  expect_equal(
    testRes,
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

test_that("Test getFileTypes()", {
  testthat::skip_if_offline()

  testTypesNone <- try(getFileTypes(repository = "aghfjdhfjgkhj"), silent = TRUE)
  skipIfTransientApiFailure(testTypesNone, "getFileTypes()")
  expect_true(nrow(testTypesNone) == 0)

  testTypes <- try(getFileTypes(pattern = "victoria"), silent = TRUE)
  skipIfTransientApiFailure(testTypes, "getFileTypes()")
  
  expect_equal(
    testTypes,
    structure(list(
      name = "austarch-a-database-of-14c-and-luminescence-ages-from-archaeological-sites-in-australia", 
      format = "csv"), 
      row.names = 1L, class = "data.frame")
  )
})

test_that("Test getRepositories()", {
  testthat::skip_if_offline()

  testReposNone <- try(getRepositories(network = "aghfjdhfjgkhj"), silent = TRUE)
  skipIfTransientApiFailure(testReposNone, "getRepositories()")
  expect_true(nrow(testReposNone) == 0)
  
  testRepos <- try(getRepositories(order = FALSE, renameColumns = FALSE), silent = TRUE)
  skipIfTransientApiFailure(testRepos, "getRepositories()")
  expect_equal(colnames(testRepos),
               c("title", "name", "notes", "ext_doi", "doi", "version", "author", 
                 "author_email", "maintainer", "maintainer_email", "temporal_start", 
                 "temporal_end", "spatial")
               )
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
  
  testRepos <- try(getRepositories(pattern = "victor", network = "isomemo", order = FALSE), silent = TRUE)
  skipIfTransientApiFailure(testRepos, "getRepositories()")
  expect_equal(
    "austarch-a-database-of-14c-and-luminescence-ages-from-archaeological-sites-in-australia",
    testRepos$Name
  )
  
  expect_equal(
    "AustArch: A Database of 14C and Luminescence Ages from Archaeological Sites in Australia",
    testRepos$Repository
  )
  
  testRepos <- try(getRepositories(order = FALSE, renameColumns = TRUE), silent = TRUE)
  skipIfTransientApiFailure(testRepos, "getRepositories()")
  expect_equal(colnames(testRepos), 
               c("Repository", "Name", "Description", "Existing DOI", "Assigned DOI", 
                 "Version", "Author", "Author Email", "Maintainer", "Maintainer Email", 
                 "Chronological range (min)", "Chronological range (max)", "Spatial Box"
               ))
})

test_that("Test getNetworks()", {
  testthat::skip_if_offline()

  testNetsNone <- try(getNetworks(pattern = "aghfjdhfjgkhj"), silent = TRUE)
  skipIfTransientApiFailure(testNetsNone, "getNetworks()")
  expect_true(nrow(testNetsNone) == 0)

  testNets <- try(getNetworks(), silent = TRUE)
  skipIfTransientApiFailure(testNets, "getNetworks()")
  
  expect_equal(
    testNets,
    structure(list(name = "isomemo-group", 
                   display_name = "IsoMemo Network", 
                   description = "IsoMemo is a network of autonomous isotopic databases."), 
              class = "data.frame", row.names = 1L)
  )
})

test_that("Test filterPattern()", {
  testthat::skip_if_offline()

  testRes <- try(callAPI(action = "current_package_list_with_resources", limit = 1000), silent = TRUE)
  skipIfTransientApiFailure(testRes, "callAPI()")
  
  expect_true(nrow(filterPattern(testRes, pattern = "Roman")) < nrow(testRes))
  expect_equal(filterPattern(testRes, pattern = "Roman"),
               filterPattern(testRes, pattern = "rOmAn"))
  expect_true(nrow(filterPattern(testRes, pattern = "cjyvfljdosijvckjnlsfnsdkfnak")) == 0)
})
