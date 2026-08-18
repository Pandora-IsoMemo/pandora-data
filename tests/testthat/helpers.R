passOnErrorMsg <- function(expr) {
  res <- try({
    expr
  })
  
  if (!inherits(res, "try-error")) return(res) else return(res[[1]])
}

isTransientFailureMessage <- function(err) {
  transientPattern <- "HTTP 429|HTTP 5[0-9]{2}|timed out|timeout|could not resolve host|failed to connect|connection|SSL|temporarily unavailable|received HTML"
  grepl(transientPattern, err, ignore.case = TRUE)
}

skipIfTransientApiFailure <- function(obj, context) {
  if (inherits(obj, "try-error")) {
    err <- as.character(obj[[1]])
    if (isTransientFailureMessage(err)) {
      testthat::skip(paste("Skipping", context, "because API/source is temporarily unavailable:", err))
    }
    testthat::fail(paste(context, "failed unexpectedly:", err))
  }

  errAttr <- attr(obj, "error")
  if (is.null(errAttr)) errAttr <- attr(obj, "errorApi")

  if (!is.null(errAttr)) {
    err <- as.character(errAttr)
    if (isTransientFailureMessage(err)) {
      testthat::skip(paste("Skipping", context, "because API/source is temporarily unavailable:", err))
    }
    testthat::fail(paste(context, "failed unexpectedly:", err))
  }

  invisible(obj)
}

expectNonTransientError <- function(expr, context) {
  res <- try({
    expr
  }, silent = TRUE)

  if (inherits(res, "try-error")) {
    err <- as.character(res[[1]])
    if (isTransientFailureMessage(err)) {
      testthat::skip(paste("Skipping", context, "because API/source is temporarily unavailable:", err))
    }
  }

  testthat::expect_true(inherits(res, "try-error"))
  invisible(res)
}

foundResource <- function(testLoaded) {
  !(inherits(testLoaded, "character") && grepl(pattern = "No resource found", testLoaded))
}
