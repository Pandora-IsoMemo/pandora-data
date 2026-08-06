#' Get Data
#'
#' @param name (character) name of a resource, e.g. an entry of the output from
#'  \code{getResources()$name}
#' @param verbose Logical, indicating whether to display processing messages.
#'   If TRUE, messages will be displayed; if FALSE, messages will be suppressed.
#'   Default is TRUE.
#' @param options (list) a list of extra options for \code{read.csv()} or \code{openxlsx::read.xlsx()} and
#'  \code{readxl::read_excel}
#' @inheritParams getResources
#'
#' @return (data.frame) return data from the Pandora API
#' @export
getData <- function(name,
                    repository = "",
                    verbose = TRUE,
                    options = dataOptions()) {
  resource <- try({
    getResources(repository = repository) %>%
      validateResource(repository = repository) %>%
      filterResourceByName(name = name) %>%
      filterValidFileType(name = name) %>%
      selectSingleFile()
  }, silent = TRUE)

  data <- try({
    loadData(
      path = resource[["url"]],
      type = resource[["format"]],
      nrows = options$nrows,
      sep = options$text$sep,
      dec = options$text$dec,
      fileEncoding = options$text$fileEncoding,
      colNames = options$colNames,
      sheet = options$xlsx$sheet,
      verbose = verbose
    )
  }, silent = TRUE)
  
  # catch possible errors directly inside this function
  res <- list()
  
  if (inherits(resource, "try-error")) {
    stop(resource[[1]])
  } else if (length(resource) == 0 && nrow(resource) == 0) {
    stop(sprintf("An error occurred for resource with name '%s'", name))
  }
  
  if (inherits(data, "try-error")) {
    msg <- if (resource[["format"]] == "csv") {
      "Please check dataOptions() for this resource."
    } else {
      ""
    }
    msg <- sprintf("%s for resource with name '%s', %s", data[[1]], name, msg)
    stop(msg)
  } else if (length(data) == 0 && !is.null(attr(data, "error"))) {
    msg <- sprintf("%s for resource with name '%s'", attr(data, "error"), name)
    stop(msg)
  } else if (length(data) > 0 && nrow(data) > 0) {
    res <- data
  } else {
    msg <- sprintf("An error occurred for resource with name '%s'", name)
    stop(msg)
  }
  
  return(res)
}

#' Data Options
#'
#' Set options for \code{utils::read.csv()}, \code{openxlsx::read.xlsx()} or 
#' \code{readxl::read_excel}. Choose delimiter and decimal separator as well
#' as sheetnumbner and number of rows to read.
#'
#' @inheritParams utils::read.csv
#' @inheritParams openxlsx::read.xlsx
#'
#' @return a list of extra options for \code{utils::read.csv()} or \code{openxlsx::read.xlsx()} or
#'  \code{readxl::read_excel}, respectively
#' @export
dataOptions <- function(nrows = NA_integer_,
                        colNames = TRUE,
                        sep = ",",
                        dec = ".",
                        fileEncoding = "",
                        sheet = 1) {
  list(
    nrows = nrows,
    colNames = colNames,
    text = list(sep = sep,
                dec = dec,
                fileEncoding = fileEncoding),
    xlsx = list(sheet = sheet)
  )
}

#' Validate Resource
#'
#' @param resource (data.frame) resources data frame
#' @inheritParams getResources
#'
#' @return (data.frame) resource, or error if empty
validateResource <- function(resource, repository) {
  if (nrow(resource) == 0) {
    stop(sprintf("No resource found for repository '%s'", repository))
  }
  return(resource)
}

#' Filter Resource by Name
#'
#' @param resource (data.frame) resources data frame
#' @param name (character) name of a resource
#'
#' @return (data.frame) filtered resource
filterResourceByName <- function(resource, name) {
  resource <- resource[resource[["name"]] == name, ]
  if (nrow(resource) == 0) {
    stop(sprintf("No resource found with name '%s'", name))
  }
  return(resource)
}
  
#' Filter Resource by Valid File Type
#'
#' @inheritParams filterResourceByName
#'
#' @return (data.frame) filtered resource
filterValidFileType <- function(resource, name) {
  validFileTypes <- config()$fileTypes
  resource <- resource[resource[["format"]] %in% validFileTypes, ]
  if (nrow(resource) == 0) {
    stop(sprintf(
      "No resource found with name '%s' and with valid file type (%s)",
      name,
      paste(validFileTypes, collapse = ", ")
    ))
  }
  return(resource)
}
  
#' Select Single File from Resources
#'
#' @inheritParams filterResourceByName
#'
#' @return (data.frame) selected resource
selectSingleFile <- function(resource) {
  if (nrow(resource) > 1) {
    orderVec <- na.omit(match(config()$fileTypes, resource[["format"]]))
    resource <- resource[orderVec, ]
    resource <- resource[1, ]
  }
  return(resource)
}

guessFileEncoding <- function(path) {
  guessed <- suppressWarnings(readr::guess_encoding(path))
  if (nrow(guessed) > 0) as.character(guessed[1, 1]) else ""
}

isRemotePath <- function(path) {
  if (!is.character(path) || length(path) != 1 || is.na(path)) return(FALSE)
  grepl("^https?://", path, ignore.case = TRUE)
}

getDownloadExtension <- function(path, type) {
  sanitizedPath <- sub("[?#].*$", "", path)
  pathExt <- tolower(tools::file_ext(sanitizedPath))

  if (type == "xlsx" && pathExt == "xls") {
    return(".xls")
  }

  if (pathExt != "") {
    return(paste0(".", pathExt))
  }

  paste0(".", tolower(type))
}

downloadRemoteResource <- function(path, type) {
  tmpDir <- tempfile(pattern = "pandora-")
  dir.create(tmpDir, recursive = TRUE, showWarnings = FALSE)

  localPath <- file.path(
    tmpDir,
    paste0("resource", getDownloadExtension(path = path, type = type))
  )

  handle <- curl::new_handle(useragent = pandoraUser())
  curl::handle_setopt(handle, followlocation = TRUE)

  response <- tryCatch(
    curl::curl_fetch_disk(url = path, path = localPath, handle = handle),
    error = function(e) {
      unlink(tmpDir, recursive = TRUE, force = TRUE)
      stop(e)
    }
  )

  if (!identical(response$status_code, 200L)) {
    unlink(tmpDir, recursive = TRUE, force = TRUE)
    stop(sprintf("Failed to download '%s' (HTTP %s).", path, response$status_code))
  }

  list(path = localPath, dir = tmpDir)
}

# returns list(path, dir); caller must register on.exit(unlink(result$dir, ...))
resolveLocalPath <- function(path, type) {
  if (!isRemotePath(path)) return(list(path = path, dir = NULL))
  downloaded <- downloadRemoteResource(path = path, type = type)
  list(path = downloaded$path, dir = downloaded$dir)
}

#' Load Data
#'
#' @param path path to the file
#' @param type (character) type of file, one of \code{c("xlsx", "xls", "odt", "csv", "txt")}
#' @inheritParams utils::read.csv
#' @inheritParams openxlsx::read.xlsx
#' @inheritParams getData
#'
#' @return (data.frame) data loaded from the file at path
#' @export
loadData <-
  function(path,
           type = c("xlsx", "xls", "odt", "csv", "txt"),
           nrows = NA_integer_,
           sep = ",",
           dec = ".",
           fileEncoding = "",
           colNames = TRUE,
           sheet = 1,
           verbose = TRUE) {
    type <- match.arg(type)

    resolved <- resolveLocalPath(path = path, type = type)
    path <- resolved$path
    on.exit(if (!is.null(resolved$dir)) unlink(resolved$dir, recursive = TRUE, force = TRUE), add = TRUE)
    
    if (fileEncoding == "") {
      fileEncoding <- guessFileEncoding(path)
    }
    
    if (type %in% c("csv", "txt")) {
      if (verbose) message(sprintf("Encoding: '%s'.\n", fileEncoding))
      isOldROnWindows()
    }
    
    if (type == "xlsx") {
      xlsSplit <- strsplit(path, split = "\\.")[[1]]
      if (xlsSplit[length(xlsSplit)] == "xls") {
        type <- "xls"
      }
    }
    
    data <- switch(
      type,
      csv = suppressWarnings({
        read.csv(
          path,
          header = colNames,
          sep = sep,
          dec = dec,
          stringsAsFactors = FALSE,
          row.names = NULL,
          fileEncoding = fileEncoding,
          nrows = getNrow(type = type, nrows = nrows)
        )
      }),
      txt = suppressWarnings({
        read.csv(
          path,
          header = colNames,
          sep = sep,
          dec = dec,
          stringsAsFactors = FALSE,
          row.names = NULL,
          fileEncoding = fileEncoding,
          nrows = getNrow(type = type, nrows = nrows)
        )
      }),
      xlsx = read.xlsx(
        path,
        sheet = sheet,
        colNames = colNames,
        rows = getNrow(type = type, nrows = nrows)
      ),
      xls = suppressWarnings({
        readxl::read_excel(
          path,
          sheet = sheet,
          col_names = colNames,
          n_max = getNrow(type = type, nrows = nrows)
        )
      }),
      ods = readODS::read_ods(
        path,
        sheet = sheet,
        col_names = colNames,
        range = getNrow(type = type, nrows = nrows)
      )
    )
    
    if (type %in% c("csv", "txt")) {
      errorInfo <- sprintf("Encoding: '%s', seperator: '%s', dec character: '%s'.", 
                           fileEncoding, sep, dec)
    } else {
      errorInfo <- ""
    }
    
    if (is.null(dim(data))) {
      stop(paste("Could not determine dimensions of data", errorInfo))
    }
    
    if (any(dim(data) == 1)) {
      stop(paste("Number of rows or columns equal to 1.", errorInfo))
    }
    
    if (any(dim(data) == 0)) {
      stop(paste("Number of rows or columns equal to 0", errorInfo))
    }
    
    return(data)
  }

#' Load Text
#'
#' @param path path or URL to a text file
#' @param collapse (logical) if TRUE, collapse all lines to a single string
#' @param lineSeparator (character) separator used when collapsing lines
#' @inheritParams getData
#' @inheritParams utils::read.csv
#'
#' @return (character vector) lines from the text file, or a single string if
#'   collapse is TRUE
#' @export
loadText <- function(path,
                     fileEncoding = "",
                     collapse = FALSE,
                     lineSeparator = "\n",
                     verbose = TRUE) {
  if (!is.character(path) || length(path) != 1 || is.na(path)) {
    stop("'path' must be a single non-missing character value.")
  }

  if (!is.character(fileEncoding) || length(fileEncoding) != 1 || is.na(fileEncoding)) {
    stop("'fileEncoding' must be a single non-missing character value.")
  }

  if (!is.logical(collapse) || length(collapse) != 1 || is.na(collapse)) {
    stop("'collapse' must be a single non-missing logical value.")
  }

  if (!is.character(lineSeparator) || length(lineSeparator) != 1 || is.na(lineSeparator)) {
    stop("'lineSeparator' must be a single non-missing character value.")
  }

  resolved <- resolveLocalPath(path = path, type = "txt")
  pathToRead <- resolved$path
  on.exit(if (!is.null(resolved$dir)) unlink(resolved$dir, recursive = TRUE, force = TRUE), add = TRUE)

  if (fileEncoding == "") {
    fileEncoding <- guessFileEncoding(pathToRead)
  }

  if (verbose && fileEncoding != "") {
    message(sprintf("Encoding: '%s'.", fileEncoding))
  }

  text <- if (fileEncoding == "") {
    readLines(pathToRead, warn = FALSE)
  } else {
    readLines(pathToRead, warn = FALSE, encoding = fileEncoding)
  }

  if (collapse) {
    return(paste(text, collapse = lineSeparator))
  }

  text
}

#' get nRow
#'
#' @param type (character) file type
#' @inheritParams utils::read.csv
getNrow <- function(type, nrows = NA_integer_) {
  if (!is.null(nrows) && !is.na(nrows) &&
      is.numeric(nrows) && (nrows > 0) && nrows == round(nrows)) {
    if (type == "xlsx")
      return(1:nrows)
    else
      if (type == "ods")
        return(paste0("A1:C", nrows))
    else
      return(nrows)
  } else {
    if (type %in% c("xlsx", "ods"))
      return(NULL)
    else
      if (type == "xls")
        return(Inf)
    else
      return(-999)
  }
}

#' Is old windows
#' 
#' Checks if package is used with an older R version which possibly leads to encryption errors on Windows.
#' Gives a warning in that case.
#' 
#' @return (logical) TRUE if system is Windows and R version is < 4.2.0
isOldROnWindows <- function() {
  if (Sys.info()["sysname"] == "Windows" && 
      ((as.numeric(R.Version()$major) < 4) ||
       (as.numeric(R.Version()$major) == 4 && as.numeric(R.Version()$minor) < 2))) {
    warning("Please upgrade to R version >= 4.2.0 in order to prevent possible encryption issues when loading text files.")
    return(TRUE)
  } else {
    return(FALSE)
  }
}
