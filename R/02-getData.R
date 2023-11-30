#' Get Data
#'
#' @param name (character) name of a resource, e.g. an entry of the output from
#'  \code{getResources()$name}
#' @param options (list) a list of extra options for \code{read.csv()} or \code{openxlsx::read.xlsx()} and
#'  \code{readxl::read_excel}
#' @inheritParams getResources
#'
#' @return (data.frame) return data from the Pandora API
#' @export
getData <- function(name,
                    repository = "",
                    options = dataOptions()) {
  # empty result if an error occurs
  res <- list()
  
  # filter repository
  resourcesForRepo <- getResources(repository = repository)
  if (nrow(resourcesForRepo) == 0) {
    attr(res, "error") <-
      sprintf("No resource found for repository '%s'", repository)
    return(res)
  }
  
#' Filter Resource by Name
#'
#' @param name (character) name of a resource
#' @param resources (data.frame) resources data frame
#'
#' @return (data.frame) filtered resource
filterResourceByName <- function(name, resources) {
  resource <- resources[resources[["name"]] == name, ]
  if (nrow(resource) == 0) {
    stop(sprintf("No resource found with name '%s'", name))
  }
  return(resource)
}
  
#' Filter Resource by Valid File Type
#'
#' @param resource (data.frame) resource data frame
#'
#' @return (data.frame) filtered resource
filterValidFileType <- function(resource) {
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
  
  # select single file
  if (nrow(resource) > 1) {
    # if more than one file for one name, order by config()$fileTypes
    orderVec <-
      na.omit(match(config()$fileTypes, resource[["format"]]))
    resource <- resource[orderVec, ]
    # take only first file
    resource <- resource[1, ]
  }
  
#' Load Data from Resource
#'
#' @param resource (data.frame) resource data frame
#' @param options (list) data loading options
#'
#' @return (data.frame) data loaded from the resource
loadDataFromResource <- function(resource, options) {
  data <- try({
    loadData(
      path = resource[["url"]],
      type = resource[["format"]],
      nrows = options$nrows,
      sep = options$text$sep,
      dec = options$text$dec,
      colNames = options$colNames,
      sheet = options$xlsx$sheet
    )
  }, silent = TRUE)
  handleDataLoadResult(data, resource)
}
  
#' Handle Data Load Result
#'
#' @param data result of data loading
#' @param resource (data.frame) resource data frame
#'
#' @return (data.frame) loaded data or modified result list
handleDataLoadResult <- function(data, resource) {
  res <- list()
  
  if (inherits(data, "try-error")) {
    msg <- if (resource[["format"]] == "csv") {
      "Please check dataOptions()."
    } else {
      ""
    }
    msg <- sprintf("%s for resource with name '%s', %s", data[[1]], name, msg)
    warning(msg)
    stop(msg)
  } else if (!is.null(data) && length(data) > 0 && nrow(data) > 0) {
    res <- data
  } else if (length(data) == 0 && !is.null(attr(data, "error"))) {
    msg <- sprintf("%s for resource with name '%s'", attr(data, "error"), name)
    stop(msg)
  } else {
    msg <- sprintf("An error occurred for resource with name '%s'", name)
    warning(msg)
    stop(msg)
  }
  
  return(res)
}
  
  res
}

#' Data Options
#'
#' @inheritParams utils::read.csv
#' @inheritParams openxlsx::read.xlsx
#'
#' @return a list of extra options for \code{utils::read.csv()} or \code{openxlsx::read.xlsx()} or
#'  \code{readxl::read_excel}, respectively
#' @export
dataOptions <- function(nrows = NA_integer_,
                        sep = ",",
                        dec = ".",
                        sheet = 1,
                        colNames = TRUE) {
  list(
    text = list(sep = sep,
                dec = dec),
    xlsx = list(sheet = sheet),
    nrows = nrows,
    colNames = colNames
  )
}

#' Load Data
#'
#' @param path path to the file
#' @param type (character) type of file, one of \code{c("xlsx", "xls", "odt", "csv", "txt")}
#' @inheritParams utils::read.csv
#' @inheritParams openxlsx::read.xlsx
#'
#' @return (data.frame) data loaded from the file at path
#' @export
loadData <-
  function(path,
           type = c("xlsx", "xls", "odt", "csv", "txt"),
           nrows = NA_integer_,
           sep = ",",
           dec = ".",
           colNames = TRUE,
           sheet = 1) {
    type <- match.arg(type)
    # empty result if an error occurs
    res <- list()
    
    # if(type == "csv" | type == "txt"){
    #   codepages <- setNames(iconvlist(), iconvlist())
    #   x <- lapply(codepages, function(enc) try(suppressWarnings({read.csv(path,
    #                                                     fileEncoding=enc,
    #                                                     sep = sep, dec = dec,
    #                                                     stringsAsFactors = FALSE,
    #                                                     row.names = NULL,
    #                                                     nrows=3, header=TRUE)}),
    #                                            silent = TRUE)) # you get lots of errors/warning here
    #   x <- x[!sapply(x, function(y) class(y) %in% "try-error")]
    #   maybe_ok <- which(sapply(x, function(y) isTRUE(all.equal(dim(y)[1], c(3)))))
    #   if(length(maybe_ok) > 0){
    #     encTry <- names(maybe_ok[1])
    #   } else {
    #     encTry <- ""
    #   }
    # }
    
    encTry <- as.character(guess_encoding(path)[1, 1])
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
          fileEncoding = encTry,
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
          fileEncoding = encTry,
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
    
    if (is.null(data) && is.null(attr(data, "error")))
      return(NULL)
    
    if (is.null(dim(data))) {
      stop("Could not determine dimensions of data")
    }
    
    if (any(dim(data) == 1)) {
      stop("Number of rows or columns equal to 1.")
    }
    
    if (any(dim(data) == 0)) {
      stop("Number of rows or columns equal to 0")
    }
    
    return(data)
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