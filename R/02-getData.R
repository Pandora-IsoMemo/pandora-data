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
    attr(res, "error") <- sprintf("No resource found for repository '%s'", repository)
    return(res)
  }
  
  # filter name
  resource <- resourcesForRepo[resourcesForRepo[["name"]] == name, ]
  if (nrow(resource) == 0) {
    attr(res, "error") <- sprintf("No resource found with name '%s'", name)
    return(res)
  }
  
  # filter valid file types
  resource <- resource[resource[["format"]] %in% config()$fileTypes, ]
  if (nrow(resource) == 0) {
    attr(res, "error") <- sprintf("No resource found with name '%s' and with valid file type (%s)",
                                  name,
                                  paste(config()$fileTypes, collapse = ", "))
    return(res)
  }
  
  # select single file
  if (nrow(resource) > 1) {
    # if more than one file for one name, order by config()$fileTypes
    orderVec <- na.omit(match(config()$fileTypes, resource[["format"]]))
    resource <- resource[orderVec, ]
    # take only first file
    resource <- resource[1, ]
  }
  
  data <- try({
    loadData(file = resource[["url"]], 
             type = resource[["format"]],
             sep = options$text$sep,
             dec = options$text$dec,
             sheetId = options$xlsx$sheet,
             withColnames = options$colNames)
  }, silent = TRUE)
  
  if (inherits(data, "try-error")) {
    msg <- ""
    if (resource[["format"]] == "csv") msg <- "Please check dataOptions()."
    msg <- sprintf("%s for resource with name '%s', %s", data[[1]], name, msg)
    warning(msg)
    attr(res, "error") <- msg
  } else if (!is.null(data) && length(data) > 0 && nrow(data) > 0) {
    # data loading SUCCESS
    res <- data
  } else if (length(data) == 0 && !is.null(attr(data, "error"))) {
    msg <- sprintf("%s for resource with name '%s'", attr(data, "error"), name)
    attr(res, "error") <- msg
  } else {
    msg <- sprintf("An error occured for resource with name '%s'", name)
    warning(msg)
    attr(res, "error") <- msg
  }
  
  res
}

#' Data Options
#' 
#' @inheritParams utils::read.csv
#' @inheritParams openxlsx::read.xlsx
#' 
#' @return a list of extra options for \code{utils::read.csv()}
dataOptions <- function(sep = ",",
                        dec = ".",
                        sheet = 1,
                        colNames = TRUE) {
  list(text = list(sep = sep,
                   dec = dec),
       xlsx = list(sheet = sheet),
       colNames = colNames
  )
}

loadData <-
  function(file,
           type,
           sep = ",",
           dec = ".",
           withColnames = TRUE,
           sheetId = 1,
           headOnly = FALSE) {
    # empty result if an error occurs
    res <- list()
    
    # if(type == "csv" | type == "txt"){
    #   codepages <- setNames(iconvlist(), iconvlist())
    #   x <- lapply(codepages, function(enc) try(suppressWarnings({read.csv(file,
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
    
    encTry <- as.character(guess_encoding(file)[1, 1])
    if (type == "xlsx") {
      xlsSplit <- strsplit(file, split = "\\.")[[1]]
      if (xlsSplit[length(xlsSplit)] == "xls") {
        type <- "xls"
      }
    }
    
    data <- switch(
      type,
      csv = suppressWarnings({
        read.csv(
          file,
          header = withColnames,
          sep = sep,
          dec = dec,
          stringsAsFactors = FALSE,
          row.names = NULL,
          fileEncoding = encTry,
          nrows = getNrow(headOnly, type)
        )
      }),
      txt = suppressWarnings({
        read.csv(
          file,
          header = withColnames,
          sep = sep,
          dec = dec,
          stringsAsFactors = FALSE,
          row.names = NULL,
          fileEncoding = encTry,
          nrows = getNrow(headOnly, type)
        )
      }),
      xlsx = read.xlsx(
        file,
        sheet = sheetId,
        colNames = withColnames,
        rows = getNrow(headOnly, type)
      ),
      xls = suppressWarnings({
        readxl::read_excel(
          file,
          sheet = sheetId,
          col_names = withColnames,
          n_max = getNrow(headOnly, type)
        )
      }),
      ods = readODS::read_ods(
        file,
        sheet = sheetId,
        col_names = withColnames,
        range = getNrow(headOnly, type)
      )
    )
    
    if (is.null(data))
      return(NULL)
    
    if (is.null(dim(data))) {
      msg <- "Could not determine dimensions of data"
      attr(res, "error") <- msg
      warning(msg)
    }
    
    if (any(dim(data) == 1)) {
      msg <- "Number of rows or columns equal to 1. Please check dataOptions()."
      attr(res, "error") <- msg
      warning(msg)
      return(res)
    }
    
    if (any(dim(data) == 0)) {
      msg <- "Number of rows or columns equal to 0"
      attr(res, "error") <- msg
      warning(msg)
    }
    
    return(data)
  }

#' get nRow
#'
#' @param headOnly (logical) if TRUE, set maximal number of rows to n
#' @param type (character) file type
#' @param n (numeric) maximal number of rows if headOnly
getNrow <- function(headOnly, type, n = 3) {
  if (headOnly) {
    if (type == "xlsx")
      return(1:n)
    else
      if (type == "ods")
        return(paste0("A1:C", n))
    else
      return(n)
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