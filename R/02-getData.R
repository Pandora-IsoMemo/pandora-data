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
             nrows = options$nrows,
             sep = options$text$sep,
             dec = options$text$dec,
             colNames = options$colNames,
             sheet = options$xlsx$sheet)
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
#' @return a list of extra options for \code{read.csv()} or \code{openxlsx::read.xlsx()} or
#'  \code{readxl::read_excel}, respectively
dataOptions <- function(nrows = NA_integer_,
                        sep = ",",
                        dec = ".",
                        sheet = 1,
                        colNames = TRUE) {
  list(text = list(sep = sep,
                   dec = dec),
       xlsx = list(sheet = sheet),
       nrows = nrows,
       colNames = colNames
  )
}

loadData <-
  function(file,
           type,
           nrows = NA_integer_,
           sep = ",",
           dec = ".",
           colNames = TRUE,
           sheet = 1) {
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
          file,
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
        file,
        sheet = sheet,
        colNames = colNames,
        rows = getNrow(type = type, nrows = nrows)
      ),
      xls = suppressWarnings({
        readxl::read_excel(
          file,
          sheet = sheet,
          col_names = colNames,
          n_max = getNrow(type = type, nrows = nrows)
        )
      }),
      ods = readODS::read_ods(
        file,
        sheet = sheet,
        col_names = colNames,
        range = getNrow(type = type, nrows = nrows)
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