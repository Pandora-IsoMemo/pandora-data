#' Get Repositories
#' 
#' Formerly getCKANResourcesChoices()
#' 
#' @param fileType (character) list of relevant file types, e.g. c("xls", "xlsx", "csv", "odt")
#' @param repository (character) name of Pandora repository
#' @param network (character) name of Pandora network
#' @param pattern (character) string for meta information search
#' @param order (logical) if TRUE order dataframe alphabetically by name
#' 
#' @return (data.frame) containing available resources within a repository
#' @export
getResources <- function(fileType = character(),
                         repository = "", network = "", pattern = "", order = TRUE) {
  res <- callAPI(action = "current_package_list_with_resources", limit = 1000)
  
  emptyOut <- data.frame(name = character(),
                         title = character(),
                         resources = character())
  
  if (!is.null(attr(res, "error"))) {
    attr(emptyOut, "error") <- attr(res, "error")
    return(emptyOut)
  }
  
  if (!all(c("name", "title", "resources") %in% names(res))) {
    return(emptyOut)
  }
  
  res <- res %>%
    filterColumn(pattern = network, column = "groups") %>%
    filterColumn(pattern = repository, column = "name") %>%
    filterPattern(pattern = pattern)
    
  resResources <- res[["resources"]]
  names(resResources) <- res[["name"]]
  
  # select not empty files
  resResources <- lapply(resResources, function(resource) {
    resource <- resource %>% 
      filter(.data$format != "", .data$url != "")
  })
  
  resResources <- resResources[sapply(resResources, nrow) > 0]
  
  if (length(resResources) == 0) return(emptyOut)
  # select relevant entries
  resourcesDF <- lapply(seq_along(resResources), function(i) {
    # set up df
    df <- resResources[[i]][c("name", "format", "url")] 
    df$format <- df$format %>%
      gsub(pattern = "\\.", replacement = "") %>%
      tolower()
    df$repository <- names(resResources[i])
    
    if (length(fileType) > 0) {
      # filter selected file types
      df <- df[df$format %in% tolower(fileType), , drop = FALSE]
    }
    
    # arrange df
    df[, c("repository", "name", "format", "url")]
  }) %>%
    bind_rows() %>%
    distinct()
  
  if (order) {
    resourcesDF <- resourcesDF %>%
      arrange(.data$repository, .data$name)
  }
  
  resourcesDF
}

#' Get File Types
#'
#' Get all available file types of a repository or those within a 
#' specific network or within a specific repository
#' optional filtering of meta information for a given string
#'
#' @param repository (character) list of relevant file types, e.g. c("xls", "xlsx", "csv", "odt")
#' @param network (character) name of Pandora network
#' @param pattern (character) string for meta information search
#' @param order (logical) if TRUE sort list names alphabetically
#' 
#' @return (data.frame) containing available file types within a repository
#' @export
getFileTypes <- function(repository = "", network = "", pattern = "", order = TRUE) {
  res <- callAPI(action = "current_package_list_with_resources", limit = 1000)
  
  emptyOut <- data.frame(name = character(),
                         title = character(),
                         resources = character(),
                         format = character())
  
  if (!is.null(attr(res, "error"))) {
    attr(emptyOut, "error") <- attr(res, "error")
    return(emptyOut)
  }
  
  if (!all(c("name", "title", "resources") %in% names(res))) {
    return(emptyOut)
  }
  
  res <- res %>%
    filterColumn(pattern = network, column = "groups") %>%
    filterColumn(pattern = repository, column = "name") %>%
    filterPattern(pattern = pattern)
    
  resResources <- res[["resources"]]
  names(resResources) <- res[["name"]]
  
 # select not empty files
  resResources <- lapply(resResources, function(resource) {
      resource <- resource %>% 
        filter(.data$format != "", .data$url != "")
    })
    
  resResources <- resResources[sapply(resResources, nrow) > 0]
    
  if (length(resResources) == 0) return(emptyOut)

  # select relevant entries
  resourcesDF <- lapply(seq_along(resResources), function(i) {
    # set up df
    df <- resResources[[i]]["format"] 
    df$format <- df$format %>%
      gsub(pattern = "\\.", replacement = "") %>%
      tolower()
    df$name <- names(resResources[i])
    # arrange df
    df[, c("name", "format")]
  }) %>%
    bind_rows() %>%
    distinct()
  
  resourcesDF %>%
    orderDatAPI(column = "name", order = order)
}

#' Get Repositories
#' 
#' Get all vailable repositories or those within
#' a specific network
#' optional filtering of meta information for a given string 
#'
#' Formerly getCKANRecordChoices()
#' 
#' @inheritParams getResources
#' 
#' @return (data.frame) containing available repositories
#' @export
getRepositories <- function(network = "", pattern = "", order = TRUE) {
  res <- callAPI(action = "current_package_list_with_resources", limit = 1000)
  
  emptyOut <- data.frame(name = character(),
                         title = character(),
                         notes = character())
  
  if (!is.null(attr(res, "error"))) {
    attr(emptyOut, "error") <- attr(res, "error")
    return(emptyOut)
  }
  
  if (!all(c("name", "title", "notes") %in% names(res))) {
    return(emptyOut)
  }
  
  res %>%
    filterColumn(pattern = network, column = "groups") %>%
    filterColumn(pattern = repository, column = "name") %>%
    orderDatAPI(column = "title", order = order) %>%
    selectDatAPI(columns = c("name", "title", "notes"))
}

#' Get Networks
#' 
#' Get all available networks (groups in CKAN terminology)
#' optional filtering of names for a given string
#' Formerly getCKANGroupChoices()
#' 
#' @inheritParams getResources
#' 
#' @return (data.frame) giving the "name" and "display_name" of available Pandora
#' networks (groups in CKAN terminology)
#' @export
getNetworks <- function(pattern = "", order = TRUE) {
  res <- callAPI(action = "group_list", all_fields = "true")
  
  emptyOut <- data.frame(name = character(),
                         display_name = character())
  
  if (!is.null(attr(res, "error"))) {
    attr(emptyOut, "error") <- attr(res, "error")
    return(emptyOut)
  }
  
  if (!all(c("name", "display_name", "description") %in% names(res))) {
    return(emptyOut)
  }
  
  res %>%
    filterPattern(pattern = pattern) %>%
    orderDatAPI(column = "display_name", order = order) %>%
    selectDatAPI(columns = c("name", "display_name", "description"))
}

filterColumn <- function(datAPI, pattern, column) {
  if (length(datAPI) == 0 || nrow(datAPI) == 0 || is.null(pattern) || pattern == "") return(datAPI)
  
  datAPI[strMatch(datAPI[[column]], pattern = pattern), , drop = FALSE]
}

orderDatAPI <- function(datAPI, column = "", order = FALSE) {
  if (length(datAPI) == 0 || nrow(datAPI) == 0 || column == "" || !order) return (datAPI)
  
  datAPI[order(datAPI[[column]]), ]
}

selectDatAPI <- function(datAPI, columns = c()) {
  if (length(datAPI) == 0 || nrow(datAPI) == 0 || length(columns) == 0) return (datAPI)
  
  datAPI[, columns, drop = FALSE]
}

#' Filter Pattern
#' 
#' Search for pattern in all columns of datAPI and filter respective rows
#'
#' @param datAPI (list) output from the Pandora API
#' @param pattern (character) string for filtering all meta information
#'
#' @return (list) a data.frame with rows that contain the pattern
filterPattern <- function(datAPI, pattern = "") {
  if (length(datAPI) == 0 || nrow(datAPI) == 0 || is.null(pattern) || pattern == "")
    return(datAPI)
  
  errMsg <- NULL
  filterMeta <- sapply(1:nrow(datAPI), function(n) {
    res <- try(datAPI[n, ] %>%
                 unlist(use.names = FALSE) %>%
                 strMatch(pattern = pattern),
               silent = TRUE)
    
    if (inherits(res, "try-error")) {
      errMsg <<- res[[1]]
      return(FALSE)
    }
    
    res %>%
      any()
  })
  
  filteredDat <- datAPI[filterMeta, ]
  
  if (!is.null(errMsg)) {
    attr(filteredDat, "errorMeta") <-
      "Error in filter for Meta data ..."
  }
  
  filteredDat
}

strMatch <- function(dat, pattern) {
  dat %>%
    tolower() %>%
    grep(pattern = tolower(pattern)) %>%
    suppressWarnings()
}

#' Call API
#'
#' @param action (character) name of the endpoint
#'  "mapping"
#' @param ... parameters for the endpoint, e.g. all_fields = "true"
callAPI <- function(action = c("current_package_list_with_resources",
                               "group_list",
                               "package_list",
                               "organization_list",
                               "tag_list"), 
                    ...) {
  action <- match.arg(action)
  
  params <- list(...)
  paramString <- paste(names(params), params, sep = "=", collapse = "&")
  
  apiBaseURL <- "https://pandoradata.earth/api/3/action/"
  url <- paste0(apiBaseURL, action)
  if (paramString != "") {
    url <- paste0(url, "?", paramString)
  }
  
  data <- try({
    fromJSON(url)
  }, silent = TRUE)
  
  if (inherits(data, "try-error")) {
    warning(data[[1]])
    res <- list()
    attr(res, "errorApi") <- data[[1]]
  } else if (data$success) {
    res <- data$result
  } else if (!data$success) {
    warning(data$result)
    res <- list()
    attr(res, "errorApi") <- data$result
  } else {
    warning("An error occured")
    res <- list()
    attr(res, "errorApi") <- "An error occured"
  }
  
  res
}
