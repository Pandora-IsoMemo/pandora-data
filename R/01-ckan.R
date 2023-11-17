#' Get Resources
#' 
#' Get all available resources within a repository or filtered
#' by file type or those within a specific network or within a specific repository
#' optional filtering of meta information for a given string
#'
#' Formerly getCKANResourcesChoices()
#' 
#' @param fileType (character) list of relevant file types, e.g. c("xls", "xlsx", "csv", "odt")
#' @param repository (character) name of a Pandora repository, e.g. an entry of the output from
#'  \code{getRepositories()$name}
#' @param network (character) name of a Pandora network, e.g. an entry of the output from
#'  \code{getNetworks()$name}
#' @param pattern (character) string for meta information search
#' @param order (logical) if TRUE order dataframe alphabetically by name
#' @param packageList (data.frame) optional, output of callAPI() from a previous call to the
#'  Pandora API.
#' 
#' @return (data.frame) containing available resources within a repository
#' @export
getResources <- function(fileType = character(),
                         repository = "", 
                         network = "", 
                         pattern = "", 
                         order = TRUE,
                         packageList = data.frame()) {
  
  emptyOut <- data.frame(repository = character(),
                         name = character(),
                         format = character(),
                         url = character())
  
  if (is.null(packageList) || nrow(packageList) == 0) {
    packageList <- callAPI(action = "current_package_list_with_resources", limit = 1000)
  }
  
  res <- packageList %>%
    validateDatAPI(emptyOut = emptyOut,
                   reqCols = c("groups", "name", "resources")) %>%
    filterColumn(pattern = network, column = "groups") %>%
    filterColumn(pattern = repository, column = "name") %>%
    filterPattern(pattern = pattern)
    
  if (nrow(res) == 0) return(res)
  
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
#' @inheritParams getResources
#' 
#' @return (data.frame) containing available file types within a repository
#' @export
getFileTypes <- function(repository = "",
                         network = "",
                         pattern = "",
                         order = TRUE,
                         packageList = data.frame()) {
  
  emptyOut <- data.frame(name = character(),
                         title = character(),
                         resources = character(),
                         format = character())
  
  if (is.null(packageList) || nrow(packageList) == 0) {
    packageList <- callAPI(action = "current_package_list_with_resources", limit = 1000)
  }
  
  res <- packageList %>%
    validateDatAPI(emptyOut = emptyOut,
                   reqCols = c("name", "title", "resources")) %>%
    filterColumn(pattern = network, column = "groups") %>%
    filterColumn(pattern = repository, column = "name") %>%
    filterPattern(pattern = pattern)
    
  if (nrow(res) == 0) return(res)
  
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
#' @param mapColnames (logical) apply names from the 'Additional Info' box from
#'  'https://pandoradata.earth/dataset/' to the columns of returned data
#' 
#' @return (data.frame) containing available repositories
#' @export
getRepositories <- function(network = "",
                            pattern = "",
                            order = TRUE,
                            mapColnames = FALSE,
                            packageList = data.frame()) {
  
  if (is.null(packageList) || nrow(packageList) == 0) {
    packageList <- callAPI(action = "current_package_list_with_resources", limit = 1000)
  }
  
  packageList %>%
    validateDatAPI(emptyOut = data.frame(name = character(),
                                         title = character(),
                                         notes = character()),
                   reqCols = c("name", "title", "notes")) %>%
    filterColumn(pattern = network, column = "groups") %>%
    filterPattern(pattern = pattern) %>%
    orderDatAPI(column = "title", order = order) %>%
    selectDatAPI(columns = c("name", "title", "notes")) %>%
    renameRepoColumns(mapColnames = mapColnames)
}

#' Rename Repository Meta Columns
#' 
#' Apply names from the 'Additional Info' box from 'https://pandoradata.earth/dataset/' to the
#'  columns of returned data
#' 
#' @inheritParams getResources
#' @inheritParams getRepositories
#' 
#' @return (data.frame) containing available repositories
#' @export
renameRepoColumns <- function(packageList, mapColnames = FALSE) {
  if (!mapColnames) return(packageList)
  
  columns <- colnames(packageList)
  names(columns) <- columns
  
  # match colnames and mapping
  colNameMapping <- config()$repositoryMetaFields
  mapMatch <- match(columns, colNameMapping)
  names(columns)[!is.na(mapMatch)] <- names(colNameMapping)[na.omit(mapMatch)]
    
  # rename
  colnames(packageList) <- names(columns)
  
  packageList
}

#' Get Networks
#' 
#' Get all available networks (groups in CKAN terminology)
#' optional filtering of names for a given string
#' Formerly getCKANGroupChoices()
#' 
#' @inheritParams getResources
#' @param groupList (data.frame) optional, output of callAPI() from a previous call to the
#'  Pandora API.
#' 
#' @return (data.frame) giving the "name" and "display_name" of available Pandora
#' networks (groups in CKAN terminology)
#' @export
getNetworks <- function(pattern = "", order = TRUE, groupList = data.frame()) {
  if (is.null(groupList) || nrow(groupList) == 0) {
    groupList <- callAPI(action = "group_list", all_fields = "true")
  }
  
  groupList %>%
    validateDatAPI(emptyOut = data.frame(name = character(),
                                         display_name = character()),
                   reqCols = c("name", "display_name", "description")) %>%
    filterPattern(pattern = pattern) %>%
    orderDatAPI(column = "display_name", order = order) %>%
    selectDatAPI(columns = c("name", "display_name", "description"))
}

validateDatAPI <- function(datAPI, emptyOut = data.frame(), reqCols = character(0)) {
  if (!is.null(attr(datAPI, "error"))) {
    attr(emptyOut, "error") <- attr(datAPI, "error")
    return(emptyOut)
  }
  
  if (!all(reqCols %in% names(datAPI))) {
    return(emptyOut)
  }
  
  datAPI
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
#' 
#' @export
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
