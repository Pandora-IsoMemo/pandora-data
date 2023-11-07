#' Get Repositories
#' 
#' Formerly getCKANResourcesChoices()
#' 
#' @param fileType (character) list of relevant file types, e.g. c("xls", "xlsx", "csv", "odt")
#' @param repository (character) name of Pandora repository
#' @param network (character) name of Pandora network
#' @param pattern (character) string for meta information search
#' @param sort (logical) if TRUE sort list names alphabetically
#' 
#' @export
getResourceList <- function(fileType = c(), repository = "", network = "", pattern = "", sort = TRUE) {
  res <- callAPI(action = "current_package_list_with_resources", limit = 1000)
  
  if (!is.null(attr(res, "error"))) {
    resList <- c("")
    names(resList) <- attr(res, "error")
    return(resList)
  }
  
  if (!all(c("title", "resources") %in% names(res))) {
    return(emptyList("Pandora resource"))
  }
  
  res <- res %>%
    filterNetwork(network = network) %>%
    filterPattern(pattern = pattern) %>%
    filterRepository(repository = repository)
  
  if (nrow(res) == 0) return(emptyList("Pandora resource"))
  
  resName <- res[["title"]]
  resList <- res[["resources"]]
  names(resList) <- resName
  
  # filter fileType and select name and url
  if (!is.null(fileType) && length(fileType) > 0 && any(fileType != "")) {
    fileType <- fileType[fileType != ""]
    resList <- lapply(resList, function(resource) {
      resource <- resource %>% 
        dplyr::filter(.data$format != "", .data$url != "")
      
      typeFound <- sapply(fileType, function(type) {
        strMatch(dat = resource$format, pattern = type) %>% length()
      }) > 0
      
      resource %>% 
        dplyr::filter(any(typeFound))
    })
    
    resList <- resList[sapply(resList, nrow) > 0]
    
    if (length(resList) == 0) return(emptyList("Pandora resource"))
  }
  
  # sort list
  if (sort) {
    resList <- resList[order(names(resList))]
  }
  
  # select relevant entries
  resList <- lapply(resList, function(x) {
    fileURL <- x[["url"]]
    names(fileURL) <- sprintf("%s (%s)", x[["name"]], x[["format"]]) %>%
      gsub(pattern = "^ *|(?<= ) | *$", replacement = "", perl = TRUE) %>%
      gsub(pattern = "\t", replacement = "", perl = TRUE)
    fileURL
  })
  
  c("Select Pandora resource ..." = "", resList)
}

#' Get File Types
#' 
#' @param repository (character) list of relevant file types, e.g. c("xls", "xlsx", "csv", "odt")
#' @param network (character) name of Pandora network
#' @param pattern (character) string for meta information search
#' @param sort (logical) if TRUE sort list names alphabetically
#' 
#' @export
getTypeList <- function(repository = "", network = "", pattern = "", sort = TRUE) {
  res <- callAPI(action = "current_package_list_with_resources", limit = 1000)
  
  if (!is.null(attr(res, "error"))) {
    resList <- c("")
    names(resList) <- attr(res, "error")
    return(resList)
  }
  
  if (!all(c("title", "resources") %in% names(res))) {
    return(emptyList("file types"))
  }
  
  res <- res %>%
    filterNetwork(network = network) %>%
    filterPattern(pattern = pattern) %>%
    filterRepository(repository = repository)
  
  if (nrow(res) == 0) return(emptyList("file types"))
  
  resList <- res[["resources"]]
  
 # select file types
  resList <- lapply(resList, function(resource) {
      resource <- resource %>% 
        dplyr::filter(.data$format != "", .data$url != "")
    })
    
  resList <- resList[sapply(resList, nrow) > 0]
    
  if (length(resList) == 0) return(emptyList("file types"))

  # select relevant entries
  resList <- lapply(resList, `[[`, "format") %>%
    unlist() %>%
    unique()
  
  # sort list
  if (sort) {
    resList <- resList[order(resList)]
  }
  
  resList
}

#' Get Repositories
#' 
#' Formerly getCKANRecordChoices()
#' 
#' @inheritParams getResourceList
#' 
#' @export
getRepositoryList <- function(network = "", pattern = "", sort = TRUE) {
  res <- callAPI(action = "current_package_list_with_resources", limit = 1000)
  
  if (!is.null(attr(res, "error"))) {
    resList <- c("")
    names(resList) <- attr(res, "error")
    return(resList)
  }
  
  if (!all(c("name", "title") %in% names(res))) {
    return(emptyList("Pandora repository"))
  }
  
  res <- res %>%
    filterNetwork(network = network) %>%
    filterPattern(pattern = pattern)
  
  if (nrow(res) == 0) return(emptyList("Pandora repository"))
  
  resName <- res[["title"]]
  resList <- c(res[["name"]])
  names(resList) <- resName
  
  # sort list
  if (sort) {
    resList <- resList[order(names(resList))]
  }
  
  c("Select Pandora repository ..." = "", resList)
}

#' Get Networks
#' 
#' Formerly getCKANGroupChoices()
#' 
#' @inheritParams getResourceList
#' 
#' @export
getNetworkList <- function(pattern = "", sort = TRUE) {
  res <- callAPI(action = "group_list", all_fields = "true")
  
  if (!is.null(attr(res, "error"))) {
    resList <- c("")
    names(resList) <- attr(res, "error")
    return(resList)
  }
  
  if (!all(c("name", "display_name") %in% names(res))) {
    return(emptyList("Pandora network"))
  }
  
  res <- res %>%
    filterPattern(pattern = pattern)
  
  if (nrow(res) == 0) return(emptyList("Pandora network"))
  
  
  resName <- res[["display_name"]]
  resList <- c(res[["name"]])
  names(resList) <- resName
  
  # sort list
  if (sort) {
    resList <- resList[order(names(resList))]
  }
  
  resList
}

filterNetwork <- function(datAPI, network) {
  if (length(datAPI) == 0 || nrow(datAPI) || is.null(network) || network == "") return(datAPI)
  
  datAPI[strMatch(datAPI[["groups"]], pattern = network), ]
}

filterRepository <- function(datAPI, repository) {
  if (length(datAPI) == 0 || nrow(datAPI) || is.null(repository) || repository == "") return(datAPI)
  datAPI[strMatch(datAPI[["name"]], pattern = repository), ]
}

emptyList <- function(what = c("Pandora resource", "Pandora repository", "Pandora network", 
                               "file types")) {
  what <- match.arg(what)
  res <- c("")
  names(res) <- sprintf("No %s available ...", what)
  res
}

strMatch <- function(dat, pattern) {
  dat %>%
    tolower() %>%
    grep(pattern = tolower(pattern)) %>%
    suppressWarnings()
}

getCKANFiles <- function(message = "Updating list of Pandora repositories ...",
                         isInternet = has_internet()) {
  if (isRunning()) {
    getCKANFileList(isInternet = isInternet) %>%
      withProgress(value = 0.8, message = message)
  } else {
    getCKANFileList(isInternet = isInternet)
  }
}

getCKANFileList <- function(isInternet = has_internet()) {
  if (!isInternet) {
    res <- list()
    attr(res, "errorApi") <- "No internet connection ..."
    return(res)
  }
  
  testCon <-
    tryGET(path = "https://pandora.earth/")
  if (is.null(testCon)) {
    res <- list()
    attr(res, "errorApi") <-
      "Cannot reach 'https://pandora.earth/' ..."
    return(res)
  }
  
  apiCon <-
    tryGET(path = "https://pandoradata.earth/api/3/action/current_package_list_with_resources?limit=1000")
  if (is.null(apiCon)) {
    res <- list()
    attr(res, "errorApi") <-
      "Could not retrieve data from 'https://pandoradata.earth/api' ..."
    return(res)
  }
  
  return(apiCon$result)
}

#' Filter CKAN by Group
#'
#' @param ckanFiles (list) output from the Pandora API already filtered for relevant entries
#' @param ckanGroup (character) title of a CKAN group
#'
#' @return (list) a fileList where the entries 'groups' == ckanGroup
filterCKANGroup <- function(ckanFiles, ckanGroup = NA) {
  if (length(ckanGroup) == 0 || all(is.na(ckanGroup)) ||
      all(ckanGroup == "") ||
      all(ckanGroup == "NA"))
    return(ckanFiles)
  
  filterGroup <- sapply(ckanFiles, function(record) {
    if (length(record$groups) == 0)
      return(FALSE)
    
    sapply(record$groups, function(group) {
      group$name %in% ckanGroup
    }) %>%
      any()
  })
  
  ckanFiles[filterGroup]
}

#' Filter Pattern
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
  
  filteredList <- datAPI[filterMeta, ]
  
  if (!is.null(errMsg)) {
    attr(filteredList, "errorMeta") <-
      "Error in filter for Meta data ..."
  }
  
  filteredList
}

filterCKANFileList <- function(fileList) {
  if (length(fileList) == 0)
    return(fileList)
  
  files <- lapply(fileList, filterSingleCKANRecord)
  keyBy(files, "title")
}

#' Filter Single CKAN Record
#'
#' Removes all meta information that is not needed by selecting only relevant entries.
#'
#' @param record (list) single entry from fileList
filterSingleCKANRecord <- function(record) {
  # if is.null(record$...), empty list will be returned
  resources <- lapply(record$resources, filterSingleCKANResource)
  groups <- lapply(record$groups, filterSingleCKANGroup)
  
  list(
    title = record$title,
    resources = keyBy(resources, "name"),
    groups = keyBy(groups, "title")
  )
}

filterSingleCKANResource <- function(resource) {
  list(name = resource$name,
       format = resource$format,
       url = resource$url)
}

filterSingleCKANGroup <- function(group) {
  list(
    name = group$name,
    title = group$title,
    description = group$description
  )
}

keyBy <- function(l, key) {
  n <- unlist(lapply(l, `[[`, key))
  names(l) <- n
  l
}

tryGET <- function(path, isInternet = has_internet()) {
  if (!isInternet)
    return(NULL)
  
  res <- try({
    httr::GET(path, timeout(2))
  }, silent = TRUE)
  
  if (inherits(res, "try-error") ||
      res$status_code == 500 || !is.null(httr::content(res)[["message"]])) {
    # if there is a message than an error occurred
    # We do not need to print an alert! If output is empty UI tells a message
    # apiName = "pandoradata.earth" or apiName = "api.github.com"
    # shinyjs::alert(paste("Could not retrieve data from", apiName))
    NULL
  } else if (res$status_code == 200) {
    httr::content(res)
  } else {
    NULL
  }
}

#' Call API
#'
#' @param action (character) name of the endpoint
#'  "mapping"
#' @param ... parameters for the endpoint, e.g. all_fields = "true"
callAPI <- function(action = c("current_package_list_with_resources", "group_list", "package_list",
                               "organization_list", "tag_list"), ...) {
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

has_internet <- function(timeout = 2) {
  res <- try({
    httr::GET("http://google.com/", timeout(timeout))
  }, silent = TRUE)
  
  ! inherits(res, "try-error")
}
