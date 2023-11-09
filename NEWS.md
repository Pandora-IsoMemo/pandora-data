# Pandora 23.11.0

## Features
The main function that facilitate the data retrieval and aggregation from the API is:

 - `getData()`

The following are its sub-functions contained in this package: 

  - `getNetworks()` returns a data.frame containing available networks (groups in CKAN terminology)
    - optional filtering of names for a given string
  - `getRepositories()` returns a data.frame containing available repositories 
    - all or those within a specific network
    - optional filtering of meta information for a given string
  - `getFileTypes()` returns a data.frame containing available file types of a repository
    - all or those within a specific network or within a specific repository
    - optional filtering of meta information for a given string
  - `getResources()` returns a data.frame containing available resources within a repository
    - all or filtered by file type or those within a specific network or within a specific repository
    - optional filtering of meta information for a given string
