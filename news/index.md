# Changelog

## Pandora 26.09.0

### Updates

- Updated base image to r-shiny:4.4.1.
- Expanded and organized .Rbuildignore, .gitignore, and .dockerignore
  entries to reduce accidental inclusion of local/CI/build artifacts.

## Pandora 26.08.4

### Updates

- Skipped
  [`getData()`](https://pandora-isomemo.github.io/pandora-data/reference/getData.md),
  [`loadData()`](https://pandora-isomemo.github.io/pandora-data/reference/loadData.md),
  and
  [`loadText()`](https://pandora-isomemo.github.io/pandora-data/reference/loadText.md)
  tests on transient network failures, such as HTTP 429 rate limits and
  temporary connectivity issues.

## Pandora 26.08.3

### Updates

- Improved robustness of API-dependent tests by skipping on transient
  network failures (for example HTTP 429 rate limits and temporary
  connectivity issues)

## Pandora 26.08.2

### Updates

- Improved error handling and messages when the Pandora API is not
  accessible.

## Pandora 26.08.1

### Updates

- added a new loadText() helper to the Pandora R package to load
  plain-text resources (local files or remote URLs), including optional
  line-collapsing, and refactored remote-download handling to be
  reusable across data loaders.

## Pandora 26.08.0

### Bug Fixes

- fixed API access issues by setting a custom User-Agent header for API
  requests and resource downloads

## Pandora 24.02.0

### Updates

- catch issue if resources are missing in tests
- automate documentation

## Pandora 23.12.0

CRAN release: 2023-12-19

### Features

- new parameters for the function
  [`getRepositories()`](https://pandora-isomemo.github.io/pandora-data/reference/getRepositories.md)
  to select and rename the columns of the result
- vignette and documentation were added to the package

## Pandora 23.11.2

### Features

- new function
  [`getData()`](https://pandora-isomemo.github.io/pandora-data/reference/getData.md)
  was added, which enables data retrieval
- the function `dataOption()` returns a list of options for
  [`utils::read.csv()`](https://rdrr.io/r/utils/read.table.html) or
  [`openxlsx::read.xlsx()`](https://rdrr.io/pkg/openxlsx/man/read.xlsx.html)
  or
  [`readxl::read_excel`](https://readxl.tidyverse.org/reference/read_excel.html),
  respectively, that can be passed to
  [`getData()`](https://pandora-isomemo.github.io/pandora-data/reference/getData.md)

## Pandora 23.11.1

### Updates

- export of
  [`callAPI()`](https://pandora-isomemo.github.io/pandora-data/reference/callAPI.md)
  function, update of documentation

## Pandora 23.11.0

### Features

The main function that facilitate the data retrieval and aggregation
from the API is:

- [`getData()`](https://pandora-isomemo.github.io/pandora-data/reference/getData.md)
  (*under development*)

The following are its sub-functions contained in this package:

- [`getNetworks()`](https://pandora-isomemo.github.io/pandora-data/reference/getNetworks.md)
  returns a data.frame containing available networks (groups in CKAN
  terminology)
  - optional filtering of names for a given string
- [`getRepositories()`](https://pandora-isomemo.github.io/pandora-data/reference/getRepositories.md)
  returns a data.frame containing available repositories
  - all or those within a specific network
  - optional filtering of meta information for a given string
- [`getFileTypes()`](https://pandora-isomemo.github.io/pandora-data/reference/getFileTypes.md)
  returns a data.frame containing available file types of a repository
  - all or those within a specific network or within a specific
    repository
  - optional filtering of meta information for a given string
- [`getResources()`](https://pandora-isomemo.github.io/pandora-data/reference/getResources.md)
  returns a data.frame containing available resources within a
  repository
  - all or filtered by file type or those within a specific network or
    within a specific repository
  - optional filtering of meta information for a given string
