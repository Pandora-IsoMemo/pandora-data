# Get Resources

Get all available resources within a repository or filtered by file type
or those within a specific network or within a specific repository
optional filtering of meta information for a given string

## Usage

``` r
getResources(
  fileType = character(),
  repository = "",
  network = "",
  pattern = "",
  order = TRUE,
  packageList = data.frame()
)
```

## Arguments

- fileType:

  (character) list of relevant file types, e.g. c("xls", "xlsx", "csv",
  "odt")

- repository:

  (character) name of a Pandora repository, e.g. an entry of the output
  from `getRepositories()$name`

- network:

  (character) name of a Pandora network, e.g. an entry of the output
  from `getNetworks()$name`

- pattern:

  (character) string for meta information search

- order:

  (logical) if TRUE, order dataframe alphabetically by 'repository' and
  'name'

- packageList:

  (data.frame) optional, output of callAPI() e.g. from a previous call
  to the Pandora API.

## Value

(data.frame) containing available resources within a repository
