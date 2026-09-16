# Get Repositories

Get all vailable repositories or those within a specific network
optional filtering of meta information for a given string

## Usage

``` r
getRepositories(
  network = "",
  pattern = "",
  order = TRUE,
  columns = getDatasetFields(),
  renameColumns = TRUE,
  packageList = data.frame()
)
```

## Arguments

- network:

  (character) name of a Pandora network, e.g. an entry of the output
  from `getNetworks()$name`

- pattern:

  (character) string for meta information search

- order:

  (logical) if TRUE, order dataframe alphabetically by 'repository' and
  'name'

- columns:

  (character) names of columns that should be returned

- renameColumns:

  (logical) apply names from the 'Additional Info' box from
  'https://pandoradata.earth/dataset/' to the columns of returned data

- packageList:

  (data.frame) optional, output of callAPI() e.g. from a previous call
  to the Pandora API.

## Value

(data.frame) containing available repositories
