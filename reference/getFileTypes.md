# Get File Types

Get all available file types of a repository or those within a specific
network or within a specific repository optional filtering of meta
information for a given string

## Usage

``` r
getFileTypes(
  repository = "",
  network = "",
  pattern = "",
  order = TRUE,
  packageList = data.frame()
)
```

## Arguments

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

(data.frame) containing available file types within a repository
