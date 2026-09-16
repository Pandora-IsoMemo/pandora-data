# Rename Repository Meta Columns

Apply names from the 'Additional Info' box from
'https://pandoradata.earth/dataset/' to the columns of returned data

## Usage

``` r
formatRepositoryList(
  packageList,
  columns = getDatasetFields(),
  renameColumns = TRUE
)
```

## Arguments

- packageList:

  (data.frame) optional, output of callAPI() e.g. from a previous call
  to the Pandora API.

- columns:

  (character) names of columns that should be returned

- renameColumns:

  (logical) apply names from the 'Additional Info' box from
  'https://pandoradata.earth/dataset/' to the columns of returned data

## Value

(data.frame) containing available repositories
