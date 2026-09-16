# Get Networks

Get all available networks (groups in CKAN terminology) optional
filtering of names for a given string

## Usage

``` r
getNetworks(pattern = "", order = TRUE, groupList = data.frame())
```

## Arguments

- pattern:

  (character) string for meta information search

- order:

  (logical) if TRUE, order dataframe alphabetically by 'repository' and
  'name'

- groupList:

  (data.frame) optional, output of callAPI() from a previous call to the
  Pandora API.

## Value

(data.frame) giving the "name" and "display_name" of available Pandora
networks (groups in CKAN terminology)
