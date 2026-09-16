# Call API

Call API

## Usage

``` r
callAPI(
  action = c("current_package_list_with_resources", "group_list", "package_list",
    "organization_list", "tag_list"),
  ...
)
```

## Arguments

- action:

  (character) name of the endpoint "mapping"

- ...:

  parameters for the endpoint, e.g. all_fields = "true"

## Value

(data.frame) output from the Pandora API
