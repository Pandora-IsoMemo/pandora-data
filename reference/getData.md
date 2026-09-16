# Get Data

Get Data

## Usage

``` r
getData(name, repository = "", verbose = TRUE, options = dataOptions())
```

## Arguments

- name:

  (character) name of a resource, e.g. an entry of the output from
  `getResources()$name`

- repository:

  (character) name of a Pandora repository, e.g. an entry of the output
  from `getRepositories()$name`

- verbose:

  Logical, indicating whether to display processing messages. If TRUE,
  messages will be displayed; if FALSE, messages will be suppressed.
  Default is TRUE.

- options:

  (list) a list of extra options for
  [`read.csv()`](https://rdrr.io/r/utils/read.table.html) or
  [`openxlsx::read.xlsx()`](https://rdrr.io/pkg/openxlsx/man/read.xlsx.html)
  and
  [`readxl::read_excel`](https://readxl.tidyverse.org/reference/read_excel.html)

## Value

(data.frame) return data from the Pandora API
