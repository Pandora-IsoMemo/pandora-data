# Load Data

Load Data

## Usage

``` r
loadData(
  path,
  type = c("xlsx", "xls", "odt", "csv", "txt"),
  nrows = NA_integer_,
  sep = ",",
  dec = ".",
  fileEncoding = "",
  colNames = TRUE,
  sheet = 1,
  verbose = TRUE
)
```

## Arguments

- path:

  path to the file

- type:

  (character) type of file, one of
  `c("xlsx", "xls", "odt", "csv", "txt")`

- nrows:

  integer: the maximum number of rows to read in. Negative and other
  invalid values are ignored.

- sep:

  the field separator character. Values on each line of the file are
  separated by this character. If `sep = ""` (the default for
  `read.table`) the separator is ‘white space’, that is one or more
  spaces, tabs, newlines or carriage returns.

- dec:

  the character used in the file for decimal points.

- fileEncoding:

  character string: if non-empty declares the encoding used on a file
  when given as a character string (not on an existing connection) so
  the character data can be re-encoded. See the ‘Encoding’ section of
  the help for [`file`](https://rdrr.io/r/base/connections.html),
  [“Variations on
  read.table”](https://cloud.R-project.org/doc/manuals/R-data.html#Variations-on-read_002etable)
  in R Data Import/Export, and ‘Note’.

- colNames:

  If `TRUE`, the first row of data will be used as column names.

- sheet:

  The name or index of the sheet to read data from.

- verbose:

  Logical, indicating whether to display processing messages. If TRUE,
  messages will be displayed; if FALSE, messages will be suppressed.
  Default is TRUE.

## Value

(data.frame) data loaded from the file at path
