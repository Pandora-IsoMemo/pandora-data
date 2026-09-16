# Data Options

Set options for
[`utils::read.csv()`](https://rdrr.io/r/utils/read.table.html),
[`openxlsx::read.xlsx()`](https://rdrr.io/pkg/openxlsx/man/read.xlsx.html)
or
[`readxl::read_excel`](https://readxl.tidyverse.org/reference/read_excel.html).
Choose delimiter and decimal separator as well as sheetnumbner and
number of rows to read.

## Usage

``` r
dataOptions(
  nrows = NA_integer_,
  colNames = TRUE,
  sep = ",",
  dec = ".",
  fileEncoding = "",
  sheet = 1
)
```

## Arguments

- nrows:

  integer: the maximum number of rows to read in. Negative and other
  invalid values are ignored.

- colNames:

  If `TRUE`, the first row of data will be used as column names.

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

- sheet:

  The name or index of the sheet to read data from.

## Value

a list of extra options for
[`utils::read.csv()`](https://rdrr.io/r/utils/read.table.html) or
[`openxlsx::read.xlsx()`](https://rdrr.io/pkg/openxlsx/man/read.xlsx.html)
or
[`readxl::read_excel`](https://readxl.tidyverse.org/reference/read_excel.html),
respectively
