# Load Text

Load Text

## Usage

``` r
loadText(
  path,
  fileEncoding = "",
  collapse = FALSE,
  lineSeparator = "\n",
  verbose = TRUE
)
```

## Arguments

- path:

  path or URL to a text file

- fileEncoding:

  character string: if non-empty declares the encoding used on a file
  when given as a character string (not on an existing connection) so
  the character data can be re-encoded. See the ‘Encoding’ section of
  the help for [`file`](https://rdrr.io/r/base/connections.html),
  [“Variations on
  read.table”](https://cloud.R-project.org/doc/manuals/R-data.html#Variations-on-read_002etable)
  in R Data Import/Export, and ‘Note’.

- collapse:

  (logical) if TRUE, collapse all lines to a single string

- lineSeparator:

  (character) separator used when collapsing lines

- verbose:

  Logical, indicating whether to display processing messages. If TRUE,
  messages will be displayed; if FALSE, messages will be suppressed.
  Default is TRUE.

## Value

(character vector) lines from the text file, or a single string if
collapse is TRUE
