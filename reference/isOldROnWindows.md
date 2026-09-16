# Is old windows

Checks if package is used with an older R version which possibly leads
to encryption errors on Windows. Gives a warning in that case.

## Usage

``` r
isOldROnWindows()
```

## Value

(logical) TRUE if system is Windows and R version is \< 4.2.0
