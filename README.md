
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Pandora

<!-- badges: start -->

[![R-CMD-check](https://github.com/Pandora-IsoMemo/pandora-data/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Pandora-IsoMemo/pandora-data/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of Pandora is to …

## Installation

You can install the released version of Pandora from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("Pandora")
#> Installiere Paket nach '/tmp/RtmpGqgRQN/temp_libpath286b6dbe7c9b'
#> (da 'lib' nicht spezifiziert)
#> Warning: package 'Pandora' is not available for this version of R
#> 
#> A version of this package for your version of R might be available elsewhere,
#> see the ideas at
#> https://cran.r-project.org/doc/manuals/r-patched/R-admin.html#Installing-packages
```

You can install the development version of Pandora from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Pandora-IsoMemo/pandora-data")
#> Downloading GitHub repo Pandora-IsoMemo/pandora-data@HEAD
#> readODS (1.8.0 -> 2.1.0) [CRAN]
#> Installing 1 packages: readODS
#> Installiere Paket nach '/tmp/RtmpGqgRQN/temp_libpath286b6dbe7c9b'
#> (da 'lib' nicht spezifiziert)
#> ── R CMD build ─────────────────────────────────────────────────────────────────
#> * checking for file ‘/tmp/RtmpD7pucP/remotes40bf37ba326/Pandora-IsoMemo-pandora-data-e3a0600/DESCRIPTION’ ... OK
#> * preparing ‘Pandora’:
#> * checking DESCRIPTION meta-information ... OK
#> * checking for LF line-endings in source and make files and shell scripts
#> * checking for empty or unneeded directories
#> Omitted ‘LazyData’ from DESCRIPTION
#> * building ‘Pandora_23.11.2.tar.gz’
#> Installiere Paket nach '/tmp/RtmpGqgRQN/temp_libpath286b6dbe7c9b'
#> (da 'lib' nicht spezifiziert)
```

## Example of how to use the package

This is a basic example of Pandora …

``` r
install.packages("Pandora")
#> Installiere Paket nach '/tmp/RtmpGqgRQN/temp_libpath286b6dbe7c9b'
#> (da 'lib' nicht spezifiziert)
#> Warning: package 'Pandora' is not available for this version of R
#> 
#> A version of this package for your version of R might be available elsewhere,
#> see the ideas at
#> https://cran.r-project.org/doc/manuals/r-patched/R-admin.html#Installing-packages

library(Pandora)
## basic example code

df <- getRepositories()
```
