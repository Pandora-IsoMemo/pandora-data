
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Pandora

<!-- badges: start -->

[![R-CMD-check](https://github.com/Pandora-IsoMemo/pandora-data/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Pandora-IsoMemo/pandora-data/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

This R-Package is a wrapper for the [CKAN
API](https://docs.ckan.org/en/2.9/api/) of the Pandora data platform.

[Pandora](https://pandora.earth/) seeks a better understanding of the
human story by offering technological solutions to make historical and
archaeological data more accessible and easily findable. It is a
grassroots initiative where research communities from across various
academic disciplines self-curate datasets and self-manage community
membership and roles. These datasets may be deposited externally or
within the Pandora data platform itself. Datasets can be assigned DOIs
and are indexed so that they are findable using scientific search
engines.

The Pandora initiative is also developing computational methods to
improve data usage and integration. Artificial intelligence techniques
are employed to investigate the dynamics of past human societies. This
generates useful knowledge that can be employed by policymakers and risk
analysts to address present and future societal challenges.

Pandora promotes data communities and networking. It offers new tools to
handle and model data plus AI methods to investigate historical
causation.

## Installation

You can install the released version of Pandora from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("Pandora")
```

You can install the development version of Pandora from
[GitHub](https://github.com/) with:

``` r
devtools::install_github("Pandora-IsoMemo/pandora-data")
```

## Example of how to use the package

This is a basic example of Pandora …

``` r
library(Pandora)

## basic example code
df <- getRepositories()

head(df)
#>                                                                                               Repository
#> 36                                                                                               14CARHU
#> 52                  14SEA Project:  A 14C database for Southeast Europe and Anatolia (10,000–3000 calBC)
#> 17                                                                             AfriArch isotopic dataset
#> 34                                                                                                 AGEAS
#> 27 Amalthea: a Database of Isotopic measurements on Archaeological and Forensic Tooth Dentine Increments
#> 8                                                                                   Archaeobotany videos
#>                                                                                                    Name
#> 36                                                                                              14carhu
#> 52                     14sea-project-a-14c-database-for-southeast-europe-and-anatolia-10-000-3000-calbc
#> 17                                                                            afriarch-isotopic-dataset
#> 34                                                                                                ageas
#> 27 amalthea-a-database-of-isotopic-measurements-on-archaeological-and-forensic-tooth-dentine-increments
#> 8                                                                                       video-tutorials
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    Description
#> 36                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       The 14CARHU (RadioCARbon Dates of Helsinki University) is an ultimate public and searchable database for the University of Helsinki radiocarbon data.
#> 52                                                                                               14SEA is a new 14C database for Southeast Europe, the Aegean and Anatolia targeting the Mesolithic, Neolithic and Chalcolithic periods.\r\nWe are especially concerned with the Mesolithic–Neolithic trajectories in key areas of the dispersal of farming from Anatolia into Europe. By including the Chalcolithic period (although differently called in the prehistories of Turkey and Southeast Europe), we aim at providing a chronological framework during which both a consolidation of farming and major changes from Neolithic life styles in these areas occurred.
#> 17                                                                                                                                                                                                                                                                                                                                                                                                                                                                              The dataset contains bioarchaeological isotopic measurements from African archaeological sites dating to the Holocene. Modern samples were included if reported within archaeological studies.
#> 34                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  Eastern European Radiocarbon Database (AGEAS) provides an outlet for archaeological radiometric data from eastern and northeastern Europe.
#> 27 Amalthea is a global database of stable isotope measurements on tooth increments from archaeological and modern individuals spanning more than 7,000 years.  The dataset includes c. 15,000 isotopic measurements from more than 700 individuals. In addition to isotopic data the database also includes information on the archaeological context and osteological features of recorded individuals. This database allows for the reconstruction of individual iso-biographies. In particular, the database allows for meta-studies on childhood diet, nutrition, and health across time and space. The database is a collaborative effort and will be regularly updated.
#> 8                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            Videos tutorials on archaeobotany
#>    Existing DOI                           Assigned DOI Version
#> 36                                                            
#> 52                                                            
#> 17              https://www.doi.org/10.48493/z8j6-cp86     1.0
#> 34                                                            
#> 27                                  10.48493/sak5-9487        
#> 8                                                             
#>                                                                         Author
#> 36                                                                            
#> 52                                         Agathe Reingruber & Laurens Thissen
#> 17 Steve Goldstein, Jesse Wolfhagen, Sean Hixon, Erin Scott, Ricardo Fernandes
#> 34                                                                            
#> 27                                           Carlo Cocozza & Ricardo Fernandes
#> 8                                                                             
#>                      Author Email      Maintainer    Maintainer Email
#> 36                                                                   
#> 52 areingruber@zedat.fu-berlin.de                                    
#> 17           goldstein@shh.mpg.de Victor Iminjili iminjili@shh.mpg.de
#> 34                                                                   
#> 27             cocozza@shh.mpg.de   Carlo Cocozza  cocozza@shh.mpg.de
#> 8                                                                    
#>    Chronological range (min) Chronological range (max)              Spatial Box
#> 36                                                                             
#> 52                    -10000                     -3000                         
#> 17                    -12000                      2020 -37.0, -26.3, 37.8, 57.4
#> 34                                                                             
#> 27                  5600 BCE                   2020 CE                     <NA>
#> 8
```
