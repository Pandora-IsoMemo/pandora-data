# How to use the Pandora Data Package

``` r

library(Pandora)
library(magrittr)
```

## Introduction

This vignette provides an overview of the basic functions within the
Pandora Data package, allowing users to explore and retrieve information
about networks, repositories, file types, and resources.

## Show all Networks

To show all available networks use the
[`getNetworks()`](https://pandora-isomemo.github.io/pandora-data/reference/getNetworks.md)
function:

``` r

networks <- getNetworks()
networks %>% 
  knitr::kable()
```

| name | display_name | description |
|:---|:---|:---|
| isomemo-group | IsoMemo Network | IsoMemo is a network of autonomous isotopic databases. |

## List Repositories

To list all available repositories use the
[`getRepositories()`](https://pandora-isomemo.github.io/pandora-data/reference/getRepositories.md)
function. You can filter the output using the `network` and `pattern`
arguments. For instance, to list all repositories in the `IsoMemo`
network:

``` r

reposIsomemo <- getRepositories(
  network = "IsoMemo"
  )

reposIsomemo[c("Repository")]  %>% 
  knitr::kable()
```

|  | Repository |
|:---|:---|
| 70 | 14CARHU |
| 85 | 14SEA Project: A 14C database for Southeast Europe and Anatolia (10,000–3000 calBC) |
| 68 | AGEAS |
| 61 | Amalthea: a Database of Isotopic measurements on Archaeological and Forensic Tooth Dentine Increments |
| 81 | ARCHIPELAGO human stable isotope database |
| 82 | AustArch: A Database of 14C and Luminescence Ages from Archaeological Sites in Australia |
| 78 | BASE DE DATOS Iber-Crono |
| 80 | CARD 2.0 |
| 21 | CIMA: Compendium Isotoporum Medii Aevi |
| 65 | Database for European 14C dates for the Bronze and Early Iron Age |
| 84 | Database our way to Europe |
| 69 | dIANA |
| 83 | Edaphobase Open access Data Warehouse for Soil Biodiversity |
| 79 | EPRG Jomon |
| 74 | INTIMATE: WG1 Database |
| 72 | Intramolecular 2H profiles |
| 13 | LiVES isotopic database |
| 16 | NEENA Stable Isotopes v.1 |
| 64 | New Zealand Radicarbon Database |
| 71 | Northern Hemisphere Modern leaf wax δDn-alkane dataset |
| 67 | ORAU database |
| 66 | Radiocarbon Palaeolithic Europe Database v28 |
| 62 | RADON - Radiocarbon dates online |
| 73 | Royal Institute for Cultural Heritage Radiocarbon and stable isotope measurements |
| 63 | SWVID: Stable Water Vapor Isotope datasets |

## List Available File Types

Use the
[`getFileTypes()`](https://pandora-isomemo.github.io/pandora-data/reference/getFileTypes.md)
function to list all available file types. The following example
showcases file types available in the ‘IsoMemo’ network:

``` r

fileTypesIsomemo <- getFileTypes(network = "isomemo")
fileTypesIsomemo  %>% 
  knitr::kable()
```

|  | name | format |
|:---|:---|:---|
| 15 | 14carhu | xlsx |
| 22 | 14sea-project-a-14c-database-for-southeast-europe-and-anatolia-10-000-3000-calbc | xlsx |
| 14 | ageas | html |
| 8 | amalthea-a-database-of-isotopic-measurements-on-archaeological-and-forensic-tooth-dentine-increments | xlsx |
| 9 | amalthea-a-database-of-isotopic-measurements-on-archaeological-and-forensic-tooth-dentine-increments | docx |
| 10 | amalthea-a-database-of-isotopic-measurements-on-archaeological-and-forensic-tooth-dentine-increments | csv |
| 18 | archipelago-human-stable-isotope-database | csv |
| 19 | archipelago-human-stable-isotope-database | xlsx |
| 20 | archipelago-human-stable-isotope-database | rdata |
| 21 | austarch-a-database-of-14c-and-luminescence-ages-from-archaeological-sites-in-australia | csv |
| 4 | cima-compendium-isotoporum-medii-aevi | xlsx |
| 5 | cima-compendium-isotoporum-medii-aevi | docx |
| 6 | cima-compendium-isotoporum-medii-aevi | rdata |
| 7 | cima-compendium-isotoporum-medii-aevi | csv |
| 17 | eprg-jomon | xlsx |
| 1 | lives-isotopic-database | xlsx |
| 2 | neena-stable-isotopes | xlsx |
| 3 | neena-stable-isotopes | csv |
| 12 | new-zealand-radicarbon-database | perl |
| 16 | northern-hemisphere-modern-leaf-wax-ddn-alkane-dataset | xlsx |
| 13 | radiocarbon-palaeolithic-europe-database-v28 | xlsx |
| 11 | radon-radiocarbon-dates-online | csv |

## Filter Resources by pattern or File Type

To list all available resources, employ the
[`getResources()`](https://pandora-isomemo.github.io/pandora-data/reference/getResources.md)
function. Filter the output using the `network`, `repository`,
`fileType`, and `pattern` arguments. For instance, to list all CSV files
whose meta information contains the string `plants`:

``` r

resourcesPlants <- getResources(
  fileType = c("csv"),
  pattern = "plant"
  )

resourcesPlants[c("name", "format")]  %>% 
  knitr::kable()
```

| name                                         | format |
|:---------------------------------------------|:-------|
| Isotopic measurements in CSV format          | csv    |
| Metadata description CSV                     | csv    |
| ADS_database_metadata_template_LV.csv        | csv    |
| CIMA Animals 29.05.2021 CSV                  | csv    |
| CIMA Humans 29.05.2021 CSV                   | csv    |
| CIMA Plants 29.05.2021 CSV                   | csv    |
| Combined CIMA CSV 29.05.2021                 | csv    |
| IsoChina human dataset in CSV format         | csv    |
| IsoMad Modern Biological Material v1         | csv    |
| IsoMedIta Animals 21-12-22 - CSV             | csv    |
| IsoMedIta Humans 21-12-22 - CSV              | csv    |
| IsoMedIta Plants 21-12-22 - CSV              | csv    |
| Isotòpia Animals (2024) csv                  | csv    |
| Isotòpia Humans (2024) csv                   | csv    |
| Isotòpia Plants (2024) csv                   | csv    |
| MAIA Animals CSV                             | csv    |
| MAIA Humans CSV                              | csv    |
| MAIA Plants CSV                              | csv    |
| Animal isotopic data CSV                     | csv    |
| Human isotopic data CSV                      | csv    |
| Plant isotopic data CSV                      | csv    |
| NEENA_Animals-v1.csv                         | csv    |
| NEENA_Humans-v1.csv                          | csv    |
| NEENA_Plants-v1.csv                          | csv    |
| PleIStO_ArchAID_Fauna_V.1.csv                | csv    |
| PleIStO_ArchAID_Hominin_V.1.csv              | csv    |
| saaid_V.2.0_2023_Animals.csv                 | csv    |
| saaid_V.2.0_2023_Humans.csv                  | csv    |
| saaid_V.2.0_2023_Plants.csv                  | csv    |
| OAPID 25.5.25                                | csv    |
| Wanyika chronological database in CSV format | csv    |
| Zanadamu CSV format                          | csv    |

## Get the data of a Specific Resource

To retrieve data from a specific resource, use the
[`getData()`](https://pandora-isomemo.github.io/pandora-data/reference/getData.md)
function. For text files one can specify e.g. the separator using the
helper function
[`dataOptions()`](https://pandora-isomemo.github.io/pandora-data/reference/dataOptions.md).
The example below demonstrates fetching data from the
`"CIMA Humans 29.05.2021 CSV"` resource:

``` r

isotopicData <- getData(
  name = "CIMA Animals 29.05.2021 CSV",
  options = dataOptions(sep = ";")) 
#> Encoding: 'UTF-8'.
isotopicData  %>% 
  head(5)  %>% 
  dplyr::select(c("Entry_ID", "General_Category_Family", "Common_Name", "Sampled_Element", "Analysed_Component")) %>%
  knitr::kable()
```

| Entry_ID | General_Category_Family | Common_Name | Sampled_Element | Analysed_Component |
|---:|:---|:---|:---|:---|
| 1 | Equid | Horse | Femur | Collagen |
| 2 | Cricetid | Vole | Cranium | Collagen |
| 3 | Cricetid | Vole | Mandible | Collagen |
| 4 | Moronid | European Bass | Bone | Collagen |
| 5 | Moronid | European Bass | Bone | Collagen |
