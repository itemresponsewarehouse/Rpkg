
# irwpkg

`irwpkg` is an R package designed to facilitate easy access to datasets
from the Item Response Warehouse
([IRW](https://datapages.github.io/irw/)), offering functions to
download, explore, and analyze these datasets.

## Installation

You can install the development version of `irwpkg` from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("hansorlee/irwpkg")
```

## Usage

To use `irwpkg`, load the package using:

``` r
library(irwpkg)
```

### View list of datasets

The list_available_datasets() function provides a list of datasets in
the IRW database, showing key metadata (dataset name, row count, and
variable count).

``` r
# List available datasets
list_datasets = list_available_datasets()
dim(list_datasets)
## [1] 548   3
head(list_datasets)
##                                 name numRows variableCount
## 1                        lessR_Mach4    7020             3
## 2                  movac_pakpour2022   72096             3
## 3 SABFI2_Gallardo_Pujol_2018_IntHapp    3771             3
## 4                        science_ltm    2744             4
## 5                    gilbert_meta_17 1840581            11
## 6                    pks_probability   12096             4
```

### view DB/table metadata

``` r
db_info = get_database_metadata()
## IRW Database Metadata:
## --------------------------------------------------
## Version:                   v11.18 
## Table Count:               548 
## Created At:                2023-09-25 19:19:15 
## Last Updated At:           2024-11-12 04:30:41 
## Total Data Size (GB):      145.09 GB
## Active Data Size (GB):     145.01 GB
## DOI:                       https://doi.org/10.57761/k50h-t692 
## Dataset URL:               https://redivis.com/datasets/as2e-cv7jb41fd?v=11.18 
## Documentation:             https://datapages.github.io/irw/ 
## Methodology:               Tables have been harmonized as per details given [here](<https://datapages.github.io/irw/standard.html>).
## 
## Usage Information:         Please find information about data licenses and citation info [here](<https://datapages.github.io/irw/docs.html>).
## 
##  
## --------------------------------------------------

table_info = get_table_metadata("lessR_Mach4")
## Table Metadata for: lessR_Mach4 
## --------------------------------------------------
## Name:                      lessR_Mach4 
## Created At:                2024-11-12 04:25:18 
## Last Updated At:           2024-11-12 15:37:02 
## Number of Rows:            7020 
## Data Size (KB):            141.86 KB
## Variable Count:            3 
## Is Sample:                 No 
## DOI:                       https://doi.org/10.57761/k50h-t692 
## Table URL:                 https://redivis.com/datasets/as2e-cv7jb41fd/tables/018s-3qybpe981?v=11.18 
## Container URL:             https://redivis.com/datasets/as2e-cv7jb41fd?v=11.18 
## --------------------------------------------------
```

### filter tables by criteria

``` r
# get tables with a column named "rater"
filter_tables(required_columns="rater")
##  [1] "swmd_mokken"                    "dumas_Organisciak_2022"        
##  [3] "fractals_rating"                "mpsycho_lakes"                 
##  [5] "ptam1_immer"                    "autonomysupport_mokken"        
##  [7] "wine_luckett2021"               "spelling2pronounce_edwards2023"
##  [9] "immer12_immer"                  "famous_melodies"

# get tables with at least 10000 rows and a column named "rater"
filter_tables(n_rows = 10000, required_columns="rater")
## [1] "dumas_Organisciak_2022"         "fractals_rating"               
## [3] "mpsycho_lakes"                  "ptam1_immer"                   
## [5] "spelling2pronounce_edwards2023" "famous_melodies"
```

### Fetch data

Once you have identified a dataset you want, you can use `fetch_data()`
to load it as a data frame in R. For example, to fetch the `swmd_mokken`
dataset, you can use:

``` r
# Fetch a dataset by name
swmd_mokken <- fetch_data(name="swmd_mokken")
dim(swmd_mokken)
## [1] 4557    4
head(swmd_mokken)
##   resp  item rater      id
## 1    1 Item2    12 1000303
## 2    1 Item1    13 1000303
## 3    1 Item3    17 1001302
## 4    1 Item1    22 1001302
## 5    1 Item2    25 1001302
## 6    1 Item3    25 1001302
```

### Download data

``` r
table = fetch_table("swmd_mokken")
download_data(table, path = "swmd_mokken.csv")
```
