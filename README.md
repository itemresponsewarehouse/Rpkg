
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
list_available_datasets()
```

### Fetch data

Once you have identified a dataset you want, you can use `fetch_data()`
to load it as a data frame in R. For example, to fetch the `abortion`
dataset, you can use:

``` r
# Fetch a dataset by name
df <- fetch_data(name="abortion")
dim(df)
head(df)
```

### Download data

``` r
table = fetch_table("abortion")
download_data(table, path = "abortion_data.csv")
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
## Active Data Size (GB):     144.95 GB
## DOI:                       https://doi.org/10.57761/k50h-t692 
## Dataset URL:               https://redivis.com/datasets/as2e-cv7jb41fd?v=11.18 
## Documentation:             https://datapages.github.io/irw/ 
## Methodology:               Tables have been harmonized as per details given [here](<https://datapages.github.io/irw/standard.html>).
## 
## Usage Information:         Please find information about data licenses and citation info [here](<https://datapages.github.io/irw/docs.html>).
## 
##  
## --------------------------------------------------
table_info = get_table_metadata("abortion")
## Table Metadata for: abortion 
## --------------------------------------------------
## Name:                      abortion 
## Created At:                2024-11-12 04:25:18 
## Last Updated At:           2024-11-12 11:29:32 
## Number of Rows:            1516 
## Data Size (KB):            35.11 KB
## Variable Count:            3 
## Is Sample:                 No 
## DOI:                       https://doi.org/10.57761/k50h-t692 
## Table URL:                 https://redivis.com/datasets/as2e-cv7jb41fd/tables/c27h-dq63215j5?v=11.18 
## Container URL:             https://redivis.com/datasets/as2e-cv7jb41fd?v=11.18 
## --------------------------------------------------
```
