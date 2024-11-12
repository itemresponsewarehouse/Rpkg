
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

table <- fetch_table()
```

### Download data

``` r
table = fetch_table("abortion")
download_data(table, path = "abortion_data.csv")
```
