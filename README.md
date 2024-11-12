
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
## Skipping install of 'irwpkg' from a github remote, the SHA1 (f4611de3) has not changed since last install.
##   Use `force = TRUE` to force installation
```

## Usage

To use `irwpkg`, load the package using:

``` r
library(irwpkg)
```

With the package now loaded, we can use the `list_available_datasets()`
function to view a list of available datasets. We can then use the
`fetch_data(example_data)` function to fetch a dataset named
`example_data` from the IRW database into a dataframe.

### View list of datasets

We can also view a list of data sets available for fetching from the IRW
using the `list_available_datasets()` function:

``` r
# List available datasets
list_available_datasets()
```

### Fetch data

For example, to fetch the `abortion` dataset, we can use:

``` r
# Fetch a dataset by name
df <- fetch_data("abortion")
dim(df)
head(df)
```
