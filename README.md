
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
## Downloading GitHub repo hansorlee/irwpkg@HEAD
## ── R CMD build ─────────────────────────────────────────────────────────────────
##      checking for file ‘/private/var/folders/2y/42rvvshx25925ssnn_4nj2h40000gn/T/RtmpxXFyDw/remotesdfc0799d160a/hansorlee-irwpkg-7ae6d9d/DESCRIPTION’ ...  ✔  checking for file ‘/private/var/folders/2y/42rvvshx25925ssnn_4nj2h40000gn/T/RtmpxXFyDw/remotesdfc0799d160a/hansorlee-irwpkg-7ae6d9d/DESCRIPTION’
##   ─  preparing ‘irwpkg’:
##      checking DESCRIPTION meta-information ...  ✔  checking DESCRIPTION meta-information
##   ─  checking for LF line-endings in source and make files and shell scripts
##   ─  checking for empty or unneeded directories
##    Omitted ‘LazyData’ from DESCRIPTION
##   ─  building ‘irwpkg_0.1.0.tar.gz’
##      
## 
```

## Usage

To use `irwpkg`, load the package using:

``` r
library(irwpkg)
```

With the package now loaded, we can download a dataset using the
`fetch_irw()` function or use the `list_available_datasets()` function
to view a list of available datasets.

### Download data

For example, to download the `pisa` dataset, we can use:

``` r
# Fetch a dataset by name
pisa_df <- fetch_irw(name = "pisa")
names(pisa_df)
```

### View list of datasets

We can also view a list of data sets available for download using the
`list_available_datasets()` function:

``` r
# List available datasets
list_available_datasets()
```
