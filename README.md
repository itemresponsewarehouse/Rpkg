
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

### Authentication

The IRW datasets are hosted on [Redivis](https://redivis.com), so to
access these datasets through `irwpkg`, you’ll need to authenticate with
your Redivis account. This connection is managed through the Redivis R
client.

**Redivis Authentication Setup**

1.  When you first use a function in `irwpkg` that connects to Redivis
    (e.g. see `list_available_datasets()` below), a browser window will
    pop up prompting you to sign into Redivis.

2.  After signing in, you’ll be prompted to allow access for the Redivis
    R Client. Click **Allow**.

3.  Once you’ve granted access, close the browser window. You’ll see the
    message “Authentication was successful” in the R console, and you
    can then re-run your `irwpkg` function.

- **Note:** Authentication typically only needs to be done once per
  session.
- For more details, refer to [Redivis R Client
  documentation](https://apidocs.redivis.com/client-libraries/redivis-r/getting-started).

### View list of datasets

The list_available_datasets() function provides a list of datasets in
the IRW database, showing key metadata (dataset name, row count, and
variable count).

``` r
# List available datasets
list_datasets = list_available_datasets()
dim(list_datasets)
## [1] 558   3
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
## Version:                   v11.19 
## Table Count:               558 
## Created At:                2023-09-25 19:19:15 
## Last Updated At:           2024-11-14 16:31:26 
## Total Data Size (GB):      145.1 GB
## Active Data Size (GB):     145.01 GB
## DOI:                       https://doi.org/10.57761/4zaa-w743 
## Dataset URL:               https://redivis.com/datasets/as2e-cv7jb41fd?v=11.19 
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
## Created At:                2024-11-14 16:25:32 
## Last Updated At:           2024-11-14 16:31:26 
## Number of Rows:            7020 
## Data Size (KB):            141.86 KB
## Variable Count:            3 
## Is Sample:                 No 
## DOI:                       https://doi.org/10.57761/4zaa-w743 
## Table URL:                 https://redivis.com/datasets/as2e-cv7jb41fd/tables/018s-9p87q43mh?v=11.19 
## Container URL:             https://redivis.com/datasets/as2e-cv7jb41fd?v=11.19 
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

# get tables with columns "rater" and "rt"
filter_tables(required_columns=c("rater", "rt"))
## [1] "famous_melodies"
```

### Fetch data

#### Fetch a single dataset

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

#### Fetch multiple datasets

You can also fetch multiple datasets at once.

``` r
matching_tables <- filter_tables(n_rows = 50000, required_columns = c("rater"))
print(matching_tables)
## [1] "fractals_rating"                "spelling2pronounce_edwards2023"

datasets <- fetch_data(c("fractals_rating", "spelling2pronounce_edwards2023"))
print(names(datasets)) # datasets is a named list
## [1] "fractals_rating"                "spelling2pronounce_edwards2023"
```

You can also use the output of `filter_tables()` directly to
`fetch_data()`.

``` r
datasets <- fetch_data(matching_tables)
print(names(datasets))
## [1] "fractals_rating"                "spelling2pronounce_edwards2023"

fractals_rating <- datasets[["fractals_rating"]]
head(fractals_rating)
##            id            item rater resp
## 1 Fractal_001      liveliness  1012    1
## 2 Fractal_001 verbalizability  1019    1
## 3 Fractal_001  favourableness  1007    1
## 4 Fractal_001     familiarity  1009    1
## 5 Fractal_001     familiarity  1019    1
## 6 Fractal_001 verbalizability  1007    1
```

### Download data

``` r
download_data("swmd_mokken", path = "mydata.csv")
```

### More coming soon :)
