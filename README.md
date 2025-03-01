# irwpkg

`irwpkg` makes it easy to access and work with tables from the [Item Response Warehouse (IRW)](https://datapages.github.io/irw/), an open repository of harmonized item response data.

Visit the [irwpkg package website](https://hansorlee.github.io/irwpkg/index.html) for more information on getting started and a complete list of available functions.

## Installation

``` r
# Install the development version of irwpkg from GitHub
devtools::install_github("hansorlee/irwpkg")
```

## IMPORTANT: Redivis Authentication

The IRW tables are hosted on [Redivis](https://redivis.com), a data management platform. To access these datasets, you'll need to:

1.  Have a Redivis account (create one at <https://redivis.com/?createAccount> if you don't have one).

2.  Authenticate using the Redivis R Client:

    1.  When you first use a function in `irwpkg` that connects to Redivis (e.g. `irw_info()`), a browser window will open, prompting you to sign in to your Redivis account.

    2.  After signing in, click **Allow** to grant access for the Redivis R Client.

    3.  Once authentication is successful, close the browser window. You will see the message “Authentication was successful” in the R console.

**Note:** You only need to authenticate once per session. For detailed instructions, refer to the [Redivis R Client documentation](https://apidocs.redivis.com/client-libraries/redivis-r/getting-started).

## Usage

After installation, load the `irwpkg` in your R session:

``` r
library(irwpkg)
```

### Example

``` r
# Get information about the IRW
irw_info() 

# View list of available tables in IRW
irw_list_tables()

# Filter tables containing the variable `rt`
irw_filter(var = "rt")
```

``` r
# fetch an IRW table
x = irw_fetch("4thgrade_math_sirt") 
```

## Troubleshooting

#### Authentication Issues

-   No Browser Pop-up: If the browser window doesn't open automatically, check your pop-up blocker settings.

-   Authentication errors: If you see an authentication error, try clearing your browser cookies and attempting again.

## Feedback and Contributions

If you encounter issues or have suggestions for improving `irwpkg`, please submit them on the [GitHub Issues page](https://github.com/hansorlee/irwpkg/issues). Contributions are welcome!
