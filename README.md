<!-- badges: start -->

[![CRAN Status](https://www.r-pkg.org/badges/version/irwpkg)](https://CRAN.R-project.org/package=irwpkg) [![R-CMD-check](https://github.com/hansorlee/irwpkg/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/hansorlee/irwpkg/actions/workflows/R-CMD-check.yaml) [![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://opensource.org/licenses/MIT)

<!-- badges: end -->

# irwpkg

`irwpkg` makes it easy to access and work with datasets from the [Item Response Warehouse (IRW)](https://datapages.github.io/irw/).

## Installation

You can install `irwpkg` from CRAN using:

``` r
install.packages("irwpkg")
```

## Required Dependency: redivis

`irwpkg` depends on the `redivis` package to access datasets hosted on [Redivis](https://redivis.com). Before using any `irwpkg` functions, you must:

1.  Install the `redivis` package
2.  Authenticate with your Redivis account

Follow the steps below to set up `redivis` and start using `irwpkg`.

### Installing redivis

Since `redivis` is not available on CRAN, you can install it directly from its GitHub repository:

``` r
devtools::install_github('redivis/redivis-r', ref='main')
```

### Redivis Authentication

After installing `redivis`, follow these steps to authenticate your Redivis account:

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
# Check IRW metadata
irw_info()
```

## Troubleshooting

#### Authentication Issues

-   No Browser Pop-up: If the browser window doesn't open automatically, check your pop-up blocker settings.

-   Authentication errors: If you see an authentication error, try clearing your browser cookies and attempting again.

## Feedback and Contributions

If you encounter issues or have suggestions for improving `irwpkg`, please submit them on the [GitHub Issues page](https://github.com/hansorlee/irwpkg/issues). Contributions are welcome!
