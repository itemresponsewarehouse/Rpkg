
# irwpkg

`irwpkg` is an R package designed to simplify access to datasets from
the Item Response Warehouse ([IRW](https://datapages.github.io/irw/)).
It provides tools for exploring, filtering, retrieving, and reformatting
datasets, making it easier to analyze item response data.

## Installation
To install the development version of `irwpkg` from GitHub:
``` r
# Install the remotes package if needed
# install.packages("remotes")

# Install irwpkg from GitHub
remotes::install_github("hansorlee/irwpkg")
```

## Usage

### Load the Package
After installation, load the `irwpkg` in your R session:
``` r
library(irwpkg)
```

### Authentication

The IRW datasets are hosted on [Redivis](https://redivis.com), a data management platform. To access these datasets, you'll need to:

1. Have a Redivis account (create one at <https://redivis.com/?createAccount>  if you don't have one).

2. Complete a one-time authentication process using the Redivis R Client requiring a pop-up.

### Steps for Authentication

1.  When you first use a function in `irwpkg` that connects to Redivis
    (e.g., `list_available_datasets()`), a browser window will open,
    prompting you to sign in to your Redivis account.

2.  After signing in, allow access for the Redivis R Client by clicking
    **Allow**.

3.  Once authentication is successful, close the browser window. You
    will see the message “Authentication was successful” in the R
    console.

- **Note:** You only need to authenticate once per session.

- For detailed instructions, refer to the [Redivis R Client
  documentation](https://apidocs.redivis.com/client-libraries/redivis-r/getting-started).


## Vignettes

`irwpkg` includes a vignette to help you get started. To access it, run:

``` r
# List all available vignettes for irwpkg
vignette(package = "irwpkg")

# Open the main vignette titled "Report"
vignette("Report", package = "irwpkg")
```

## Troubleshooting

#### Authentication Issues
- No Browser Pop-up:
If the browser window doesn't open automatically, check your pop-up blocker settings.

- Authentication errors:
If you see an authentication error, try clearing your browser cookies and attempting again.

## Feedback and Contributions
If you encounter issues or have suggestions for improving `irwpkg`, please submit them on the [GitHub Issues page](https://github.com/hansorlee/irwpkg/issues). Contributions are welcome!

