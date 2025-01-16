
# irwpkg

`irwpkg` is an R package designed to simplify access to datasets from
the Item Response Warehouse ([IRW](https://datapages.github.io/irw/)).
It provides tools for exploring, filtering, retrieving, and reformatting
datasets, making it easier to analyze item response data.

## Installation

### Prerequisites
To install `irwpkg` with vignettes, you must have the Quarto CLI installed on your system.

1. **Check if Quarto is installed:**
Run the following command in your terminal:
```bash
quarto --version
```
If Quarto is not installed, download and install it [here](https://quarto.org/docs/get-started/).

2. **Install the Quarto R Package:**
Open R and run:
```r
install.packages("quarto")
```

### Install the Package
To install the development version of `irwpkg` from GitHub:
``` r
# Install the remotes package if needed
# install.packages("remotes")

# Install irwpkg from GitHub
remotes::install_github("hansorlee/irwpkg", build_vignettes = TRUE)
```

If you don't need vignettes, you can install the package without them:
``` r
remotes::install_github("hansorlee/irwpkg", build_vignettes = FALSE)
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

If the vignette does not appear, reinstall the package with vignettes enabled:
```r
remotes::install_github("hansorlee/irwpkg", build_vignettes = TRUE, force = TRUE)
```

## Troubleshooting

#### Installation Issues
- Error: vignette builder 'quarto' not found:
Make sure you have the Quarto CLI installed: [Install Quarto](https://quarto.org/docs/get-started/)

- Missing vignette after installation:
If the vignette does not appear when running `vignette(package = "irwpkg")`, ensure that the package was installed with vignettes enabled.
``` r
remotes::install_github("hansorlee/irwpkg", build_vignettes = TRUE)
```

#### Authentication Issues
- No Browser Pop-up:
If the browser window doesn't open automatically, check your pop-up blocker settings.

- Authentication errors:
If you see an authentication error, try clearing your browser cookies and attempting again.

## Feedback and Contributions
If you encounter issues or have suggestions for improving `irwpkg`, please submit them on the [GitHub Issues page](https://github.com/hansorlee/irwpkg/issues). Contributions are welcome!

