
# irwpkg

`irwpkg` is an R package designed to simplify access to datasets from
the Item Response Warehouse ([IRW](https://datapages.github.io/irw/)).
It provides tools for exploring, filtering, retrieving, and reformatting
datasets, making it easier to analyze item response data.

<!-- ## Installation -->
<!-- To install the development version of `irwpkg` from [GitHub](https://github.com/): -->
<!-- ```{r} -->
<!-- #| eval: false -->
<!-- # Install the remotes package if needed -->
<!-- # install.packages("remotes") -->
<!-- # Install irwpkg from GitHub -->
<!-- remotes::install_github("hansorlee/irwpkg") -->
<!-- ``` -->

## Loading the Package

After installation, load the `irwpkg` package:

``` r
library(irwpkg)
```

## Authentication

The IRW datasets are hosted on [Redivis](https://redivis.com), a data management platform. To access these datasets, you'll need to:

1. Have a Redivis account (create one at <https://redivis.com/?createAccount>  if you don't have one).

2. Complete a one-time authentication process using the Redivis R Client requiring a pop-up.

### Steps for Authentification

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


### Troubleshooting

#### No Browser 

If the browser window doesn't open automatically, check your pop-up blocker settings.

#### Authentication errors

If you see an authentication error, try clearing your browser cookies and attempting again.

## Viewing the Vignette

To help you get started, irwpkg includes a vignette for a tutorial on
how to use this package. You can access the vignette using the following
commands:

``` r
# List all available vignettes for irwpkg
vignette(package = "irwpkg")

# Open the main vignette titled "Report"
vignette("Report", package = "irwpkg")
```

<!-- #### Troubleshooting -->
<!-- If the vignette does not appear, ensure that the package was installed with vignettes enabled. You can do this by reinstalling the package: -->
<!-- ```{r} -->
<!-- #| eval: false -->
<!-- # remotes::install_github("hansorlee/irwpkg", build_vignettes = TRUE) -->
<!-- ``` -->

### Troubleshooting


#### Missing Vignette

If the vignette does not appear when running
`vignette(package = "irwpkg")`, ensure that the package was installed
with vignettes enabled. Developers can reinstall the package with the
following command:

``` r
devtools::install(build_vignettes = TRUE)
```

<!-- Please feel free to suggest additional features or report issues on the repository’s GitHub Issues page! -->
