
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rspacer

<!-- badges: start -->
<!-- badges: end -->

The goal of rspacer is to wrap the [RSpace
API](https://community.researchspace.com/public/apiDocs).

## Installation

You can install the development version of rspacer like so:

``` r
remotes::install_github("burgerga/rspacer")
```

## Usage

Add the environment files `RSPACE_API_URL` and `RSPACE_API_KEY` to your
`.Renviron` file (you can do so using the `usethis::edit_r_environ()`
command). Your `.Renviron` should contain the following lines:

    RSPACE_API_URL="https://leiden.researchspace.com/api/v1"
    RSPACE_API_KEY="<YOUR_API_KEY_HERE>"

The `RSPACE_API_URL` is typically the URL of your RSpace instance
followed by `api/v1`. To get your API key see the instructions in the
**Authentication with an API key** section at
<https://community.researchspace.com/public/apiDocs>.  
**Keep your API key private!**

After restarting R, you should now be able to run

``` r
library(rspacer)
api_status()
#> $message
#> [1] "OK"
#> 
#> $rspaceVersion
#> [1] "1.96.4"
```

Now you can explore the functionality of `rspacer`.
