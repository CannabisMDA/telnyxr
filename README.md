
<!-- README.md is generated from README.Rmd. Please edit that file -->

# telnyxr - An Interface to the Telnyx API for R

<!-- badges: start -->
<!-- badges: end -->

`{telnyxr}` provides users a range of functions designed to communicate
with Telnyx API from R.

## Installation

You can install the development version from GitHub:

    if (!require("remotes")) {
      install.packages("remotes")
    }
    remotes::install_github("CannabisMDA/telnyxr", dependencies = TRUE)

## Example

### Set Up Authentication

You should only need to do this once per session.

``` r
library("telnyxr")

# Using environment variable.
Sys.setenv(TELNYX_TOKEN = "KEY4Ozq8BFX94w5St5hikg7UV0lPpH8e56M9W4Ozq8BFX94w5St5hikg7U")
telnyxr_auth()

# Or providing the token.
telnyxr_auth("KEY4Ozq8BFX94w5St5hikg7UV0lPpH8e56M9W4Ozq8BFX94w5St5hikg7U")
```

### Send a Text Message

``` r
create_message(to = "+12125557634", text = "Hello from R 👋", from = "+19178675903")
```
