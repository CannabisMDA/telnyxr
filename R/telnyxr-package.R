#' `{telnyxr}`: An Interface to the Telnyx API for R
#'
#' `{telnyxr}` provides users a range of functions designed to communicate with Telnyx API from R.
#'
"_PACKAGE"

.state <- new.env(parent = emptyenv())

# Load Telnyx API tokens from environment variable.
assign("token", Sys.getenv("TELNYX_TOKEN"), envir = .state)

api_url <- function() "https://api.telnyx.com/v2/"
