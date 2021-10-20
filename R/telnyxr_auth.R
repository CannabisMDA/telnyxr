#' Set Telnyx's Credentials
#'
#' @param token Telnyx's token.
#'
#' @export
#'
telnyxr_auth <- function(token = Sys.getenv("TELNYX_TOKEN")) {
  if (nchar(token) == 0) {
    stop("Please provide your Telnyx credentials.", call. = FALSE)
  }
  assign("token", token, envir = .state)
}
token <- function() get("token", envir = .state)
