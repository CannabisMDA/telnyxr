#' Send a message
#'
#' Send a message with a Phone Number, Alphanumeric Sender ID, Short Code or Number Pool.
#' This endpoint allows you to send a message with any messaging resource. Current messaging
#' resources include: long-code, short-code, number-pool, and alphanumeric-sender-id.
#'
#' @param to string (address) Receiving address (+18665552367 formatted phone number or short code).
#'   Example: "+18665552367"
#' @param text string Message body (i.e., content) as a non-empty string. **Required for SMS**
#' @param from string (address) Sending address (+18665552367 formatted phone number, alphanumeric
#'   sender ID, or short code). **Required if sending with a phone number, short code, or
#'   alphanumeric sender ID.**
#' @param media_urls array of string A list of media URLs. The total media size must be less than
#'   1 MB. **Required for MMS**
#' @param messaging_profile_id string Unique identifier for a messaging profile. **Required if
#'   sending via number pool or with an alphanumeric sender ID.**
#' @param subject string Subject of multimedia message.
#' @param auto_detect boolean Automatically detect if an SMS message is unusually long and exceeds
#'   a recommended limit of message parts.
#' @param type string The protocol for sending the message, either SMS or MMS.
#'   Options: ["SMS", "MMS"]
#' @param use_profile_webhooks boolean If the profile this number is associated with has webhooks,
#'   use them for delivery notifications. If webhooks are also specified on the message itself,
#'   they will be attempted first, then those on the profile.
#' @param webhook_failover_url string (url) The failover URL where webhooks related to this message
#'   will be sent if sending to the primary URL fails.
#' @param webhook_url string (url) The URL where webhooks related to this message will be sent.
#'
#' @importFrom dplyr as_tibble
#' @importFrom glue glue
#' @importFrom httr add_headers content content_type_json POST
#' @importFrom jsonlite fromJSON toJSON
#'
#' @export
#'
create_message <- function(
  to,
  text = NULL,
  from = NULL,
  media_urls = NULL,
  messaging_profile_id = NULL,
  subject = NULL,
  auto_detect = FALSE,
  type = NULL,
  use_profile_webhooks = TRUE,
  webhook_failover_url = NULL,
  webhook_url = NULL
) {
  endpoint <- "messages?"
  post_body <- list(
    to = to,
    text = text,
    from = from,
    media_urls = media_urls,
    messaging_profile_id = messaging_profile_id,
    subject = subject,
    auto_detect = auto_detect,
    type = type,
    use_profile_webhooks = use_profile_webhooks,
    webhook_failover_url = webhook_failover_url,
    webhook_url = webhook_url
  )
  post_body <- post_body[!unlist(lapply(post_body, is.null))]
  res <- POST(
    glue("{api_url()}{endpoint}"),
    body = toJSON(post_body, auto_unbox = TRUE),
    add_headers(Authorization = paste0("Bearer ", token())),
    content_type_json()
  )
  if (res$status_code != 200) {
    return(res)
  }
  res <- fromJSON(content(res, as = "text"))
  as_tibble(res)
}
