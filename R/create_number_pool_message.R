#' Send a message using number pool
#'
#' Send a message using number pool.
#'
#' @param messaging_profile_id string Unique identifier for a messaging profile.
#' @param to string (address) Receiving address (+18665552367 formatted phone number or short code).
#'   Example: "+18665552367"
#' @param text string Message body (i.e., content) as a non-empty string. **Required for SMS**
#' @param media_urls array of string A list of media URLs. The total media size must be less than
#'   1 MB. **Required for MMS**
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
create_number_pool_message <- function(messaging_profile_id,
                                       to,
                                       text = NULL,
                                       media_urls = NULL,
                                       subject = NULL,
                                       auto_detect = FALSE,
                                       type = NULL,
                                       use_profile_webhooks = TRUE,
                                       webhook_failover_url = NULL,
                                       webhook_url = NULL) {
  endpoint <- "messages/number_pool?"
  post_body <- list(
    messaging_profile_id = messaging_profile_id,
    to = to,
    text = text,
    media_urls = media_urls,
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
