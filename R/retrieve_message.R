#' Retrieve a message
#'
#' Note: This API endpoint can only retrieve messages that are no older than 10 days since their
#' creation. If you require messages older than this, please generate an MDR report.
#'
#' @param id string (uuid) The id of the message.
#'
#' @importFrom dplyr as_tibble
#' @importFrom glue glue
#' @importFrom httr add_headers content GET
#' @importFrom jsonlite fromJSON
#'
#' @export
#'
retrieve_message <- function(id) {
  endpoint <- glue("messages/{id}?")
  res <- GET(glue("{api_url()}{endpoint}"), add_headers(Authorization = paste0("Bearer ", token())))
  if (res$status_code != 200) {
    return(res)
  }
  res <- fromJSON(content(res, as = "text"))
  as_tibble(res)
}
