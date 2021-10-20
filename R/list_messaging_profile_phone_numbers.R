#' List phone numbers associated with a messaging profile
#'
#' List phone numbers associated with a messaging profile.
#'
#' @param id string (uuid) The id of the messaging profile to retrieve.
#' @param page_number integer (1) The page number to load.
#' @param page_size integer (1 - 250) The size of the page.
#'
#' @importFrom dplyr as_tibble
#' @importFrom glue glue
#' @importFrom httr add_headers content GET
#' @importFrom jsonlite fromJSON
#'
#' @export
#'
list_messaging_profile_phone_numbers <- function(id, page_number = 1, page_size = 20) {
  endpoint <- glue("messaging_profiles/{id}/phone_numbers?")
  page_number <- glue("page[number]={page_number}&")
  page_size <- glue("page[size]={page_size}&")
  res <- GET(
    glue("{api_url()}{endpoint}{page_number}{page_size}"),
    add_headers(Authorization = paste0("Bearer ", token()))
  )
  if (res$status_code != 200) {
    return(res)
  }
  res <- fromJSON(content(res, as = "text"))
  if (res$meta$total_pages > 1) {
    # TODO: Handle all pages download.
    browser()
  }
  as_tibble(res$data)
}
