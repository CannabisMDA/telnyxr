#' List phone numbers
#'
#' List phone numbers.
#'
#' @param page_number integer (1) The page number to load.
#' @param page_size integer (1 - 250) The size of the page.
#' @param filter_tag string Filter by phone number tags.
#' @param filter_phone_number string Filter by phone number. Requires at least three digits.
#'   Non-numerical characters will result in no values being returned.
#' @param filter_status string Filter by phone number status. Options: ["purchase_pending",
#'   "purchase_failed", "port_pending", "active", "deleted", "port_failed", "emergency_only",
#'   "ported_out", "port_out_pending"]
#' @param filter_voice_connection_name_contains string Filter contains connection name. Requires at
#'   least three characters. Example: "test"
#' @param filter_voice_connection_name_starts_with string Filter starts with connection name.
#'   Requires at least three characters. Example: "test"
#' @param filter_voice_connection_name_ends_with string Filter ends with connection name. Requires
#'   at least three characters. Example: "test"
#' @param filter_voice_connection_name_eq string Filter by connection name. Example: "test"
#' @param filter_usage_payment_method string Filter by usage_payment_method.
#'   Options: ["pay-per-minute", "channel"]
#' @param filter_billing_group_id string Filter by the billing_group_id associated with phone
#'   numbers. To filter to only phone numbers that have no billing group associated them, set the
#'   value of this filter to the string 'null'. Example: "62e4bf2e-c278-4282-b524-488d9c9c43b2"
#' @param filter_emergency_address_id string (int64) Filter by the emergency_address_id associated
#'   with phone numbers. To filter only phone numbers that have no emergency address associated
#'   with them, set the value of this filter to the string 'null'. Example: "9102160989215728032"
#' @param filter_customer_reference string Filter numbers via the customer_reference set.
#' @param sort string Specifies the sort order for results. If not given, results are sorted by
#'   created_at in descending order. Options: ["purchased_at", "phone_number", "connection_name",
#'   "usage_payment_method"]
#'
#' @importFrom dplyr as_tibble
#' @importFrom glue glue
#' @importFrom httr add_headers content GET
#' @importFrom jsonlite fromJSON
#'
#' @export
#'
list_phone_numbers <- function(
  page_number = 1,
  page_size = 20,
  filter_tag = NULL,
  filter_phone_number = NULL,
  filter_status = NULL,
  filter_voice_connection_name_contains = NULL,
  filter_voice_connection_name_starts_with = NULL,
  filter_voice_connection_name_ends_with = NULL,
  filter_voice_connection_name_eq = NULL,
  filter_usage_payment_method = NULL,
  filter_billing_group_id = NULL,
  filter_emergency_address_id = NULL,
  filter_customer_reference = NULL,
  sort = NULL
) {
  endpoint <- "phone_numbers?"
  page_number <- glue("page[number]={page_number}&")
  page_size <- glue("page[size]={page_size}&")
  filter_tag <- ifelse(is.null(filter_tag), "", glue("filter[tag]={filter_tag}&"))
  filter_phone_number <- ifelse(
    is.null(filter_phone_number), "", glue("filter[phone_number]={filter_phone_number}&")
  )
  filter_status <- ifelse(is.null(filter_status), "", glue("filter[status]={filter_status}&"))
  filter_voice_connection_name_contains <- ifelse(
    is.null(filter_voice_connection_name_contains),
    "",
    glue("filter[voice.connection_name][contains]={filter_voice_connection_name_contains}&")
  )
  filter_voice_connection_name_starts_with <- ifelse(
    is.null(filter_voice_connection_name_starts_with),
    "",
    glue("filter[voice.connection_name][starts_with]={filter_voice_connection_name_starts_with}&")
  )
  filter_voice_connection_name_ends_with <- ifelse(
    is.null(filter_voice_connection_name_ends_with),
    "",
    glue("filter[voice.connection_name][ends_with]={filter_voice_connection_name_ends_with}&")
  )
  filter_voice_connection_name_eq <- ifelse(
    is.null(filter_voice_connection_name_eq),
    "",
    glue("filter[voice.connection_name][eq]={filter_voice_connection_name_eq}&")
  )
  filter_usage_payment_method <- ifelse(
    is.null(filter_usage_payment_method),
    "",
    glue("filter[usage_payment_method]={filter_usage_payment_method}&")
  )
  filter_billing_group_id <- ifelse(
    is.null(filter_billing_group_id),
    "",
    glue("filter[billing_group_id]={filter_billing_group_id}&")
  )
  filter_emergency_address_id <- ifelse(
    is.null(filter_emergency_address_id),
    "",
    glue("filter[emergency_address_id]={filter_emergency_address_id}&")
  )
  filter_customer_reference <- ifelse(
    is.null(filter_customer_reference),
    "",
    glue("filter[customer_reference]={filter_customer_reference}&")
  )
  sort <- ifelse(is.null(sort), "", glue("sort={sort}&"))
  res <- GET(
    glue(
      "{api_url()}{endpoint}{page_number}{page_size}{filter_tag}{filter_phone_number}",
      "{filter_status}{filter_voice_connection_name_contains}",
      "{filter_voice_connection_name_starts_with}{filter_voice_connection_name_ends_with}",
      "{filter_voice_connection_name_eq}{filter_usage_payment_method}{filter_billing_group_id}",
      "{filter_emergency_address_id}{filter_customer_reference}{sort}"
    ),
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
