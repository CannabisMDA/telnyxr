#' List available phone numbers
#'
#' List available phone numbers to buy.
#'
#' @param filter_phone_number_starts_with string Filter numbers starting with a pattern (exclude
#'   NDC from start of this filter if used with `national_destination_code` filter).
#' @param filter_phone_number_ends_with string Filter numbers ending with a pattern (max length of
#'   4 digits if used with `national_destination_code` filter).
#' @param filter_phone_number_contains string Filter numbers containing a pattern (must be used
#'   with `national_destination_code` filter and only searches within last 4 digits).
#' @param filter_locality string Filter phone numbers by city.
#' @param filter_administrative_area string Filter phone numbers by US state/CA province.
#'   Example: "IL"
#' @param filter_country_code string Filter phone numbers by ISO alpha-2 country code. Example: "US"
#' @param filter_national_destination_code string Filter by the national destination code of the
#'   number. This filter is only applicable to North American numbers.
#' @param filter_rate_center string Filter phone numbers by NANP rate center. This filter is only
#'   applicable to North American numbers. Example: "CHICAGO HEIGHTS"
#' @param filter_phone_number_type string Filter phone numbers by number type.
#'  Options: ["local", "toll_free", "mobile", "national", "shared_cost", "landline"]
#' @param filter_features array of string Filter if the phone number should be used for voice, fax,
#'   mms, sms, emergency. Options: ["sms", "mms", "voice", "fax", "emergency"]
#' @param filter_limit integer Limits the number of results. Example: 100
#' @param filter_best_effort boolean Filter to determine if best effort results should be included.
#'   Only available in NANPA countries.
#' @param filter_quickship boolean Filter to exclude phone numbers that need additional time after
#'   to purchase to receive phone calls. Only available for toll-free numbers.
#' @param filter_reservable boolean Filter to exclude phone numbers that cannot be reserved before
#'   purchase.
#' @param filter_exclude_held_numbers boolean Filter to exclude phone numbers that are currently on
#'   hold for your account.
#'
#' @importFrom dplyr as_tibble
#' @importFrom glue glue
#' @importFrom httr add_headers content GET
#' @importFrom jsonlite fromJSON
#'
#' @export
#'
list_available_phone_numbers <- function(filter_phone_number_starts_with = NULL,
                                         filter_phone_number_ends_with = NULL,
                                         filter_phone_number_contains = NULL,
                                         filter_locality = NULL,
                                         filter_administrative_area = NULL,
                                         filter_country_code = NULL,
                                         filter_national_destination_code = NULL,
                                         filter_rate_center = NULL,
                                         filter_phone_number_type = NULL,
                                         filter_features = NULL,
                                         filter_limit = NULL,
                                         filter_best_effort = NULL,
                                         filter_quickship = NULL,
                                         filter_reservable = NULL,
                                         filter_exclude_held_numbers = NULL) {
  endpoint <- "available_phone_numbers?"
  filter_phone_number_starts_with <- ifelse(
    is.null(filter_phone_number_starts_with),
    "",
    glue("filter[phone_number][starts_with]={filter_phone_number_starts_with}&")
  )
  filter_phone_number_ends_with <- ifelse(
    is.null(filter_phone_number_ends_with),
    "",
    glue("filter[phone_number][ends_with]={filter_phone_number_ends_with}&")
  )
  filter_phone_number_contains <- ifelse(
    is.null(filter_phone_number_contains),
    "",
    glue("filter[phone_number][contains]={filter_phone_number_contains}&")
  )
  filter_locality <- ifelse(
    is.null(filter_locality), "", glue("filter[locality]={filter_locality}&")
  )
  filter_administrative_area <- ifelse(
    is.null(filter_administrative_area),
    "",
    glue("filter[administrative_area]={filter_administrative_area}&")
  )
  filter_country_code <- ifelse(
    is.null(filter_country_code), "", glue("filter[country_code]={filter_country_code}&")
  )
  filter_national_destination_code <- ifelse(
    is.null(filter_national_destination_code),
    "",
    glue("filter[national_destination_code]={filter_national_destination_code}&")
  )
  filter_rate_center <- ifelse(
    is.null(filter_rate_center), "", glue("filter[rate_center]={filter_rate_center}&")
  )
  filter_phone_number_type <- ifelse(
    is.null(filter_phone_number_type),
    "",
    glue("filter[phone_number_type]={filter_phone_number_type}&")
  )
  filter_features <- ifelse(
    is.null(filter_features),
    "",
    paste0(glue("filter[features][]={filter_features}&"), collapse = "")
  )
  filter_limit <- ifelse(is.null(filter_limit), "", glue("filter[limit]={filter_limit}&"))
  filter_best_effort <- ifelse(
    is.null(filter_best_effort), "", glue("filter[best_effort]={tolower(filter_best_effort)}&")
  )
  filter_quickship <- ifelse(
    is.null(filter_quickship), "", glue("filter[quickship]={tolower(filter_quickship)}&")
  )
  filter_reservable <- ifelse(
    is.null(filter_reservable), "", glue("filter[reservable]={tolower(filter_reservable)}&")
  )
  filter_exclude_held_numbers <- ifelse(
    is.null(filter_exclude_held_numbers),
    "",
    glue("filter[exclude_held_numbers]={tolower(filter_exclude_held_numbers)}&")
  )
  res <- GET(
    glue(
      "{api_url()}{endpoint}{filter_phone_number_starts_with}{filter_phone_number_ends_with}",
      "{filter_phone_number_contains}{filter_locality}{filter_administrative_area}",
      "{filter_country_code}{filter_national_destination_code}{filter_rate_center}",
      "{filter_phone_number_type}{filter_features}{filter_limit}{filter_best_effort}",
      "{filter_quickship}{filter_reservable}{filter_exclude_held_numbers}"
    ),
    add_headers(Authorization = paste0("Bearer ", token()))
  )
  if (res$status_code != 200) {
    return(res)
  }
  res <- fromJSON(content(res, as = "text"))
  as_tibble(res$data)
}
