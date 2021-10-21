#' Fetch messaging detail records
#'
#' Fetch all Mdr records.
#'
#' @param start_date date-time Start date. Example: Sys.time()
#' @param end_date date-time End date. Example: Sys.time()
#' @param page_number integer (1) Page number.
#' @param page_size integer (1 - 50) Size of the page.
#' @param sort array of string Field used to order the data. Example: "created_at"
#' @param id string Message uuid. Example: "e093fbe0-5bde-11eb-ae93-0242ac130002"
#' @param direction string Options: ["INBOUND", "OUTBOUND"]
#' @param outbound_profile_id string Configured profile id. New profiles can be created and
#'   configured on Telnyx portal. Example: "30ef55db-c4a2-4c4a-9804-a68077973d07"
#' @param cld string Destination number. Example: "+15551237654"
#' @param cli string Origination number. Example: "+15551237654"
#' @param status string Message status. Options: ["GW_TIMEOUT", "DELIVERED", "DLR_UNCONFIRMED",
#'   "DLR_TIMEOUT", "RECEIVED", "GW_REJECT", "FAILED"]
#' @param message_type string type of the message. Default: "text". Options: ["audio", "contacts",
#'   "document", "hsm", "image", "location", "template", "text", "video", "voice", "unknown"]
#' @param country_iso string Destination number ISO country code. Example: "US"
#' @param error string Reported error code. Example: "40001"
#' @param normalized_carrier string Carrier. Example: "Verizon"
#' @param tag string Specified tag. Example: "Tag1"
#' @param mcc string Mobile country code. Example: "204"
#' @param mnc string Mobile network code. Example: "01"
#' @param product string Used product. Options: ["LONG_CODE", "SHORT_CODE", "SHORT_CODE_FTEU",
#'   "TOLL_FREE", "ALPHANUMERIC_ID", "RCS"]
#'
#' @importFrom dplyr as_tibble
#' @importFrom glue glue
#' @importFrom httr add_headers content GET
#' @importFrom jsonlite fromJSON
#'
#' @export
#'
get_paginated_mdrs <- function(start_date,
                               end_date,
                               page_number = 1,
                               page_size = 20,
                               sort = c("created_at"),
                               id = NULL,
                               direction = NULL,
                               outbound_profile_id = NULL,
                               cld = NULL,
                               cli = NULL,
                               status = NULL,
                               message_type = NULL,
                               country_iso = NULL,
                               error = NULL,
                               normalized_carrier = NULL,
                               tag = NULL,
                               mcc = NULL,
                               mnc = NULL,
                               product = NULL) {
  endpoint <- "message_detail_records?"
  start_date <- glue('start_date={as.character(start_date, format = "%Y-%m-%dT%H:%M:%S%Z")}&')
  end_date <- glue('end_date={as.character(end_date, format = "%Y-%m-%dT%H:%M:%S%Z")}&')
  page_number <- glue("page[number]={page_number}&")
  page_size <- glue("page[size]={page_size}&")
  sort <- paste0(glue("sort[]={sort}&"), collapse = "")
  id <- ifelse(is.null(id), "", glue("id={id}&"))
  direction <- ifelse(is.null(direction), "", glue("direction={direction}&"))
  outbound_profile_id <- ifelse(
    is.null(outbound_profile_id), "", glue("outbound_profile_id={outbound_profile_id}&")
  )
  cld <- ifelse(is.null(cld), "", glue("cld={cld}&"))
  cli <- ifelse(is.null(cli), "", glue("cli={cli}&"))
  status <- ifelse(is.null(status), "", glue("status={status}&"))
  message_type <- ifelse(is.null(message_type), "", glue("message_type={message_type}&"))
  country_iso <- ifelse(is.null(country_iso), "", glue("country_iso={country_iso}&"))
  error <- ifelse(is.null(error), "", glue("error={error}&"))
  normalized_carrier <- ifelse(
    is.null(normalized_carrier), "", glue("normalized_carrier={normalized_carrier}&")
  )
  tag <- ifelse(is.null(tag), "", glue("tag={tag}&"))
  mcc <- ifelse(is.null(mcc), "", glue("mcc={mcc}&"))
  mnc <- ifelse(is.null(mnc), "", glue("mnc={mnc}&"))
  product <- ifelse(is.null(product), "", glue("product={product}&"))
  res <- GET(
    glue(
      "{api_url()}{endpoint}{start_date}{end_date}{page_number}{page_size}{sort}{id}{direction}",
      "{outbound_profile_id}{cld}{cli}{status}{message_type}{country_iso}{error}",
      "{normalized_carrier}{tag}{mcc}{mnc}{product}"
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
