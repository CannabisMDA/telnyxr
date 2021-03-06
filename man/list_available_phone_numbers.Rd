% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/list_available_phone_numbers.R
\name{list_available_phone_numbers}
\alias{list_available_phone_numbers}
\title{List available phone numbers}
\usage{
list_available_phone_numbers(
  filter_phone_number_starts_with = NULL,
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
  filter_exclude_held_numbers = NULL
)
}
\arguments{
\item{filter_phone_number_starts_with}{string Filter numbers starting with a pattern (exclude
NDC from start of this filter if used with `national_destination_code` filter).}

\item{filter_phone_number_ends_with}{string Filter numbers ending with a pattern (max length of
4 digits if used with `national_destination_code` filter).}

\item{filter_phone_number_contains}{string Filter numbers containing a pattern (must be used
with `national_destination_code` filter and only searches within last 4 digits).}

\item{filter_locality}{string Filter phone numbers by city.}

\item{filter_administrative_area}{string Filter phone numbers by US state/CA province.
Example: "IL"}

\item{filter_country_code}{string Filter phone numbers by ISO alpha-2 country code. Example: "US"}

\item{filter_national_destination_code}{string Filter by the national destination code of the
number. This filter is only applicable to North American numbers.}

\item{filter_rate_center}{string Filter phone numbers by NANP rate center. This filter is only
applicable to North American numbers. Example: "CHICAGO HEIGHTS"}

\item{filter_phone_number_type}{string Filter phone numbers by number type.
Options: ["local", "toll_free", "mobile", "national", "shared_cost", "landline"]}

\item{filter_features}{array of string Filter if the phone number should be used for voice, fax,
mms, sms, emergency. Options: ["sms", "mms", "voice", "fax", "emergency"]}

\item{filter_limit}{integer Limits the number of results. Example: 100}

\item{filter_best_effort}{boolean Filter to determine if best effort results should be included.
Only available in NANPA countries.}

\item{filter_quickship}{boolean Filter to exclude phone numbers that need additional time after
to purchase to receive phone calls. Only available for toll-free numbers.}

\item{filter_reservable}{boolean Filter to exclude phone numbers that cannot be reserved before
purchase.}

\item{filter_exclude_held_numbers}{boolean Filter to exclude phone numbers that are currently on
hold for your account.}
}
\description{
List available phone numbers to buy.
}
