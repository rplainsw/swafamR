#' PGDS Arrival Curve
#'
#' A dataset that provides a standard arrival curve to use for
#' other airlines
#'
#' @format A dataframe with 5 rows and 18 variables:
#'  \describe{
#'   \item{distance}{international or domestic}
#'   \item{timeframe}{timeframe before or after 9 am}
#'   \item{perc_bin10}{increment of pax arriving at 1 min prior}
#'   \item{perc_bin20}{increment of pax arriving at 1 min prior}
#'   \item{perc_bin30}{increment of pax arriving at 1 min prior}
#'   \item{perc_bin40}{increment of pax arriving at 1 min prior}
#'   \item{perc_bin50}{increment of pax arriving at 1 min prior}
#'   \item{perc_bin60}{increment of pax arriving at 1 min prior}
#'   \item{perc_bin70}{increment of pax arriving at 1 min prior}
#'   \item{perc_bin80}{increment of pax arriving at 1 min prior}
#'   \item{perc_bin90}{increment of pax arriving at 1 min prior}
#'   \item{perc_bin100}{increment of pax arriving at 1 min prior}
#'   \item{perc_bin110}{increment of pax arriving at 1 min prior}
#'   \item{perc_bin120}{increment of pax arriving at 1 min prior}
#'   \item{perc_bin130}{increment of pax arriving at 1 min prior}
#'   \item{perc_bin140}{increment of pax arriving at 1 min prior}
#'   \item{perc_bin150}{increment of pax arriving at 1 min prior}
#'   \item{perc_bin160}{increment of pax arriving at 1 min prior}
#' }
"df_pgds"


#' Other Airline Local Markets
#'
#' A dataset from 2019 providing the local factor of passengers
#'
#' @format A dataframe with 1507 rows and 8 variables:
#' \describe{
#'   \item{dominant_marketing_airline}{Carrier}
#'   \item{origin}{origin}
#'   \item{passengers_per_day}{Total Passengers}
#'   \item{connection_points}{Route pairing}
#'   \item{right_dominant_marketing_airline}{Other carrier}
#'   \item{right_passengers_per_day}{Joined route passengers}
#'   \item{total}{total}
#'   \item{local}{local}
#' }
"df_oa_local"


#' Average Transaction Times for Ticketing
#'
#' @format A dataset with 68 rows and 3 variables
#' \describe{
#'   \item{station}{origin}
#'   \item{curbside_transaction_time}{skycap service rate}
#'   \item{ticketing_transaction_time}{full service service rate}
#' }
"df_transaction"


#' List of domestic stations
#'
#' @format A vector of 68 character
"domestic_list"





















