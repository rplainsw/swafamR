#' Connect to data packages in SQLite DB
#'
#' @param SQLiteDB A string
#' @return DB Connection
#' @export

dbConnectSQLite <- function(SQLiteDB) {

  DBI::dbConnect(
    RSQLite::SQLite(),
    SQLiteDB
  )
}

#' Clean load factors from DB, filter out for orig-dest-dep hour
#' pairings that have less than 2 flights
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @param data tibble: of load factors from DB
#' @param minute_bin integer: bin the departure time
#' @export

clean_load_factors <- function(data, minute_bin = 10) {

  data %>%
    dplyr::mutate(dep_hour = floor(.data$dep_int/100)) %>%
    dplyr::group_by(.data$orig, .data$dest, .data$dep_hour) %>%
    dplyr::mutate(rn = dplyr::row_number()) %>%
    dplyr::mutate(rn = max(.data$rn, na.rm = FALSE)) %>%
    dplyr::filter(.data$rn > 2)

}


#' Proxy logic for new stations to use paired station historical data
#'
#' @param data dataframe: col required 'orig'
#' @export

update_new_stations <- function(data) {

  old_stations <- c(
    "FLL", "SAN", "DEN", "HOU",
    "MDW", "BUR", "PDX", "CHS",
    "DEN", "MEM", "TPA", "PNS",
    "DSM", "ISP"
  )

  new_stations <- data %>%
    dplyr::filter(.data$orig %in% old_stations) %>%
    dplyr::mutate(orig = dplyr::case_when(

      .data$orig == 'FLL' ~ 'MIA' ,
      .data$orig == 'SAN' ~ 'PSP',
      .data$orig == 'DEN' ~ 'MTJ',
      .data$orig == 'HOU' ~ 'IAH',
      .data$orig == 'MDW' ~ 'ORD',
      .data$orig == 'BUR' ~ 'SBA',
      .data$orig == 'PDX' ~ 'FAT',
      .data$orig == 'CHS' ~ 'SAV',
      .data$orig == 'DEN' ~ 'COS',
      .data$orig == 'MEM' ~ 'JAN',
      .data$orig == 'TPA' ~ 'SRQ',
      .data$orig == 'PNS' ~ 'VPS',
      .data$orig == 'DSM' ~ 'BZN',
      .data$orig == 'ISP' ~ 'SYR'
    ))

  data %>%
    dplyr::bind_rows(new_stations)

}

#' Group arrivals and get percentage of PAX arriving in given time
#' bin prior to departure. Arranges vector into a wide format to join
#' onto each flight.
#'
#' @param data dataframe
#' @param minute_bin integer: bin the departure time
#' @export

clean_arrival_curve <- function(data, minute_bin=5) {

  data %>%
    dplyr::mutate(min_prior_arr = (
      floor(.data$min_prior_arr/minute_bin) * minute_bin
    ) + minute_bin) %>%
    dplyr::mutate(min_prior_arr = ifelse(
      .data$min_prior_arr > 240, 240, .data$min_prior_arr
    )) %>%
    dplyr::group_by(.data$orig, .data$before_9) %>%
    dplyr::mutate(total_pax = sum(.data$total, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$orig, .data$before_9, .data$min_prior_arr, .data$total_pax) %>%
    dplyr::summarise(pax_in_bin = sum(.data$total, na.rm = TRUE), .groups = 'drop') %>%
    dplyr::mutate(perc_bin = .data$pax_in_bin/.data$total_pax) %>%
    tidyr::pivot_wider(
      id_cols = c(.data$orig, .data$before_9),
      names_from = .data$min_prior_arr,
      values_from = .data$perc_bin
    )

}
