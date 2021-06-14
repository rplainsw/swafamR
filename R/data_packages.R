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
#' @return dataframe
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
#' @return dataframe
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
#' @return dataframe
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

#' Pull in assumptions from quickbase
#' @return dataframe
#' @export
generate_station_assumptions <- function() {

  url <- 'https://southwest.quickbase.com/db/bj36xdfn2?a=API_DoQuery&usertoken=b2pyfd_fzh_cjgmnn9ce7z3vwcjbkkxteb9r49'
  url2 <- 'https://southwest.quickbase.com/db/bj3zqje49?a=API_DoQuery&usertoken=b2pyfd_fzh_cjgmnn9ce7z3vwcjbkkxteb9r49'
  x <- xml2::read_xml(url)
  y <- xml2::read_xml(url2)
  myXML <- XML::xmlParse(x)
  myXML2 <- XML::xmlParse(y)

  df <- XML::xmlToDataFrame(myXML, stringsAsFactors = FALSE) %>%
    dplyr::mutate_all(~utils::type.convert(., as.is = TRUE)) %>%
    janitor::clean_names() %>%
    dplyr::select(station = .data$station_assumption_station_code,
           .data$full_service_positions_of_screens_customer_touch_points:.data$station_assumption_bag_make_up_device_type) %>%
    dplyr::filter(!is.na(.data$station)) %>%
    dplyr::as_tibble()

  df2 <- XML::xmlToDataFrame(myXML2, stringsAsFactors = FALSE) %>%
    dplyr::rename(station = .data$station_code) %>%
    dplyr::select(.data$station, .data$system_throughput) %>%
    dplyr::mutate(system_throughput = as.numeric(.data$system_throughput))

  dplyr::left_join(df, df2, by='station')

}



adjust_pgds_bins <- function(df_pgds, minute_bin=5) {

  df_pgds2 <- df_pgds %>%
    dplyr::filter(!is.na(.data$distance)) %>%
    dplyr::rename(before_9 = .data$timeframe) %>%
    dplyr::mutate(before_9 = .data$before_9 == 'Pre')

  temp_var <- ifelse(minute_bin == 5, 2, ifelse(minute_bin == 2, 5, 1))
  message(glue::glue("Minute Bin: {minute_bin}  Temp Var: {temp_var}"))

  df_pgds2 %>%
    dplyr::mutate(domestic = .data$distance == 'Domestic') %>%
    dplyr::rename_at(dplyr::vars(dplyr::starts_with("perc_bin")), stringr::str_replace, "perc_bin", "") %>%
    dplyr::relocate(.data$domestic, .after = .data$before_9) %>%
    dplyr::select(-.data$distance) %>%
    tidyr::pivot_longer(
      cols = !.data$before_9:.data$domestic,
      names_to = 'minutes_prior',
      values_to = 'perc_bin'
    ) %>%
    dplyr::mutate(minutes_prior = as.numeric(.data$minutes_prior)) %>%
    dplyr::mutate(temp = list(0:9)) %>%
    tidyr::unnest(cols = c(.data$temp)) %>%
    dplyr::mutate(minutes_prior = .data$minutes_prior - .data$temp) %>%
    dplyr::arrange(.data$domestic, .data$before_9, .data$minutes_prior) %>%
    dplyr::filter(.data$minutes_prior %% minute_bin == 0) %>%
    dplyr::mutate(perc_bin = .data$perc_bin/temp_var) %>%
    dplyr::select(-.data$temp) %>%
    tidyr::pivot_wider(
      id_cols = .data$before_9:.data$domestic,
      names_from = .data$minutes_prior,
      values_from = .data$perc_bin
    ) %>%
    dplyr::mutate(oa = TRUE)
}

#' List of Model Inputs
#'
#' @param dat_pack DB Object
#' @param minute_bin integer
#' @export
assign_mod_inputs <- function(dat_pack, minute_bin=5) {

  flight_schedule <- NULL

  load_factors <- NULL

  bag_factors <- NULL

  arr_curve <- dplyr::tbl(dat_pack, "ARRIVAL_CURVE") %>%
    dplyr::collect() %>%
    clean_arrival_curve(minute_bin = 10)

  assumptions <- generate_station_assumptions()

  contact_ratios <- dplyr::tbl(dat_pack, "CONTACT_RATIOS") %>%
    dplyr::collect()

  pre_check_perc <- dplyr::tbl(dat_pack, "PRE_CHECK") %>%
    dplyr::collect()

  gate_counts <- dplyr::tbl(dat_pack, "GATES") %>%
    dplyr::collect() %>%
    dplyr::mutate(buffer = ceiling(.data$buffer/minute_bin) * minute_bin,
           ron = ceiling(.data$buffer/minute_bin) * minute_bin)

  shared_checkpoints <- dplyr::tbl(dat_pack, "SHARED_CHECKPOINT") %>%
    dplyr::collect() %>%
    dplyr::mutate(dplyr::across(c('cart_staging', 'checkpoint', 'cbis', 'cbra'), as.numeric))

  list(
    flight_schedule = flight_schedule,
    load_factors = load_factors,
    bag_factors = bag_factors,
    arr_curve = arr_curve,
    assumptions = assumptions,
    contact_ratios = contact_ratios,
    pre_check_perc = pre_check_perc,
    gate_counts = gate_counts,
    shared_checkpoints = shared_checkpoints
  )

}




























