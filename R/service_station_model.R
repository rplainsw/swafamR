#' Service Station Model
#'
#' Model lobby areas of Full Service, Skycap, and STK's as well as bag drop.
#' Major room for imporvements here, but for now it uses basic queue theoy
#' deterministic logic. Service Rate is how many PAX can be processed in the
#' binned time increment. This function processes the pax, gives the queues and wait times
#' at each time increment.
#'
#' @param df_pax_model dataframe
#' @param mod_inputs list: object of data packages
#' @param minute_bin integer
#' @export
service_station_model <- function(df_pax_model, mod_inputs, minute_bin=5) {

  # assign dataframes
  assumptions <- mod_inputs$assumptions %>%
    dplyr::rename(orig = .data$station)
  df_transactions <- swafamR::df_transaction %>%
    dplyr::rename(orig = .data$station)

  # Take columns needed from station assumptions
  # positions, checkpoints, and contact ratios
  clean_assumptions <- assumptions %>%
    dplyr::left_join(mod_inputs$contact_ratios, by=c('orig')) %>%
    dplyr::select(.data$orig,
           skycap_rate = .data$skycap,
           full_service_rate = .data$full_service,
           stk_rate = .data$stk,
           skycap_pos = .data$curbside_positions_of_screens_customer_touch_points,
           full_service_pos = .data$full_service_positions_of_screens_customer_touch_points,
           stk_pos = .data$self_tagging_kiosks,
           bag_drop_pos = .data$station_assumption_id_validation_activator_positions_for_self_tagging,
           regular_checkpoints = .data$of_regular_checkpoints,
           pre_checkpoints = .data$of_pre_checkpoints) %>%
    dplyr::as_tibble() %>%
    dplyr::mutate_if(is.numeric, ~tidyr::replace_na(., 0))

  # Service Station Workflow
  df_service_station <- df_pax_model %>% # Join dataframes
    dplyr::left_join(clean_assumptions, by='orig') %>%
    dplyr::left_join(df_transactions, by='orig') %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$carrier == 'WN') %>%
    dplyr::group_by(.data$orig) %>%
    dplyr::arrange(.data$time) %>%
    dplyr::mutate(
      # Default transaction times for missing assumptions
      curbside_transaction_time = ifelse(is.na(.data$curbside_transaction_time), 2, .data$curbside_transaction_time),
      ticketing_transaction_time = ifelse(is.na(.data$ticketing_transaction_time), 2.6, .data$ticketing_transaction_time)
    ) %>%
    dplyr::mutate(
      # how many can be processed in the minute bins
      # e.g. 10 minute bin / 2.6 = 3.846 Serviced
      skycap_service_rate = minute_bin/.data$curbside_transaction_time,
      full_service_service_rate = minute_bin/.data$ticketing_transaction_time
    ) %>%
    dplyr::mutate(
      # Capacities based on ground ops throughput
      # e.g. for 10 min bins
      # STK CAP = (10/ 2_min_process_time) * kiosks
      # BAG DROP CAP = (10/ 22_seconds) * bag drop positions
      stk_capacity = (minute_bin/2) * .data$stk_pos,
      bag_drop_capacity = .data$bag_drop_pos * (minute_bin/(22/60)),
      full_service_capacity = .data$full_service_service_rate * .data$full_service_pos,
      skycap_capacity = .data$skycap_service_rate * .data$skycap_pos,
      exp_stk = .data$pax_wbag_demand * .data$stk_rate,
      exp_full_service = .data$pax_wbag_demand * .data$full_service_rate,
      exp_skycap = .data$pax_wbag_demand * .data$skycap_rate
    ) %>%
    dplyr::mutate(
      exp_bag_drop = .data$exp_stk
    ) %>%
    dplyr::mutate(
      # Queue for bin, DEMAND - CAPACITY
      skycap_overflow = .data$exp_skycap - .data$skycap_capacity,
      stk_overflow = .data$exp_stk - .data$stk_capacity,
      full_service_overflow = .data$exp_full_service - .data$full_service_capacity,
      bag_drop_overflow = .data$exp_stk - .data$bag_drop_capacity
    ) %>%
    dplyr::mutate(
      # Adjust Queue to only contain + values
      skycap_overflow = ifelse(.data$skycap_overflow < 0, 0, .data$skycap_overflow),
      stk_overflow = ifelse(.data$stk_overflow < 0, 0, .data$stk_overflow),
      full_service_overflow = ifelse(.data$full_service_overflow < 0, 0, .data$full_service_overflow),
      bag_drop_overflow = ifelse(.data$bag_drop_overflow < 0, 0, .data$bag_drop_overflow)
    ) %>%
    dplyr::mutate(
      full_service_demand = .data$exp_full_service + dplyr::lag(.data$full_service_overflow),
      stk_demand = .data$exp_stk + dplyr::lag(.data$stk_overflow),
      skycap_demand = .data$exp_skycap + dplyr::lag(.data$skycap_overflow),
      bag_drop_demand = .data$exp_bag_drop + dplyr::lag(.data$bag_drop_overflow)
    ) %>%
    dplyr::group_by(.data$orig) %>%
    dplyr::arrange(.data$orig, .data$time) %>%
    dplyr::mutate(
      # Add Queue to previous bin to get current queue
      skycap_queue = ceiling(dplyr::lag(.data$skycap_overflow)) + ceiling(.data$skycap_overflow),
      stk_queue = ceiling(dplyr::lag(.data$stk_overflow)) + ceiling(.data$stk_overflow),
      full_service_queue = ceiling(dplyr::lag(.data$full_service_overflow)) + ceiling(.data$full_service_overflow),
      bag_drop_overflow = ceiling(dplyr::lag(.data$bag_drop_overflow)) + ceiling(.data$bag_drop_overflow)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      stk_wait_time = ((.data$stk_demand * minute_bin)/.data$stk_capacity)/(.data$stk_capacity/5),
      full_service_wait_time = (.data$full_service_demand * minute_bin) / .data$full_service_capacity,
      skycap_wait_time = (.data$skycap_demand * minute_bin) / .data$skycap_capacity,
      bag_drop_wait_time = ((.data$bag_drop_demand * minute_bin) / .data$bag_drop_capacity)/(.data$bag_drop_capacity/(10/(22/60)))
    ) %>%
    dplyr::mutate(carrier = 'WN')

  df_service_station
}
