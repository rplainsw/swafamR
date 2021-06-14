
join_gate_model_inputs <- function(mod_inputs) {

  df_sched <- mod_inputs$flight_schedule

  df_ron_times <- mod_inputs$gate_counts

  df_gate <- df_sched %>%
    dplyr::rename(station = .data$orig) %>%
    dplyr::select(.data$station, .data$carrier, .data$dep_dttm, .data$arr_dttm, .data$eqpt) %>%
    dplyr::mutate(plane_type = dplyr::case_when(
      .data$eqpt %in% c('733', '73C', '73W') ~ '-700',
      .data$eqpt %in% c('73H', '738', '7M8') ~ '-800',
      TRUE ~ ''
    )) %>%
    dplyr::filter(.data$carrier == 'WN') %>%
    dplyr::group_by(.data$station) %>%
    dplyr::mutate(origators = sum(is.na(.data$arr_dttm))) %>%
    dplyr::ungroup() %>%
    dplyr::select(.data$station, .data$arr_dttm, .data$dep_dttm, .data$plane_type, .data$origators) %>%
    dplyr::left_join(df_ron_times, by='station')  %>%
    dplyr::filter(!is.na(.data$gate_count))

  df_gate
}


originator_designation <- function(df_gate) {

  df_gate <- df_gate %>%
    dplyr::mutate(dep_dttm = .data$dep_dttm + lubridate::minutes(.data$buffer)) %>%
    dplyr::group_by(.data$station) %>%
    dplyr::arrange(.data$station, .data$dep_dttm) %>%
    dplyr::mutate(origator = is.na(.data$arr_dttm)) %>%
    dplyr::mutate(origator_id = cumsum(.data$origator)) %>%
    dplyr::ungroup()

  df_gate
}

allocate_time_gates <- function(df_gate) {

  df_gate <- df_gate %>%
    dplyr::mutate(
      origator_id = .data$origator_id * .data$origator,
      arrival_at_gate_origator = .data$dep_dttm - lubridate::minutes(.data$ron),
      time_at_gate_regular = lubridate::minutes(.data$dep_dttm - .data$arr_dttm),
      time_at_gate_origator = lubridate::force_tz(lubridate::date(.data$dep_dttm) + lubridate::hours(3)),
      time_at_gate_origator = difftime(.data$dep_dttm, .data$time_at_gate_origator, units = "mins"),
      time_at_gate_origator = as.numeric(.data$time_at_gate_origator),
      time_at_gate_regular = lubridate::minutes(.data$dep_dttm - .data$arr_dttm),
      time_at_gate = dplyr::case_when(
        is.na(.data$time_at_gate_regular) & (.data$origator_id <= .data$gate_count) ~ .data$time_at_gate_origator,
        is.na(.data$time_at_gate_regular) & (.data$origator_id > .data$gate_count) ~ as.numeric(.data$ron),
        TRUE ~ lubridate::minute(.data$time_at_gate_regular)
      ))

  df_gate
}

#' Gate Model
#'
#' Models the planes occupying gates during the day. After classifying plane types, add
#' buffer time to departure time. From there work, the way back of occupying a gate from dep time
#' + buffer, to the arrival time. Arrival times that are NULL are considered originators. Originator
#' logic is calculated by taking the cumsum of of binary orignator values. This will give which number
#' originator the plane is.
#'
#' @param mod_inputs list
#' @param minute_bin integer
#' @export
gate_model <- function(mod_inputs, minute_bin=5) {

  df_gate <- join_gate_model_inputs(mod_inputs) %>%
    originator_designation() %>%
    allocate_time_gates()

  df_gate %>%
    dplyr::group_by(rn = dplyr::row_number()) %>%
    dplyr::mutate(time_list = list(seq(0, ceiling(.data$time_at_gate/minute_bin)*minute_bin, minute_bin))) %>%
    tidyr::unnest(cols = .data$time_list) %>%
    dplyr::mutate(time = .data$dep_dttm - lubridate::minutes(.data$time_list),
           occupy_gate = 1) %>%
    dplyr::group_by(.data$station, .data$time) %>%
    dplyr::summarise(seven_hundreds = sum(.data$plane_type == '-700'),
              eight_hundreds = sum(.data$plane_type == '-800'),
              all = sum(.data$occupy_gate),
              origators = max(.data$origators, na.rm = TRUE),
              .groups = 'drop') %>%
    dplyr::left_join(mod_inputs$gate_counts %>%
                dplyr::select(-.data$ron, -.data$buffer), by='station') %>%
    dplyr::mutate(carrier = 'WN') %>%
    dplyr::rename(orig = .data$station) %>%
    dplyr::arrange(.data$orig, .data$time)

}

