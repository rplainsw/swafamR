#' @keywords internal
carousel_capacity <- function(data, ...) {

  data %>%
    dplyr::select(
      .data$station,
      caro_lf_cap = .data$outbound_bag_cart_staging_length_for_wn_lf,
      device_type = .data$bag_make_up_device_type
    ) %>%
    dplyr::mutate(caro_lf_cap = ifelse(.data$device_type == "Sloped", .data$caro_lf_cap * 2, .data$caro_lf_cap)) %>%
    dplyr::mutate(caro_lf_cap = .data$caro_lf_cap/3)

}

#' @keywords internal
flight_count <- function(data) {

  data %>%
    dplyr::count(.data$dep_dttm, .data$orig, .data$carrier)

}

#' @keywords internal
plane_type_counts <- function(.data) {

  .data %>%
    dplyr::filter(.data$carrier == 'WN') %>%
    dplyr::mutate(plane_type=ifelse(.data$seats <= 143, '-700','-800')) %>%
    dplyr::group_by(.data$orig, .data$carrier) %>%
    dplyr::summarise(daily_700_count = sum(.data$plane_type == '-700'),
              daily_800_count = sum(.data$plane_type == '-800'),
              .groups = 'drop')

}

#' @keywords internal
generate_capacities <- function(data, ...) {

  data %>%
    dplyr::select(
      .data$station,
      bag_capacity = .data$system_throughput,
      tpoint_capacity_lf = .data$outbound_bag_cart_staging_length_for_wn_lf,
      device_type = .data$station_assumption_bag_make_up_device_type,
      cart_staging_type = .data$bag_cart_staging_type,
      eds_machines = .data$of_eds_machines,
      int_full_service_pos = .data$int_l_full_service_positions,
      .data$system_type,
      .data$outbound_bag_cart_staging_length_for_wn_lf,
      cutoff_time = .data$station_assumption_cut_off_time
    ) %>%
    dplyr::mutate(
      bag_capacity = ifelse(is.na(.data$bag_capacity), 0, .data$bag_capacity),
      bag_capacity = .data$bag_capacity/6,
      carousel_capacity = ifelse(
        .data$device_type == 'Sloped',
        .data$tpoint_capacity_lf * 2,
        .data$tpoint_capacity_lf),
      carousel_capacity = .data$carousel_capacity / 3
    )

}

#' @keywords internal
RobustMax <- function(x) {if (length(x)>0) max(x, na.rm = TRUE) else 0}

#' @keywords internal
increment_logic <- function(data, col_metric, col_cap) {

  data %>%
    dplyr::filter(!! rlang::sym(col_cap) > 0) %>%
    dplyr::group_by(.data$station, .data$carrier) %>%
    dplyr::arrange(.data$station, .data$carrier, .data$time) %>%
    dplyr::mutate(
      counter = ifelse(
        (!! sym(col_metric) - !! rlang::sym(col_cap) > 0) &
          (dplyr::lag(!! rlang::sym(col_metric)) - !! rlang::sym(col_cap) > 0),
        1,
        0)
    ) %>%
    dplyr::mutate(
      start_run = ifelse(.data$counter == 1 & dplyr::lag(.data$counter) == 0, 1, 0),
      end_run = ifelse(.data$counter == 1 & dplyr::lead(.data$counter) == 0, 1, 0)
    ) %>%
    dplyr::mutate(rn = .data$row_number()) %>%
    dplyr::filter(.data$end_run > 0 | .data$start_run > 0) %>%
    dplyr::mutate(length_run = .data$rn - dplyr::lag(.data$rn)) %>%
    dplyr::filter(.data$end_run == 1) %>%
    dplyr::filter(.data$start_run == 0) %>%
    dplyr::group_by(.data$station) %>%
    dplyr::summarise(x = RobustMax(.data$length_run),
              .groups = 'drop') %>%
    dplyr::arrange(-.data$x)

}




