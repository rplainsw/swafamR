#' Checkpoint Model
#'
#' @param df_pax_model dataframe
#' @param mod_inputs list
#' @param minute_bin integer
#' @export
checkpoint_model <- function(df_pax_model, mod_inputs, minute_bin=5) {

  df <- df_pax_model %>%
    dplyr::ungroup() %>%
    dplyr::select(-.data$bag_demand, -.data$pax_wbag_demand) %>%
    dplyr::left_join(mod_inputs$pre_check, by='orig') %>%
    dplyr::left_join(
      mod_inputs$shared_checkpoints %>%
        dplyr::select(orig = .data$station, .data$carrier, .data$checkpoint),
      by=c('orig','carrier')) %>%
    dplyr::filter(.data$carrier == 'WN' | .data$checkpoint == 1) %>%
    dplyr::mutate(exp_pre_check = .data$pax_demand * .data$pre_chk_perc,
           exp_reg_check = .data$pax_demand - .data$exp_pre_check)

  df_carrier <- df %>%
    dplyr::group_by(.data$orig, .data$time, .data$carrier) %>%
    dplyr::summarise(total_reg_check_carrier = sum(.data$exp_reg_check),
              total_pre_check_carrier = sum(.data$exp_pre_check),
              .groups='drop')

  df <- df %>%
    dplyr::group_by(.data$orig, .data$time) %>%
    dplyr::summarise(total_reg_check = sum(.data$exp_reg_check),
              total_pre_check = sum(.data$exp_pre_check),
              pre_chk_perc = max(.data$pre_chk_perc, na.rm = TRUE),
              .groups='drop') %>%
    dplyr::left_join(mod_inputs$assumptions %>%
                dplyr::select(.data$station,
                       reg_pos = .data$of_regular_checkpoints,
                       pre_pos = .data$of_pre_checkpoints) %>%
                dplyr::mutate_if(is.integer, ~tidyr::replace_na(., 0)),
              by=c("orig"="station")) %>%
    dplyr::mutate(
      reg_check_capacity = 27 * .data$reg_pos,
      pre_check_capacity = 35 * .data$pre_pos
    ) %>%
    dplyr::group_by(.data$orig) %>%
    dplyr::arrange(.data$orig, .data$time) %>%
    dplyr::mutate(reg_check_overflow = .data$total_reg_check - .data$reg_check_capacity,
           pre_check_overflow = .data$total_pre_check - .data$pre_check_capacity) %>%
    dplyr::mutate(reg_check_overflow = ifelse(.data$reg_check_overflow < 0, 0, .data$reg_check_overflow),
           pre_check_overflow = ifelse(.data$pre_check_overflow < 0, 0, .data$pre_check_overflow)) %>%
    dplyr::mutate(reg_check_demand = .data$total_reg_check + dplyr::lag(.data$reg_check_overflow),
           pre_check_demand = .data$total_pre_check + dplyr::lag(.data$pre_check_overflow)) %>%
    dplyr::mutate(reg_check_queue = ceiling(dplyr::lag(.data$reg_check_overflow)) + ceiling(.data$reg_check_overflow),
           pre_check_queue = ceiling(dplyr::lag(.data$pre_check_overflow)) + ceiling(.data$pre_check_overflow)) %>%
    dplyr::mutate(reg_check_wait_time = (.data$reg_check_demand * minute_bin) / .data$reg_check_capacity,
           pre_check_wait_time = (.data$pre_check_demand * minute_bin) / .data$pre_check_capacity)


  df <- df_carrier %>%
    dplyr::left_join(df, by=c('orig','time'))

  df
}
