
tpoint_model <- function(df_bag_model, cart_staging_time=120, minute_bin=5) {

  tpoint_curve <- seq(30, cart_staging_time, minute_bin)

  tpoint <- df_bag_model %>%
    dplyr::filter(.data$carrier == 'WN' | .data$cart_staging == 1)

  tpoint <- tpoint %>%
    dplyr::mutate(ncarts = ceiling(.data$expected_bag/45)) %>%
    dplyr::mutate(ncarts = ifelse(.data$ncarts > 3, 3, .data$ncarts)) %>%
    dplyr::select(.data$dep_dttm, .data$dep_int, .data$orig,
                  .data$dest, .data$carrier, .data$ncarts)

  tpoint <- tpoint %>%
    cbind(matrix(tpoint_curve,
                 nrow = nrow(tpoint),
                 ncol = length(tpoint_curve),
                 byrow = TRUE) %>%
            data.frame()) %>%
    tidyr::pivot_longer(
      cols = dplyr::starts_with("X"),
      names_to = 'key',
      values_to = 'minutes_prior'
    ) %>%
    dplyr::mutate(time = .data$dep_dttm - lubridate::minutes(.data$minutes_prior),
                  ncarts = ifelse(.data$minutes_prior <= 60, .data$ncarts-1, .data$ncarts)) %>%
    dplyr::group_by(.data$orig, .data$carrier, .data$time) %>%
    dplyr::summarise(ncarts = sum(.data$ncarts), .groups = 'drop') %>%
    dplyr::mutate(parrallel_carts = .data$ncarts * 12,
                  perpendicular_carts = .data$ncarts * 8.5)

  tpoint
}
