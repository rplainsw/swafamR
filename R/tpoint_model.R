#' Tpoint Model
#'
#' Similar to how the arrival curve is implemented, a tpoint curve is created from
#' 30 min prior to departure up to the cart staging time minutes prior to departure.
#' A cart represents 45 bags, we cap it at 3 carts for a flight. Carts are then allocated
#' based on the number required for the flight and how long the cart staging time is.
#' At 1 hour prior to departure, a cart is removed from the allocation.
#'
#' If a flight is expecting 100 bags, that would represent 3 carts due to a ceiling
#' function. At a stage time of 120 min, 3 carts would be allocated 2 hours before departure.
#' They would remain there until an hour prior, at which time 1 cart would be removed. The
#' remaining 2 carts would then be allocated at tpoint until 30 min prior to departure, at
#' which time both carts are removed.
#'
#' @param df_bag_model dataframe
#' @param cart_staging_time integer
#' @param minute_bin integer
#' @export
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
