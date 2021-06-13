
carousel_arrival_curve <- function(.data) {

  .data %>%
    dplyr::filter(.data$carrier == 'WN' | .data$cart_staging == 1) %>%
    dplyr::select(.data$expected_pax_wbag:.data$`240`) %>%
    dplyr::select(-.data$expected_pax_wbag) %>%
    dplyr::mutate_all(~replace(., is.na(.), 0))
}


carousel_model <- function(df_bag_model, minutes_prior_bag_pickup=120) {

  arrival_curve_caro <- carousel_arrival_curve(df_bag_model)

  caro <- df_bag_model %>%
    dplyr::filter(.data$carrier == 'WN' | .data$cart_staging == 1) %>%
    dplyr::select(.data$dep_dttm, .data$dep_int, .data$orig, .data$dest, .data$carrier) %>%
    cbind(
      df_bag_model %>%
        dplyr::filter(.data$carrier == "WN" | .data$cart_staging == 1) %>%
        dplyr::pull(.data$expected_bag) * arrival_curve_caro %>%
        data.frame()
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::starts_with("X"),
      names_to = 'minutes_prior',
      values_to = 'bags'
    ) %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(minutes_prior = stringr::str_replace(.data$minutes_prior, "X", "") %>%
             as.numeric(.data$minutes_prior)) %>%
    dplyr::mutate(
      time = .data$dep_dttm - lubridate::minutes(.data$minutes_prior),
      remove_caro_factor = ifelse(.data$minutes_prior <= minutes_prior_bag_pickup, 1, 0)
    ) %>%
    dplyr::group_by(.data$orig, .data$dest, .data$dep_int) %>%
    dplyr::arrange(.data$orig, .data$dest, .data$dep_int, dplyr::desc(.data$minutes_prior)) %>%
    dplyr::mutate(bags_on_carousel = cumsum(.data$bags - (10*.data$remove_caro_factor))) %>%
    dplyr::mutate(bags_on_carousel = ifelse(.data$bags_on_carousel < 0, 0, .data$bags_on_carousel)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$orig, .data$time) %>%
    dplyr::summarise(bags_on_carousel = sum(.data$bags_on_carousel),
              .groups = 'drop') %>%
    dplyr::mutate(carrier = 'WN') %>%
    dplyr::ungroup()

  return(caro)
}
