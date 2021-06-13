join_flight_sched_arr_curve <- function(mod_inputs, minute_bin) {

  df_arr_curve <- mod_inputs$arr_curve %>%
    dplyr::mutate(carrier = 'WN',
           before_9 = .data$before_9 == 1)

  df <- mod_inputs$flight_schedule %>%
    dplyr::mutate(domestic = .data$orig %in% swafamR::domestic_list,
           before_9 = .data$dep_int < 900)

  df_swa <- df %>%
    dplyr::filter(.data$carrier == 'WN') %>%
    dplyr::left_join(df_arr_curve, by = c('orig', 'before_9', 'carrier'))

  df_oa <- df %>%
    dplyr::filter(.data$carrier != 'WN') %>%
    dplyr::left_join(swafamR::df_pgds %>% adjust_pgds_bins(minute_bin = minute_bin))

  dplyr::bind_rows(df_swa, df_oa)
}

#' Tiered Factors
#'
#' Create a grouping by orig, dest and dep hour and use
#' what is available.
#'
#' @param df dataframe: flight schedule
#' @importFrom rlang :=
#' @importFrom rlang sym
#' @return dataframe
#' @export
tiered_factor_logic <- function(df) {

  params <- df %>%
    dplyr::select(dplyr::ends_with("factor")) %>%
    names()

  factor1 <- params[[1]]
  factor2 <- params[[2]]

  df_orig <- df %>%
    dplyr::group_by(.data$orig) %>%
    dplyr::summarise(factor1 = mean(!! sym(factor1), na.rm=TRUE),
              factor2 = mean(!! sym(factor2), na.rm = TRUE),
              .groups='drop')%>%
    dplyr::rename(!!factor1 := factor1,
           !!factor2 := factor2) %>%
    dplyr::mutate(carrier = 'WN')

  df_orig_dest <- df %>%
    dplyr::group_by(.data$orig, .data$dest) %>%
    dplyr::summarise(factor1 = mean(!! sym(factor1), na.rm=TRUE),
              factor2 = mean(!! sym(factor2), na.rm = TRUE),
              .groups='drop')%>%
    dplyr::rename(!!factor1 := factor1,
           !!factor2 := factor2) %>%
    dplyr::mutate(carrier = 'WN')

  df_orig_dest_hour <- df %>%
    dplyr::group_by(.data$orig, .data$dest, .data$dep_hour) %>%
    dplyr::summarise(factor1 = mean(!! sym(factor1), na.rm=TRUE),
              factor2 = mean(!! sym(factor2), na.rm = TRUE),
              .groups='drop') %>%
    dplyr::rename(!!factor1 := factor1,
           !!factor2 := factor2) %>%
    dplyr::mutate(carrier = 'WN')

  tiered_factors <- list(
    df_orig = df_orig,
    df_orig_dest = df_orig_dest,
    df_orig_dest_hour = df_orig_dest_hour,
    params = params
  )

  return(tiered_factors)

}


aggregate_tiers <- function(flight_schedule, factors) {

  dat <- factors %>%
    tiered_factor_logic()

  factor1 <- dat$params[[1]]
  factor2 <- dat$params[[2]]


  df <- flight_schedule %>%
    dplyr::left_join(dat$df_orig,
              by=c('orig','carrier')) %>%
    dplyr::left_join(dat$df_orig_dest,
              by=c('orig','dest','carrier'),
              suffix=c("_o","_od")) %>%
    dplyr::left_join(dat$df_orig_dest_hour,
              by=c('orig','dest','dep_hour','carrier'))


  return(df)

}

bag_model <- function(mod_inputs, minute_bin) {

  load_factors <- aggregate_tiers(mod_inputs$flight_schedule,
                                  mod_inputs$load_factors)

  bag_factors <- aggregate_tiers(mod_inputs$flight_schedule,
                                 mod_inputs$bag_factors)

  flight_cols <- colnames(mod_inputs$flight_schedule)


  join_cols <- c('orig','dest','dep_hour','carrier')


  df_bag_model <- join_flight_sched_arr_curve(mod_inputs, minute_bin = minute_bin) %>%
    dplyr::left_join(load_factors, by=flight_cols) %>%
    dplyr::left_join(bag_factors, by=flight_cols) %>%
    dplyr::mutate(load_factor = ifelse(is.na(.data$load_factor), .data$load_factor_od, .data$load_factor),
           load_factor = ifelse(is.na(.data$load_factor), .data$load_factor_o, .data$load_factor),
           orig_factor = ifelse(is.na(.data$orig_factor), .data$orig_factor_od, .data$orig_factor),
           orig_factor = ifelse(is.na(.data$orig_factor), .data$orig_factor_o, .data$orig_factor),
           chk_ratio_factor  = ifelse(is.na(.data$chk_ratio_factor), .data$chk_ratio_factor_od, .data$chk_ratio_factor),
           chk_ratio_factor = ifelse(is.na(.data$chk_ratio_factor), .data$chk_ratio_factor_o, .data$chk_ratio_factor),
           avg_bag_factor = ifelse(is.na(.data$avg_bag_factor), .data$avg_bag_factor_od, .data$avg_bag_factor),
           avg_bag_factor = ifelse(is.na(.data$avg_bag_factor), .data$avg_bag_factor_o, .data$avg_bag_factor),
    ) %>%
    dplyr::select(-dplyr::ends_with("_o"), -dplyr::ends_with("_od") ) %>%
    dplyr::left_join(swafamR::df_oa_local %>%
                dplyr::filter(.data$total > 50) %>%
                dplyr::select(carrier = .data$dominant_marketing_airline, orig = .data$origin, .data$local),
              by=c('orig','carrier')
    ) %>%
    dplyr::mutate(
      load_factor = ifelse(is.na(.data$load_factor), 0.8, .data$load_factor),
      orig_factor = dplyr::case_when(
        is.na(.data$arr_dttm) & .data$carrier == 'WN' ~ 1,
        !is.na(.data$arr_dttm) & .data$carrier == 'WN' ~ .data$orig_factor,
        .data$carrier != 'WN' & .data$local > 0 ~ .data$local,
        TRUE ~ 0.9
      ),
      chk_ratio_factor = ifelse(is.na(.data$chk_ratio_factor), 0.5, .data$chk_ratio_factor),
      avg_bag_factor = ifelse(is.na(.data$avg_bag_factor), 1.3, .data$avg_bag_factor)
    ) %>%
    dplyr::select(-.data$local) %>%
    dplyr::left_join(mod_inputs$shared_checkpoints,
              by=c('orig'='station','carrier'='carrier')) %>%
    dplyr::filter(.data$cart_staging == 1 | .data$cbis == 1 | .data$cbra == 1 | .data$checkpoint == 1 |  .data$carrier == 'WN') %>%
    dplyr::mutate(expected_pax = .data$seats * .data$load_factor * .data$orig_factor) %>%
    dplyr::mutate(expected_bag = .data$expected_pax * .data$chk_ratio_factor * .data$avg_bag_factor,
           expected_pax_wbag = .data$expected_pax * .data$chk_ratio_factor) %>%
    dplyr::arrange(.data$dep_int) %>%
    dplyr::relocate(c(dplyr::ends_with("factor"), dplyr::starts_with("expected")), .after=.data$before_9)

  df_bag_model

}




























