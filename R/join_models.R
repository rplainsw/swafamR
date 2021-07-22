#' @export
join_tpoint_models <- function(df_bag_model, minute_bin = 5) {

  df_bag_model %>%
    tpoint_model(cart_staging_time = 120, minute_bin = minute_bin) %>%
    dplyr::left_join(df_bag_model %>%
                tpoint_model(cart_staging_time = 150, minute_bin = minute_bin),
              by=c('orig','carrier','time'),
              suffix=c("", "_150")
    ) %>%
    dplyr::left_join(df_bag_model %>%
                tpoint_model(cart_staging_time = 180, minute_bin = minute_bin),
              by=c('orig','carrier','time'),
              suffix=c("","_180")
    )

}

#' @export
join_increments <- function(df_final_output) {

  counter <- list(

    bags = df_final_output %>%
      increment_logic('total_bag_demand' , 'bag_capacity') %>%
      dplyr::rename(bag_counter = .data$x),

    reg_check = df_final_output %>%
      dplyr::mutate(wait_goal = 10) %>%
      increment_logic('reg_check_wait_time','wait_goal')%>%
      dplyr::rename(reg_check_counter = .data$x),

    pre_check = df_final_output %>%
      dplyr::mutate(wait_goal = 10) %>%
      increment_logic('pre_check_wait_time', 'wait_goal')%>%
      dplyr::rename(pre_check_counter = .data$x),


    skycap = df_final_output %>%
      dplyr::filter(.data$carrier == 'WN') %>%
      dplyr::mutate(wait_goal = 10) %>%
      increment_logic('skycap_wait_time', 'wait_goal')%>%
      dplyr::rename(skycap_counter = .data$x),

    stk = df_final_output %>%
      dplyr::filter(.data$carrier == 'WN') %>%
      dplyr::mutate(stk_capacity = 1) %>%
      increment_logic('stk_wait_time','stk_capacity')%>%
      dplyr::rename(stk_counter = .data$x) ,


    full_service = df_final_output %>%
      dplyr::filter(.data$carrier == 'WN') %>%
      dplyr::mutate(wait_goal = 10) %>%
      increment_logic('full_service_wait_time', 'wait_goal')%>%
      dplyr::rename(full_service_counter = .data$x),

    bag_drop = df_final_output %>%
      dplyr::filter(.data$carrier == 'WN') %>%
      dplyr::mutate(wait_goal = 2) %>%
      increment_logic('bag_drop_wait_time', 'wait_goal')%>%
      dplyr::rename(bag_drop_counter = .data$x),

    gate = df_final_output %>%
      dplyr::filter(.data$carrier == 'WN') %>%
      increment_logic('all', 'gate_count')%>%
      dplyr::rename(gate_counter = .data$x),

    parallel = df_final_output %>%
      dplyr::filter(.data$carrier == 'WN') %>%
      increment_logic('parrallel_carts', 'tpoint_capacity_lf')%>%
      dplyr::rename(parallel_counter = .data$x),

    parallel_150 = df_final_output %>%
      dplyr::filter(.data$carrier == 'WN') %>%
      increment_logic('parrallel_carts_150', 'tpoint_capacity_lf')%>%
      dplyr::rename(parallel_counter_150 = .data$x),

    parallel_180 = df_final_output %>%
      dplyr::filter(.data$carrier == 'WN') %>%
      increment_logic('parrallel_carts_180', 'tpoint_capacity_lf')%>%
      dplyr::rename(parallel_counter_180 = .data$x),

    perpendicular = df_final_output %>%
      dplyr::filter(.data$carrier == 'WN') %>%
      increment_logic('perpendicular_carts', 'tpoint_capacity_lf')%>%
      dplyr::rename(perpendicular_counter = .data$x),

    perpendicular_150 = df_final_output %>%
      dplyr::filter(.data$carrier == 'WN') %>%
      increment_logic('perpendicular_carts_150', 'tpoint_capacity_lf')%>%
      dplyr::rename(perpendicular_counter_150 = .data$x),

    perpendicular_180 = df_final_output %>%
      dplyr::filter(.data$carrier == 'WN') %>%
      increment_logic('perpendicular_carts_180', 'tpoint_capacity_lf')%>%
      dplyr::rename(perpendicular_counter_180 = .data$x),

    bags_on_carousel = df_final_output %>%
      dplyr::filter(.data$carrier == 'WN', carousel_capacity > 0) %>%
      increment_logic('bags_on_carousel_120', 'carousel_capacity')%>%
      dplyr::rename(carousel_counter = .data$x),

    bags_on_carousel_150 = df_final_output %>%
      dplyr::filter(.data$carrier == 'WN', carousel_capacity > 0) %>%
      increment_logic('bags_on_carousel_150', 'carousel_capacity')%>%
      dplyr::rename(carousel_counter_150 = .data$x),

    bags_on_carousel_180 = df_final_output %>%
      dplyr::filter(.data$carrier == 'WN', carousel_capacity > 0) %>%
      increment_logic('bags_on_carousel_180', 'carousel_capacity')%>%
      dplyr::rename(carousel_counter_180 = .data$x)

  )  %>%
    purrr::reduce(dplyr::full_join, by = "station") %>%
    dplyr::mutate_at(dplyr::vars(dplyr::contains("count")), ~replace(., is.na(.), 0))


  return(counter)
}

#' Main Function
#'
#' Runs all the models and joins them together for a typical model run
#'
#' @param mod_inputs list
#' @param minute_bin integer
#' @export
main_model <- function(mod_inputs, minute_bin = 5) {

  message("Bag Model")
  df_bag_model <- bag_model(mod_inputs = mod_inputs, minute_bin = minute_bin)

  message("Tpoint Model")
  df_tpoint_model  <- df_bag_model %>%
    join_tpoint_models(minute_bin = minute_bin)

  message("Gate Model")
  df_gate_model <- mod_inputs %>%
    gate_model(minute_bin = minute_bin)

  message("Pax Model")
  df_pax_model <- df_bag_model %>%
    pax_model()

  message("Service Stations")
  df_service_stations_model <- df_pax_model %>%
    service_station_model(mod_inputs, minute_bin = minute_bin)

  message("Checkpoint Model")
  df_checkpoint_model <- df_pax_model %>%
    checkpoint_model(mod_inputs, minute_bin = minute_bin)

  message("Carousel Model")
  df_carousel_model <- carousel_model(df_bag_model, minutes_prior_bag_pickup = 120) %>%
    dplyr::left_join(carousel_model(df_bag_model, 150),
              by=c("orig","carrier","time"), suffix=c("_120","_150"))  %>%
    dplyr::left_join(carousel_model(df_bag_model, 180),
              by=c("orig","carrier","time")) %>%
    dplyr::left_join(mod_inputs$assumptions %>%
                carousel_capacity(), by=c("orig"="station"))

  message("Final Output")
  df_final_output <- df_pax_model %>%
    dplyr::left_join(df_gate_model, by=c('orig','time', 'carrier')) %>%
    dplyr::left_join(df_tpoint_model, by=c('orig','time','carrier')) %>%
    dplyr::left_join(df_checkpoint_model, by=c('orig','time','carrier')) %>%
    dplyr::left_join(df_service_stations_model %>%
                dplyr::select(-.data$bag_demand, -.data$pax_demand, -.data$pax_wbag_demand, -.data$total_bag_demand),
              by=c('orig', 'time', 'carrier')) %>%
    dplyr::left_join(df_carousel_model, by=c('orig','time','carrier')) %>%
    dplyr::rename(bags_on_carousel_180 = .data$bags_on_carousel) %>%
    dplyr::left_join(mod_inputs$assumptions %>%
                generate_capacities(),
              by=c('orig'='station')) %>%
    dplyr::full_join(mod_inputs$flight_schedule %>%
                flight_count(),
              by=c('orig', 'carrier', 'time'='dep_dttm')) %>%
    dplyr::left_join(mod_inputs$flight_schedule %>%
                plane_type_counts(),
              by=c('orig','carrier')) %>%
    dplyr::left_join(mod_inputs$shared_checkpoints, by=c('orig'='station','carrier')) %>%
    dplyr::mutate(
      bag_demand = ifelse(.data$carrier == 'WN', .data$bag_demand, .data$bag_demand * .data$cbis),
      run_date = Sys.Date(),
      weekday = lubridate::wday(.data$time, label = TRUE, abbr = FALSE),
      model_date = lubridate::as_date(.data$time),
      schedule = lubridate::month(.data$time, label = TRUE, abbr = FALSE),
      report_type = 'Scheduled',
      year = lubridate::year(.data$time)) %>%
    dplyr::rename(station = .data$orig)

  df_final_output <- df_final_output %>%
    dplyr::left_join(df_final_output %>% join_increments(), by='station') %>%
    dplyr::mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
    dplyr::mutate_if(is.integer, ~replace(., is.na(.), 0))


}

