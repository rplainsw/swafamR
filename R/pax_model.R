#' Distribute Pax and Bag Demand
#'
#' Take the vector of expected demands, and multiply them across the
#' matrix of arrival curve percentages. This places the proportion of expected
#' demand in the given minutes prior increments. The data is pivoted to
#' subtract the minutes prior from departure datetime, and summarized by
#' the appropriate times. This returns a single time increment for each origin.
#'
#' @param df_bag_model dataframe
#' @param vector_name character
#' @param col_name character
distribute_demand <- function(df_bag_model, vector_name, col_name) {

  pax <- df_bag_model

  arrival_curve <- pax %>%
    dplyr::select(.data$expected_pax_wbag:.data$`240`) %>%
    dplyr::select(-.data$expected_pax_wbag) %>%
    dplyr::mutate_all(~replace(., is.na(.), 0))

  df <- pax %>%
    dplyr::select(.data$date_of_dep, .data$carrier, .data$orig, .data$dep_dttm) %>%
    cbind(
      pax %>%
        dplyr::pull(vector_name) * arrival_curve %>%
        data.frame()
    ) %>%
    dplyr::as_tibble() %>%
    tidyr::pivot_longer(
      cols = dplyr::starts_with("X"),
      names_to = 'minutes_prior',
      values_to = col_name
    ) %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(minutes_prior = as.numeric(stringr::str_replace(.data$minutes_prior, "X", ""))) %>%
    dplyr::mutate(time = .data$dep_dttm - lubridate::minutes(.data$minutes_prior)) %>%
    dplyr::group_by(.data$orig, .data$carrier, .data$time) %>%
    dplyr::summarise(col_name = sum(!! sym(col_name), na.rm = TRUE), .groups = 'drop')

  df

}


#' Pax Modle
#'
#' Applies the distribute_demand to each vector and creates a joined output.
#'
#' @param df_bag_model dataframe
#' @export
pax_model <- function(df_bag_model) {

  df1 <- distribute_demand(df_bag_model, "expected_bag", "bag_demand") %>%
    dplyr::rename(bag_demand = .data$col_name) %>%
    dplyr::arrange(.data$time)
  df2 <- distribute_demand(df_bag_model, "expected_pax", "pax_demand") %>%
    dplyr::rename(pax_demand = .data$col_name)
  df3 <- distribute_demand(df_bag_model, "expected_pax_wbag", "pax_wbag_demand") %>%
    dplyr::rename(pax_wbag_demand = .data$col_name)

  df_pax <- df1 %>%
    dplyr::left_join(df2, by=c('orig','carrier','time')) %>%
    dplyr::left_join(df3, by=c('orig','carrier','time'))

  df_pax
}
