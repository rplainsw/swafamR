test_that("clean load factors work", {

  df <- tibble::tibble(
    orig = rep('ABQ', 5),
    dest = c('DAL', 'DAL', 'DAL', 'DEN', 'DEN'),
    date_of_dep = rep('2019-06-10', 5),
    dep_int = round(runif(5, 900, 959)),
    load_factor = round(runif(5),2),
    orig_factor = round(runif(5), 2)
  ) %>%
    clean_load_factors()


  expect_equal(nrow(df), 3)
})

test_that("update stations works", {

  df <- tibble::tibble(
    orig = rep('FLL', 5),
    dest = c('DAL', 'DAL', 'DAL', 'DEN', 'DEN'),
    date_of_dep = rep('2019-06-10', 5),
    dep_int = round(runif(5, 900, 959)),
    load_factor = round(runif(5),2),
    orig_factor = round(runif(5), 2)
  ) %>%
    update_new_stations() %>%
    dplyr::count(.data$orig)

  expect_equal(nrow(df), 2)

})
