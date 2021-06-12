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

test_that("clean arrival curve works", {

  df <- tibble::tibble(
    orig = rep('DAL', 5),
    before_9 = rep(1, 5),
    min_prior_arr = round(runif(5, 5, 160)),
    total = round(runif(5, 1, 100))
  ) %>%
    clean_arrival_curve()

  expect_equal(df %>%
                 tidyr::pivot_longer(cols = !orig:before_9) %>%
                 dplyr::pull(value) %>%
                 sum(), 1)

})
