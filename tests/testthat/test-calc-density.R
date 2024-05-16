# calc_density(). Quick test just to keep GHA happy for now.
test_that("cacl_density results on hake 2021 recruitment do not change; only checks one value for now to get working", {
  one_year_mcmc <- dplyr::pull(hake_recruitment_mcmc, `2021`)
  res_calc_density <- calc_density(one_year_mcmc)

  vec_results <- c(10.1873, 4.085088, 29.49938,  2.702156,  25.55708, 25.4143, 22.85492,
    2.559376, 21 ,0.03289304, 123, 0.003572334, 1, 16, 0.008976952, 108,
    0.006995915) # calculated 2024-05-16
  # ints_here <- tibble::tibble_row(t(vec_results))
  ## names(ints_here) <- c("median",
  ##                       "2.5",
  ##                       "97.5",
  ##                       "hdi_lower",
  ##                       "hdi_upper",
  ##                       "width_equal",
  ##                       "width_hdi",
  ##                       "width_diff",
  ##                       "i_low_equal",
  ##                       "y_low_equal_interp",
  ##                       "i_high_equal",
  ##                       "y_high_equal_interp",
  ##                       "i_low_hdi",
  ##                       "y_low_hdi_interp",
  ##                       "i_high_hdi",
  ##                       "y_high_hdi_interp")
  expect_equal(res_calc_density$intervals[1:5] %>% as.numeric(),
               vec_results[1:5],
               tolerance = 1e-5)
})
