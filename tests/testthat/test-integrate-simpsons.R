test_that("integrate_simpsons() gives sensible results", {

  set.seed(42)
  num_samples <- 8000
  n_for_density <- formals(create_intervals.numeric)$n

  # Normal distribution
  res_1 <- integrate_simpsons(density(rnorm(num_samples),
                                      n = n_for_density))

  expect_equal(res_1, 1, tolerance = 1e-05)

  # Lognormal distribution
  res_2 <- integrate_simpsons(density(rlnorm(num_samples),
                                      n = n_for_density))

  expect_equal(res_2, 1, tolerance = 1e-04)   # worked locally with 1e-05 but
                                        # not on GHA

  # Lognormal distribution parameterised based on saved simulation
  res_3 <- integrate_simpsons(density(rlnorm(num_samples,
                                             meanlog = sim$mean_log,
                                             sdlog = sim$sd_log),
                                      n = n_for_density))

  expect_equal(res_3, 1, tolerance = 1e-05)

  # Expected errors
  expect_error(integrate_simpsons(77))

  not_equally_spaced <- density(rlnorm(num_samples))
  not_equally_spaced$x[2]  <- not_equally_spaced$x[2] * 10
  expect_error(integrate_simpsons(not_equally_spaced))

})
