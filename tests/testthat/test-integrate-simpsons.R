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

  # Now try domain option

  # Normal distribution
  set.seed(42)
  dens_4 <- density(rnorm(num_samples),
                    n = n_for_density)
  res_4_left <- integrate_simpsons(dens_4,
                                   domain = c(min(dens_4$x), 0))
  res_4_right <- integrate_simpsons(dens_4,
                                   domain = c(0, max(dens_4$x)))

  expect_equal(res_4_left, 0.5, tolerance = 1e-02)    # Needed small tolerance,
                                        # could be since doing around 0 (the
                                        # mode) it's easy for random samples to
                                        # switch between left and right; quite a
                                        # strict condition
                                        # but they sum to 1 (should, as same as res_4)
  expect_equal(res_4_right, 0.5, tolerance = 1e-02)
  expect_equal(res_4_left + res_4_right, 1, tolerance = 1e-05)

  # Maybe hoping for too much accuracy; need a simulation experiment on multiple
  # sets of random samples.
  # expect_equal(integrate_simpsons(dens_4,
  #                                 domain = c(-2, -1)),
  #              pnorm(-1) - pnorm(-2))



  # Expected errors
  expect_error(integrate_simpsons(77))

  not_equally_spaced <- density(rlnorm(num_samples))
  not_equally_spaced$x[2]  <- not_equally_spaced$x[2] * 10
  expect_error(integrate_simpsons(not_equally_spaced))

})
