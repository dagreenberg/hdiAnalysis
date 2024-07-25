test_that("intermediate_y() gives correct results", {

  expect_equal(intermediate_y(5, 4, 6, 10, 17),
               13.5)

  expect_equal(intermediate_y(9, 4, 10, 100, 120),
               116.66666666)

  expect_equal(intermediate_y(14, 10, 20, 200, 100),
               160)


  expect_error(intermediate_y(8, 10, 7, 100, 120))
  expect_error(intermediate_y(5, 6, 7, 100, 120))
  expect_error(intermediate_y(8, 6, 7, 100, 120))
})
