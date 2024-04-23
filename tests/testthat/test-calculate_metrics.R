test_that("calculation of rmse works", {
  actual <- data.frame(id = 1:10, proportion = c(1,3,5,7,9,10,13,16,18,20))
  deconvolution <- data.frame(id = 1:10, proportion = c(2,3,4,6,7,8,12,17,19,21))
  expected <- round(1.224745, 5)
  expect_equal(round(get_rmse(actual, deconvolution), 5), expected)
})
