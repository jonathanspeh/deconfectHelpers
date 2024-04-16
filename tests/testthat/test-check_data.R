test_that("find_suspicious_names() works", {
  test_data <- readRDS("testdata/GSE107991.RData")
  test_data_head <- head(test_data)
  expected <- readRDS("testdata/GSE107991_expected_dates.RData")
  expect_equal(find_suspicious_names(test_data, "Gene_name"), expected)
  expect_message(find_suspicious_names(test_data_head, "Gene_name"),
                 "no suspicious gene names found")
})



