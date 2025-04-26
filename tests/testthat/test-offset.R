test_that("offset_size equals offset_seq length", {

  off_len <- \(n, limit) length(offset_seq(n, limit))

  expect_equal(offset_size(100, 10), off_len(100, 10))
  expect_equal(offset_size(47984, 5000), off_len(47984, 5000))
  expect_equal(offset_size(147984, 2000), off_len(147984, 2000))
})
