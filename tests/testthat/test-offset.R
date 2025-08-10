test_that("offset size equals offset seq length", {

  expect_equal(offset(100, 10), length(offset(100, 10, "seq")))
  expect_equal(offset(47984, 5000), length(offset(47984, 5000, "seq")))
  expect_equal(offset(147984, 2000), length(offset(147984, 2000, "seq")))

})
