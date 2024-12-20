test_that("parsing rspace ids works", {
  expect_equal(parse_rspace_id(242392), 242392)
  expect_equal(parse_rspace_id("242392"), 242392)
  expect_equal(parse_rspace_id("SD242392"), 242392)
  expect_equal(parse_rspace_id("GL258158"), 258158)
  expect_equal(parse_rspace_id("FL242398"), 242398)
  expect_equal(parse_rspace_id("NB242194"), 242194)
})

test_that("is_url works", {
  expect_true(is_url("https://leiden.researchspace.com/globalId/SD242392"))
  expect_false(is_url("SD242392"))
})
