tiny_df <- data.frame(
  name = c("title", "date", "results"),
  content = c("test", "25-09-2024", "creating fields")
)
test_that("data_frame_to_fields works", {
  expect_equal(
    data_frame_to_fields(tiny_df),
    list(
      title = list(name = "title", content = "test"),
      date = list(name = "date", content = "25-09-2024"),
      results = list(name = "results", content = "creating fields")
    )
  )
  # error with incorrect input. TODO raise this error in the function itself?
  expect_error(data_frame_to_fields("this is not a data frame"))
  expect_error(data_frame_to_fields(c("this is not a data frame")))
  expect_error(data_frame_to_fields(list("this is not a data frame")))
})

