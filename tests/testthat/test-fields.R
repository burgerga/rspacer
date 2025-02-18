test_that("data_frame_to_fields creates a list", {
  # sections and matching list that we expect. sections and fields are defined in helper.R
  sections <- helper_get_sections()
  fields <- helper_get_unnamed_fields()

  expect_equal(data_frame_to_fields(sections), fields)
  # error with incorrect input. TODO raise this error in the function itself?
  expect_error(data_frame_to_fields("this is not a data frame"))
  expect_error(data_frame_to_fields(c("this is not a data frame")))
  expect_error(data_frame_to_fields(list("this is not a data frame")))
})

test_that("fields_to_data_frame creates a tibble", {
  # sections and matching list that we expect. sections and fields are defined in helper.R
  sections <- helper_get_sections()
  fields <- helper_get_fields()

  expect_equal(fields_to_data_frame(fields), sections)
  # error with incorrect input. TODO raise this error in the function itself?
  expect_error(fields_to_data_frame("this is not a fields list"))
  expect_error(fields_to_data_frame(c("this is not a fields list")))
})

test_that("doc_get_fields creates a fields data frame", {
  local_mocked_bindings(
    get_api_key = function() {
      return("API key used for testing")
    },
    document_retrieve = function(doc_id, api_key = get_api_key()) {
      helper_larger_SD()
    }
  )
  expect_equal(
    doc_get_fields("some incorrect id"),
    fields_to_data_frame(helper_larger_SD()$fields)
  )
})

test_that("put_all_fields_in_one_field can collapse fields", {
  # Rspace needs a list with a list with at least content.
  fields <- helper_get_fields()
  expect_equal(
    put_all_fields_in_one_field(fields),
    list(list(
      content =
        "<p>The title</p>\n<p>test</p>\n<p>small title</p>\n<p>smaller name</p>\n<p>25-09-2024</p>\n<p>creating fields</p>"
    ))
  )
  expect_equal(
    put_all_fields_in_one_field(fields, use_html_sep = FALSE),
    list(list(content = "The title\ntest\nsmall title\nsmaller name\n25-09-2024\ncreating fields"))
  )
  # TODO run with fields in helper_small_SD and helper_larger_SD
})
