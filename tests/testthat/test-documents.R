test_that("document_list_attachments", {
  local_mocked_bindings(
    get_api_key = function() {
      return("API key used for testing")
    },
    doc_get_fields = function(doc_id) {
      helper_get_fields_df()
    }
  )

  # Should have no attachments
  expect_false(document_list_attachments("123", 1, NULL))

  # Should return a tibble created with two attachments: 2 rows, 9 columns.
  expect_type(document_list_attachments("ABC", field_name = "results"), typeof(tibble::tibble()))
  expect_equal(ncol(document_list_attachments("ABC", field_name = "results")), 9)
  expect_equal(nrow(document_list_attachments("ABC", field_name = "results")), 2)

  expect_type(document_list_attachments("ABC", field_id = 4), typeof(tibble::tibble()))
  expect_equal(ncol(document_list_attachments("ABC", field_id = 4)), 9)
  expect_equal(nrow(document_list_attachments("ABC", field_id = 4)), 2)

  expect_equal(
    document_list_attachments("ABC", field_name = "results"),
    document_list_attachments("ABC", field_id = 4)
  )

  # Error if input is incorrect
  expect_error(document_list_attachments())
  expect_error(document_list_attachments("123", NULL, NULL))
  expect_error(document_list_attachments("123", "123", "NULL"))
  expect_error(document_list_attachments("123", "1", NULL))
  expect_error(document_list_attachments("123", NULL, 123))
})
