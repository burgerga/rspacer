test_that("add_information_to_doc_body can add information to a list", {
  # nothing added
  expect_equal(add_information_to_doc_body(list()), list())
  # template_id
  expect_error(add_information_to_doc_body(list(), template_id = "incorrect ID"))
  #TODO: how to test an API?
  #TODO below how to check a correct but non-existing template and think about the attachments
  #expect_error(add_information_to_doc_body(list(), template_id = "SD001"))
  #expect_equal(add_information_to_doc_body(list(), folder_id = "SD123", tags = "test", attachment = list("7" = "hoi.txt")))

  expect_equal(add_information_to_doc_body(list(), folder_id = "SD123", tags = "test"),
               list(parentFolderId = 123, tags = "test"))

})

test_that("excel_rspace_document_name can process a document name", {
  sections <- helper_get_sections()
  # Check document name
  expect_equal(excel_rspace_document_name(path = "minimal_excel.xlsx", sections = NULL, document_name = "thisone"), "thisone")
  expect_error(excel_rspace_document_name(path = "minimal_excel.xlsx", sections = NULL, document_name = NA))
  expect_error(excel_rspace_document_name(path = "minimal_excel.xlsx", sections = NULL, document_name = 123))
  # Check to get name from sections in the order that is in the user manual
  expect_equal(excel_rspace_document_name(path = "minimal_excel.xlsx", sections = sections, document_name = "thisone"), "thisone")
  expect_equal(excel_rspace_document_name(path = "minimal_excel.xlsx", sections = sections), "The title")
  expect_equal(excel_rspace_document_name(path = "minimal_excel.xlsx", sections = sections[2:6,]), "test")
  expect_equal(excel_rspace_document_name(path = "minimal_excel.xlsx", sections = sections[3:6,]), "small title")
  expect_equal(excel_rspace_document_name(path = "minimal_excel.xlsx", sections = sections[4:6,]), "smaller name")
  # Get the name from a file path
  expect_equal(excel_rspace_document_name(path = "some_path/minimal_excel.xlsx", sections = sections[5:6,]),
               "minimal_excel")
})

test_that("data_frame_to_fields creates a list", {
  # sections and matching list that we expect. sections and fields are defined in helper.R
  sections <- helper_get_sections()
  fields <- helper_get_fields()

  expect_equal(data_frame_to_fields(sections), fields)
  # error with incorrect input. TODO raise this error in the function itself?
  expect_error(data_frame_to_fields("this is not a data frame"))
  expect_error(data_frame_to_fields(c("this is not a data frame")))
  expect_error(data_frame_to_fields(list("this is not a data frame")))
})

test_that("excel_to_doc_body can create a doc body",{
  # one that should work
  expect_type(excel_to_doc_body(path = testthat::test_path("minimal_excel.xlsx"), verbose = F, file_type = "xlsx"), "list")
  # not existing file
  expect_error(excel_to_doc_body(path = "not_existing_file.xlsx", verbose = F, file_type = ".xlsx"))
  # incorrect excel file type specified
  expect_error(excel_to_doc_body(path = testthat::test_path("minimal_excel.xlsx"), verbose = F, file_type = ".xlsx"))
})
