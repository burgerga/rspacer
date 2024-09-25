html_to_doc_body <- function(path, verbose = T) {
  xml <- xml2::read_html(path)
  title    <- rvest::html_element(xml, xpath = "//title") |> rvest::html_text()

  # Select sections that have a h2 child
  sections <- rvest::html_elements(xml, xpath = "//body//section[h2]")

  section_content <- purrr::map(sections, rvest::html_children)
  section_heading <- purrr::map(section_content, ~ rvest::html_text(.x[1]))
  section_content <- purrr::map(section_content, ~ .x[-1]) #< remove heading
  names(section_content) <- section_heading

  if(verbose) {
    cli::cli_inform("{.field Title}: {title}")
    purrr::iwalk(section_heading, ~ cli::cli_inform("{.field - Section {.y}}: {.x}"))
  }

  fields = purrr::imap(section_content, ~ list(
    name = .y,
    content = .x
  ))

  list(
    name = title,
    fields = fields
  )
}

data_frame_to_fields <- function(fields_data_frame) {
  fields <- lapply(1:nrow(fields_data_frame), function(row_nr) {
    list("name" = fields_data_frame$name[row_nr],
         "content" = fields_data_frame$content[row_nr])
  })
  names(fields) <- fields_data_frame$name
  return(fields)
}

excel_to_doc_body <- function(path, verbose = T, file_type = NULL) {

  if(is.null(file_type)){
    file_type = tools::file_ext(path)
  }
  sections <- switch(file_type,
                     "xlsx" = readxl::read_excel(path, col_names = c("name", "content")),
                     "csv" = readr::read_csv(path, col_names = c("name", "content")),
                     "tsv" = readr::read_tsv(path, col_names = c("name", "content"))
  )

  title    <- filter(sections, name == "Title") %>% pull(content)

  if(verbose) {
    cli::cli_inform("{.field Title}: {title}")
    purrr::iwalk(section_heading, ~ cli::cli_inform("{.field - Section {.y}}: {.x}"))
  }
  # Get a list as required by Rspace
  fields <- data_frame_to_fields(sections)
  list(
    name = title,
    fields = fields
  )
}

doc_get_fields <- function(doc_id, api_key = get_api_key()) {
  doc <- document_retrieve(doc_id, api_key)
  tibble::tibble(fields = doc$fields) |> tidyr::unnest_wider("fields")
}

attachment_upload <- function(doc_body, attachment, api_key){
  # Test if the attachment list has the correct format
  if(!is.list(attachment)) cli::cli_abort(message = c("x" = "attachment is not provided as a list"))
  if(!identical(sort(names(attachment)), c("field", "path"))) cli::cli_abort(message = c("x" = "attachment is either missing the field number or the path"))
  if(as.numeric(attachment$field) > length(doc_body$fields))  cli::cli_abort(message = c("x" = str_glue("attachment field number is higher than the total number of fields: {length(doc_body$fields)}")))

  # Upload the attachment and add its name and path to to doc_body
  json <- file_upload(attachment$path, api_key)
  doc_body$fields[[attachment$field]]$content <- glue::glue(doc_body$fields[[attachment$field]]$content,
                                                            "<p>Inserted <fileId={json$id}></p>")
  return(doc_body)
}

add_information_to_doc_body <- function(doc_body, template_id = NULL, folder_id = NULL, tags = NULL, attachment = NULL){
  if(!is.null(template_id)){
    form_id <- parse_rspace_id(doc_to_form_id(template_id, verbose = F))
    doc_body$form = list(id = form_id)
  }

  if(!is.null(folder_id)) {
    doc_body$parentFolderId <- parse_rspace_id(folder_id)
  }

  if(!is.null(tags)) {
    doc_body$tags <- paste(tags, collapse = ",")
  }

  if(!is.null(attachment)) {
    doc_body <- attachment_upload(doc_body, attachment, api_key)
  }

  # The API wants a plain array -> remove the names
  names(doc_body$fields) <- NULL
  return(doc_body)
}
#'
#' Upload html document (e.g., generated from quarto)
#'
#' @param path html document to be uploaded
#' @param template_id document id of the RSpace template used
#' @param folder_id folder_id in which the document will be created (can be a notebook)
#' @param tags vector of tags to apply to the document
#' @param attachment attachment to attach to one of the fields, e.g., `list(field = 7, path = "file.txt")`
#' @param existing_document_id if you want to replace a document by a new one, specify the current identifier here.
#' @inheritParams api_status
#' @export
document_create_from_html <- function(path, template_id = NULL, folder_id = NULL, tags = NULL, attachment = NULL, api_key = get_api_key(), existing_document_id = NULL) {
  doc_body <- html_to_doc_body(path, verbose = F)
  if(!is.null(template_id)) {
    template_fields <- doc_get_fields(template_id)

    if(length(doc_body$fields) != nrow(template_fields))
      cli::cli_abort("Document has different number of fields ({length(doc_body_fields)}) than template ({nrow(template_fields)})")
    doc_body$fields <- purrr::map2(doc_body$fields, template_fields$type, ~ {
      if(.y %in% c("string", "date")) {
        .x$content <- rvest::html_text(.x$content)
      } else {
        .x$content <- as.character(.x$content) |> paste(collapse = "\n")
      }
      .x
    })
  } else {
    # TODO Basic Document can have only 1 field
    doc_body$fields <- purrr::map(doc_body$fields, ~ {
      .x$content <- as.character(.x$content) |> paste(collapse = "\n")
      .x
    })
  }
  # Add tags, form ID and attachments to doc_body
  doc_body <- add_information_to_doc_body(doc_body, folder_id, tags, attachment)

  # Create or replace the document
  if(is.null(existing_document_id)){
    json <- document_post(doc_body)
  } else {
    json <- document_replace(doc_body, existing_document_id)
  }

  return(invisible(json))
}

#'
#' Convert a tabular document to an Rspace entry
#'
#' This function can upload Excel/csv/tabular files to Rspace structured documents.
#' The file needs to have exactly two columns, one with the Rspace structured document fields and one with the content.
#' @param path Tabular file
#' @param verbose Print information if set to TRUE.
#' @param file_type File format of the tabular file. Can be "xlsx", "csv", or "tsv" (Excel, comma separated or tab separated). If not specified, this will be guessed from the file path.
#' @inheritParams api_status
#' @returns a list with sublists, each for each row. The named list has the column headers as fields
#' @examples
#' excel_to_doc_body("assay_with_information.xlsx")
#' @export
document_create_from_excel <- function(path, template_id = NULL, folder_id = NULL, tags = NULL, attachment = NULL, api_key = get_api_key(), existing_document_id = NULL) {
  doc_body <- excel_to_doc_body(here("examples_for_upload/test_excel.xlsx"), verbose = F)

  # Add tags, form ID and attachments to doc_body
  doc_body <- add_information_to_doc_body(doc_body, template_id = template_id, folder_id = folder_id, tags = tags, attachment = attachment)

  # Create or replace the document
  if(is.null(existing_document_id)){
    json <- document_post(doc_body)
  } else {
    json <- document_replace(doc_body, existing_document_id)
  }

  return(invisible(json))
}
