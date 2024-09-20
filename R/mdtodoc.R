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

doc_get_fields <- function(doc_id, api_key = get_api_key()) {
  doc <- document_retrieve(doc_id, api_key)
  tibble::tibble(fields = doc$fields) |> tidyr::unnest_wider("fields")
}

add_information_to_doc_body <- function(doc_body, folder_id, tags, attachment){
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

    form_id <- parse_rspace_id(doc_to_form_id(template_id, verbose = F))
    doc_body$form = list(id = form_id)
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
