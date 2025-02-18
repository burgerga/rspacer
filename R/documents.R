#' Get document from RSpace based on document ID
#'
#' @param doc_id Unique identifier of the document
#' @inheritParams api_status
#'
#' @export
#'
document_retrieve <- function(doc_id, api_key = get_api_key()) {
  request() |>
    httr2::req_url_path_append("documents", parse_rspace_id(doc_id)) |>
    httr2::req_headers(`apiKey` = api_key) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
}

document_post <- function(body, api_key = get_api_key()) {
  body$tags <- paste0(c("rspacer", body$tags), collapse = ",")
  request() |>
    httr2::req_url_path_append("documents") |>
    httr2::req_headers(`apiKey` = api_key) |>
    httr2::req_body_json(body) |>
    httr2::req_perform() |>
    httr2::resp_body_json() -> json

  cli::cli_inform("Document created: {.url {create_global_id_link(json$globalId)}}")

  json
}

document_replace <- function(body, existing_document_id, api_key = get_api_key()) {
  if(!is.null(body$tags)) {
    ifelse("rspacer" %in% stringr::str_split_1(body$tags, ","), body$tags, paste0(c("rspacer", body$tags), collapse = ","))
  }
  request() |>
    httr2::req_url_path_append("documents", parse_rspace_id(existing_document_id)) |>
    httr2::req_headers(`apiKey` = api_key) |>
    httr2::req_body_json(body) |>
    httr2::req_method("PUT") |>
    httr2::req_perform() |>
    httr2::resp_body_json() -> json

  cli::cli_inform("Document replaced: {.url {create_global_id_link(json$globalId)}}")

  return(json)
}

#'
#' Global search for a term
#'
#' Global search for a term, works identically to the simple "All" search in RSpace Workspace.
#' Must be >= 3 characters long.
#'
#' @param query  description
#' @param ... query parameters as documented in
#' <https://community.researchspace.com/public/apiDocs> \[GET /documents\]
#' @inheritParams api_status
#'
#' @export
document_search <- function(query, ..., api_key = get_api_key()) {
  request() |>
    httr2::req_url_path_append("documents") |>
    httr2::req_url_query(query = query, ...) |>
    httr2::req_headers(`apiKey` = get_api_key()) |>
    httr2::req_perform() |>
    httr2::resp_body_json() -> json

  tibble::tibble(documents = json$documents) |>
    tidyr::unnest_wider("documents")
}

#'
#' Get the form id used for a document
#'
#' @param verbose whether to print the matching document/form
#' @inheritParams document_retrieve
#' @export
doc_to_form_id <- function(doc_id, verbose = TRUE, api_key = get_api_key()) {
  json <- document_retrieve(doc_id, api_key)
  if (verbose) {
    cli::cli_inform(c(
      "{.field Document}: {json$globalId} ({json$name})",
      "{.field Form}:\t {json$form$globalId} ({json$form$name})"
    ))
  }
  json$form$globalId
}

#'
#' List attachments of a structured document
#'
#' This function lists all attachments of a field in a structured document.
#'
#' @param doc_id Unique identifier of the document
#' @param field_id Specify either `field_id` or `field_name`. Identifier for the
#' fields where attachments are listed. This identifier is relative, for example 1
#' for the top field, 2 for the second field, etc. It is not the unique field identifier.
#' @param field_name Specify either `field_id` or `field_name`. The field name
#' for which attachments need to be listed.
#' @inheritParams api_status
#' @return A data frame with identifiers and information on attachments.
#' Returns `FALSE` if no files are attached to the field.
#' @export
document_list_attachments <- function(doc_id, field_id = NULL, field_name = NULL, api_key = get_api_key()) {
  if (is.null(doc_id)) cli::cli_abort("Specify the documnt identifier `doc_id`")
  # Check field id and/or name
  if (is.null(field_id) && is.null(field_name)) cli::cli_abort("Specify `field_id` or `field_name`")
  if (!is.null(field_id) && !is.null(field_name)) cli::cli_abort("Specify only `field_id` or `field_name`")
  if (!is.null(field_id) && !is.numeric(field_id)) cli::cli_abort("`field_id` should be a number")
  if (!is.null(field_name) && !is.character(field_name)) cli::cli_abort("`field_name` should be a string")

  fields <- doc_get_fields(doc_id)
  if (!is.null(field_name)) fields <- dplyr::filter(fields, .data$name == field_name)
  if (!is.null(field_id)) fields <- fields[field_id, ]

  fields |>
    dplyr::pull(files) |>
    unlist(recursive = F) -> files
  # Return FALSE if no files are attached
  if (is.null(files)) {
    return(FALSE)
  }

  files |>
    fields_to_data_frame() -> attachment_list
  return(attachment_list)
}
