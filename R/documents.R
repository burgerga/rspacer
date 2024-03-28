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
  body$tags <- paste0(c("rspacer", body$tags), collapse= ",")
  request() |>
    httr2::req_url_path_append("documents") |>
    httr2::req_headers(`apiKey` = api_key) |>
    httr2::req_body_json(body) |>
    httr2::req_perform() |>
    httr2::resp_body_json() -> json

  cli::cli_inform("Document created: {.url {create_global_id_link(json$globalId)}}")

  json
}

#'
#' Global search for a term
#'
#' @param query  description
#' @param ... query parameters as documented in
#' <https://community.researchspace.com/public/apiDocs> \[GET&nbsp;/documents\]
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
doc_to_form_id <- function(doc_id, verbose = T, api_key = get_api_key()) {
  json <- document_retrieve(doc_id, api_key)
  if(verbose) {
    cli::cli_inform(c(
      "{.field Document}: {json$globalId} ({json$name})",
      "{.field Form}:\t {json$form$globalId} ({json$form$name})"
    ))
  }
  json$form$globalId
}
