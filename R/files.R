#'
#' Upload a file to the gallery
#'
#' @param path file to be uploaded
#' @inheritParams api_status
#' @export
file_upload <- function(path, api_key = get_api_key()) {
  request() |>
    httr2::req_url_path_append("files") |>
    httr2::req_body_multipart(file = curl::form_file(path)) |>
    httr2::req_headers(`apiKey` = api_key) |>
    httr2::req_perform() |>
    httr2::resp_body_json() -> json

  cli::cli_inform("File uploaded to {.url create_global_id_link(json$globalId)}")

  json
}

#'
#' Download a file from the gallery
#'
#' @param file_id gallery file to be downloaded
#' @param path download destination
#' @inheritParams api_status
#' @export
file_download <- function(file_id, path = ".", api_key = get_api_key()) {
  if(fs::is_dir(path)) {
    # determine file name
    request() |>
      httr2::req_url_path_append("files", parse_rspace_id(file_id)) |>
      httr2::req_headers(`apiKey` = api_key) |>
      httr2::req_perform() |>
      httr2::resp_body_json() -> json

    path <- fs::path(path, json$name)
  }

  if(!can_overwrite(path)) {
    cli::cli_inform(" Cancelling download")
    return(invisible(FALSE))
  }

  request() |>
    httr2::req_url_path_append("files", parse_rspace_id(file_id), "file") |>
    httr2::req_headers(`apiKey` = api_key) |>
    httr2::req_perform(path = path) |>
    httr2::resp_check_status() -> resp
  cli::cli_inform("Downloaded to {.path {resp$body}} ({file.size(resp$body)} bytes)")
    return(invisible(TRUE))
}
