#' List contents of a folder
#'
#' @param folder_id Unique identifier of the folder,
#' if NULL will return contents of the Workspace Home folder
#' @inheritParams api_status
#'
#' @export
#'
folder_tree <- function(folder_id = NULL, api_key = get_api_key()) {
  path <- list("folders", "tree")
  if (!is.null(folder_id)) path <- c(path, parse_rspace_id(folder_id))

  request() |>
    httr2::req_url_path_append(path) |>
    httr2::req_headers(`apiKey` = api_key) |>
    httr2::req_perform() |>
    httr2::resp_body_json() -> json

  tibble::tibble(records = json$records) |>
    tidyr::unnest_wider("records")
}
