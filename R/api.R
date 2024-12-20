#'
#' Get the RSpace API URL
#'
#' Gets the RSpace API URL from the `RSPACE_API_URL` environment variable.
#' See [set_api_url()] on how to set it.
#'
#' @examples
#' \dontrun{
#' get_api_url()
#' }
#'
#' @seealso [set_api_url()]
#' @export
get_api_url <- function() {
  api_url <- Sys.getenv("RSPACE_API_URL")
  if (identical(api_url, "")) {
    cli::cli_abort(c("Please set env var {.envvar RSPACE_API_URL} to your RSpace API url",
      "See {.fun rspacer::set_api_url} for more information."
    ))
  }

  api_url
}

#'
#' Set the `RSPACE_API_URL` environment variable
#'
#' Sets the `RSPACE_API_URL` environment variable to the provided RSpace API URL.
#' The RSpace API URL is likely the URL of your RSpace instance followed by `api/v1`.
#'
#' This will only set the environment variable for the current session,
#' to set it permanently, add `RSPACE_API_URL=<your_api_url_here>` to your `.Renviron` file,
#' for example, using [usethis::edit_r_environ()].
#'
#' @param rspace_api_url The RSpace API URL
#'
#' @examples
#' \dontrun{
#' set_api_url("https://leiden.researchspace.com/api/v1")
#' }
#' @export
set_api_url <- function(rspace_api_url) {
  Sys.setenv(RSPACE_API_URL = rspace_api_url)
}

#'
#' Get the RSpace API key
#'
#' Gets the RSpace API key from the `RSPACE_API_KEY` environment variable.
#' See [set_api_key()] on how to set it.
#'
#' @examples
#' \dontrun{
#' get_api_key()
#' }
#'
#' @seealso [set_api_key()]
#' @export
get_api_key <- function() {
  api_key <- Sys.getenv("RSPACE_API_KEY")
  if (identical(api_key, "")) {
    cli::cli_abort(c("Please set env var {.envvar RSPACE_API_KEY} to your RSpace API key",
      "See {.fun rspacer::set_api_key} for more information."
    ))
  }

  api_key
}

#'
#' Set the `RSPACE_API_KEY` environment variable
#'
#' Sets the `RSPACE_API_KEY` environment variable to the provided RSpace API key.
#' To create an API key go to 'Manage API Key' section of your profile page (MyRSpace -> Profile).
#'
#' This will only set the environment variable for the current session,
#' to set it permanently, add `RSPACE_API_KEY=<your_api_key_here>` to your `.Renviron` file,
#' for example, using [usethis::edit_r_environ()].
#'
#' @param rspace_api_key Your RSpace API key
#'
#' @export
set_api_key <- function(rspace_api_key) {
  Sys.setenv(RSPACE_API_KEY = rspace_api_key)
}

rspace_error_body <- function(resp) {
  resp |>
    httr2::resp_body_json() |>
    purrr::pluck("message")
}

create_global_id_link <- function(global_id) {
  # TODO: wait for https://github.com/r-lib/httr2/issues/464
  # for this function to be exported in httr2
  httr2:::url_modify(get_api_url(), path = glue::glue("globalId/{global_id}"))
}

request <- function() {
  httr2::request(get_api_url()) |>
    httr2::req_user_agent("rspacer (https://github.com/burgerga/rspacer)") |>
    httr2::req_error(body = rspace_error_body)
}


#'
#' Function to check availability of the API service
#'
#' @param api_key RSpace API key
#'
#' @examples
#' \dontrun{
#' api_status()
#' }
#'
#' @export
api_status <- function(api_key = get_api_key()) {
  request() |>
    httr2::req_url_path_append("status") |>
    httr2::req_headers(`apiKey` = get_api_key()) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
}
