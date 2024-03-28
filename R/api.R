get_api_url <- function() {
  api_url <- Sys.getenv("RSPACE_API_URL")
  if (identical(api_url, "")) {
    cli::cli_abort(c("Please set env var {.envvar RSPACE_API_URL} to your RSpace API url",
      "(this is likely the url of your RSpace instance followed by {.code api/v1})."
    ))
  }

  api_url
}

get_api_key <- function() {
  api_key <- Sys.getenv("RSPACE_API_KEY")
  if (identical(api_key, "")) {
    cli::cli_abort(c("Please set env var {.envvar RSPACE_API_KEY} to your RSpace API key",
      "(see {.strong Authentication with an API key} at {.url https://community.researchspace.com/public/apiDocs})."
    ))
  }

  api_key
}

rspace_error_body <- function(resp) {
  resp |>
    httr2::resp_body_json() |>
    purrr::pluck("message")
}

create_global_id_link <- function(global_id) {
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
#' @export
api_status <- function(api_key = get_api_key()) {
  request() |>
    httr2::req_url_path_append("status") |>
    httr2::req_headers(`apiKey` = get_api_key()) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
}
