parse_rspace_id <- function(rspace_id) {
  rspace_id_parse_error <- glue::glue(
    "doc_id must be either an integer or character in one of the following forms:",
    "- 242392",
    "- \"242392\"",
    "- \"SD242392\"",
    "- \"GL258158\"",
    "- \"FL242398\"",
    # "- \"https://leiden.researchspace.com/globalId/SD242392\"",
    .sep = "\n"
  )
  rspace_id_regex <- "(?:SD|FL|GL)?(\\d+)$"

  # check if rspace_id is integer, or whole numeric
  if (is.integer(rspace_id) ||
    (is.numeric(rspace_id) && all.equal(rspace_id, as.integer(rspace_id)))) {
    return(rspace_id)
  }
  # if not a character -> error
  if (!is.character(rspace_id) || is_url(rspace_id)) stop(rspace_id_parse_error)
  if (!all(stringr::str_detect(rspace_id, rspace_id_regex))) {
    stop(rspace_id_parse_error)
  }
  as.integer(stringr::str_extract(rspace_id, rspace_id_regex, group = 1))
}


cli_hyperlink <- function(url, ...) {
  cli::style_hyperlink(url, url, ...)
}

is_url <- function(x) {
  is.character(x) && !is.null(httr2::url_parse(x)$scheme)
}

ui_yes <- function(x, .envir = parent.frame()) {
  if (!rlang::is_interactive()) stop("User input required, but session is not interactive.")

  cli::cli_inform(x, .envir = .envir)
  qs <- c("Yes", "No")
  out <- utils::menu(qs)
  out != 0L && qs[[out]] == "Yes"
}

can_overwrite <- function(path) {
  if (!fs::file_exists(path)) {
    return(TRUE)
  }

  ui_yes("Overwrite pre-existing file {.file {path}}?")
}
