helper_get_sections <- function(){
  # Example sections objects
  sections <- data.frame(
    name = c("Title", "Name", "title", "name", "date", "results"),
    content = c("The title", "test", "small title", "smaller name", "25-09-2024", "creating fields")
  )
  return(sections)
}

helper_get_fields <- function(){
  fields <- list(
    Title   = list(name = "Title", content = "The title"),
    Name    = list(name = "Name", content = "test"),
    title   = list(name = "title", content = "small title"),
    name    = list(name = "name", content = "smaller name"),
    date    = list(name = "date", content = "25-09-2024"),
    results = list(name = "results", content = "creating fields")
  )
  return(fields)
}
