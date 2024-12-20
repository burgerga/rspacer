helper_get_sections <- function() {
  # Example sections objects
  sections <- tibble::tibble(
    name = c("Title", "Name", "title", "name", "date", "results"),
    content = c("The title", "test", "small title", "smaller name", "25-09-2024", "creating fields")
  )
  return(sections)
}

helper_get_fields <- function() {
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

helper_get_unnamed_fields <- function() {
  fields <- list(
    list(name = "Title", content = "The title"),
    list(name = "Name", content = "test"),
    list(name = "title", content = "small title"),
    list(name = "name", content = "smaller name"),
    list(name = "date", content = "25-09-2024"),
    list(name = "results", content = "creating fields")
  )
  return(fields)
}

helper_get_fields_df <- function() {
  # This is the structure of an RSpace fields object with two attached files.
  files_list <- list(
    NULL, NULL, NULL,
    list(
      list(
        id = 123, globalId = "GL123", name = "fake_file1.csv", caption = NULL,
        contentType = "text/csv", created = "1970-01-01T13:01:59.154Z",
        size = 47, version = 2, "_links" = "not used now"
      ),
      list(
        id = 124, globalId = "GL124", name = "fake_file2.csv", caption = NULL,
        contentType = "text/csv", created = "1970-01-01T13:01:59.154Z",
        size = 47, version = 2, "_links" = "not used now"
      )
    ),
    NULL
  )
  # Files part is not working :(
  fields_df <- tibble::tibble(
    "id" = c(101, 102, 103, 104, 105),
    "globalId" = c("FD101", "FD102", "FD103", "FD104", "FD105"),
    "name" = c("Title", "Name", "date", "results", "other"),
    "type" = c("string", "string", "date", "text", "number"),
    "content" = c("A", "B", "01-01-1970", "<p>more text</p>", 1),
    "lastModified" = rep("1970-01-01T13:01:59.154Z", 5),
    "columnIndex" = seq(1, 5),
    "files" = files_list,
    "listOfMaterials" = rep(NA, 5),
    "_links" = rep(NA, 5)
  )
  return(fields_df)
}

helper_small_SD <- function() {
  small_SD <- list(
    form   = list(globalId = "some form ID"),
    fields = helper_get_unnamed_fields()
  )
  return(small_SD)
}

helper_larger_SD <- function() {
  larger_SD <- list(
    id = 123456,
    globalId = "SD123456",
    name = "Test upload of a document",
    tags = "rspacer,testing",
    form = list(
      id = 56789,
      globalId = "FM56789",
      name = "Assay template name"
    ),
    fields = list(
      list(id = 120, globalId = "FD120", name = "Template Used", type = "string", content = "something here"),
      list(id = 121, globalId = "FD121", name = "Title", type = "string", content = "something here"),
      list(id = 122, globalId = "FD122", name = "Date", type = "date", content = "01-01-2020"),
      list(id = 123, globalId = "FD123", name = "Text field", type = "text",
           content = "<p>A paragraph</p>\n<p>&nbsp;</p>\n<h2>And a header</h2>")
    )
  )
  return(larger_SD)
}
