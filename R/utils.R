APIcall <- function(token, url) {
  configuration <- httr::config(token = token)

  result <- try(html <- httr::GET(url,configuration))
  result <- class(result)

  if (result[1] == "try-error") {
    stop("Incorrect Token")
  }

  json <- httr::content(html)
  jsonlite::fromJSON(jsonlite::toJSON(json))
}
