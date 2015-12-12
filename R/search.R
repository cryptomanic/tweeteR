#' Hash Tag Twitter.
#'
#' Search for tweets having particular hashtag.
#'
#' @param token Access Token
#' @param hashtag Hash Tag
#' @param count Maximum number of tweets to return per page,
#'              up to a maximum of 100. Defaults to 15.
#' @param lang Restricts tweets to the given language,
#'             given by an ISO 639-1 code
#'
#' @examples
#' hashTag(token, "oldisgold")
#' hashTag(token, "oldisgold", count = 10)
#' hashTag(token, "oldisgold", count = 50, lang = "zh")
#' @export

hashTag <- function(token = NULL, hashtag = NULL, count = 15, lang = "en") {
  if (is.null(token) || is.null(hashtag)) {
    stop('Neither token nor hashtag can be NULL')
  }

  url           <- "https://api.twitter.com/1.1/search/tweets.json?q=%23"
  url           <- paste0(url, hashtag, "&count=", count, "&lang=", lang)
  configuration <- httr::config(token = token)

  result <- try(html <- httr::GET(url,configuration))
  result <- class(result)

  if (result[1] == "try-error") {
    stop("Incorrect Token")
  }

  json <- httr::content(html)
  ldf  <- jsonlite::fromJSON(jsonlite::toJSON(json))

  tweets    <- gsub(x = ldf$statuses$text, pattern = "https?://.+$|\\n","")
  user_id   <- sapply(ldf$statuses$user$id, function(x) x)
  user_name <- sapply(ldf$statuses$user$name, function(x) x)
  location  <- sapply(ldf$statuses$user$location, function(x) x)

  data.frame(user_id, user_name, location, tweets)
}
