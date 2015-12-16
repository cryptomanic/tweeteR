#' Followers
#'
#' Return ids of followers of an user
#'
#' @param token Access Token
#' @param username Screen Name
#'
#' @examples
#' followers(<your access token>, "xyz")
#'
#' @export

followers <- function(token = NULL, username = NULL) {
  if (is.null(token) || is.null(username)) {
    stop("Neither token nor username can be null")
  }

  followers_ids <- vector("character")
  cursor        <- -1
  url           <- "https://api.twitter.com/1.1/followers/ids.json?"
  url           <- paste0(url, "screen_name=", username, "&cursor=", cursor)

  while (TRUE) {
    data <- APIcall(token, url)
    followers_ids <- append(followers_ids, data$ids)
    if (data$next_cursor == 0 || length(followers_ids) >= 10000) {
      break
    }

    cursor <- data$next_cursor
    url <- sub("&cursor=.*$", paste0("&cursor=",cursor), url)
  }

  invisible(followers_ids)
}
