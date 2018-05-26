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
#' @export

hashTag <-
  function(token = NULL, hashtag = NULL, count = 15, lang = "en") {
    if (is.null(token) || is.null(hashtag)) {
      stop('Neither token nor hashtag can be NULL')
    }

    url <- "https://api.twitter.com/1.1/search/tweets.json?q=%23"
    url <- paste0(url, hashtag, "&count=", count, "&lang=", lang)

    ldf <- APIcall(token, url)
    ldf <- ldf$statuses

    tweets    <- gsub(x = ldf$text, pattern = "https?://.+$|\\n","")
    ldf <- ldf$user

    user_id     <-  sapply(ldf$id, function(x) x)
    user_name   <-  sapply(ldf$name, function(x) x)
    location    <-  sapply(ldf$location, function(x) x)
    img_profile <<- sapply(ldf$profile_image_url, function(x) x)

    data.frame(user_id, user_name, location, tweets)
  }

#' User Information.
#'
#' General Information of User.
#'
#' @param token Access Token
#' @param user_name Screen Name
#' @param user_id User Id
#'
#' @export

userInfo <-
  function(token = NULL, user_name = NULL, user_id = NULL) {
    if (is.null(token)) {
      stop("Token cannot be null")
    }
    if (is.null(user_name) && is.null(user_id)) {
      stop("username and userid cannot be null simultaneously")
    }

    url <-
      "https://api.twitter.com/1.1/statuses/user_timeline.json?count=1&"
    if (is.null(user_id)) {
      url <- paste0(url, "screen_name=", user_name)
    } else {
      url <- paste0(url, "id=", user_id)
    }

    ldf <- APIcall(token, url)
    ldf <- ldf$user
    profile_pic <<- ldf$profile_image_url[[1]]

    list(
      id          = ldf$id[[1]],
      name        = ldf$name[[1]],
      location    = ldf$location[[1]],
      description = ldf$description[[1]],
      followers   = ldf$followers_count[[1]]
    )
  }
