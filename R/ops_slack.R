#' Send messages to slack
#'
#' Using an incoming webhook, sends a message to slack
#'
#' @param msg A basic character string with your message
#' @param webhook_url Your Slack App webhook's incoming URL
#' @param complex_message A more complex message as a list (will be converted
#' to JSON)
#' @export
ops_slack <- function(msg, webhook_url, complex_msg = NULL) {
  if (is.null(complex_msg)) {
    httr::POST(
      url = webhook_url,
      body = complex_msg,
      encode = "json"
    )
  } else {
    httr::POST(
      url = webhook_url,
      body = list(text = msg),
      encode = "json"
    )
  }
}
