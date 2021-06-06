#' Send an operational email
#'
#' Used to warn of process success or failure. Use sparingly.
#'
#' @param msg A string containing the markdown message you wish to send
#' @param to The recipient.
#' @param from The sender. If NULL, defaults to the recipient specified in "to"
#' @importFrom blastula compose_email md smtp_send creds_anonymous
#' @export
ops_mail <- function(mgs, subject, from, to = NULL) {

  if(is.null(to)) {
    to = from
  }

  email <- blastula::compose_email(body = blastula::md(
    msg
  ))

  email %>% blastula::smtp_send(
    to = to,
    from = from,
    subject = subject,
    credentials = blastula::creds_anonymous(
      host = "localhost",
      use_ssl = FALSE,
      port = "25"
    )
  )
}

