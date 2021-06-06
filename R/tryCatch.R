#' tryCatch Wrapper
#'
#' A simple tryCatch wrapper that checks if a result is an error and then
#' performs some action. Useful for appending a list of failed ids to a
#' log file.
#'
#' @param expr An R expression to try
#' @param on_error An R expression to run if there was an error
#' @export
ops_tc <- function(expr, on_error, ...) {
  r <- tryCatch({
    expr
  }, error = function(e) e)

  if(inherits(r, "error")) {
    message(e$msg)
    return(on_error)
  } else {
    return(r)
  }
}
