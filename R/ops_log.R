#' Initialize a logfile
#'
#' @param path The path of the output file
#' @importFrom logger log_appender appender_file
#' @export
ops_logfile <- function(path) {
  logger::log_appender(logger::appender_file(path))
}

#' Log a message
#'
#' @param msg A character string
#' @param id
#' @export
ops_loginfo <- function(msg) {
  logger::log_info(msg)
}

#' Mark a start time
#'
#' Places a variable in the function scope that allows for future time "since".
#' @export
ops_logstart <- function() {
  .ops_start_time <<- Sys.time()
}

#' Mark time since
#'
#' Time since \code{ops_logstart} was called. Use within a function for best
#' results.
#'
#' @export
ops_logtick <- function() {
  .ops_last_tick <<- Sys.time()
  paste0(
    round(
      difftime(Sys.time(), .ops_start_time, unit = "mins"),
      2),
    " mins")
}

#' Mark time since previous tick
#'
#' @export
ops_lognexttick <- function() {
  .ops_last_tick <<- Sys.time()
  paste0(
    round(
      difftime(Sys.time(), .ops_last_tick, unit = "mins"),
      2),
    " mins")
}
