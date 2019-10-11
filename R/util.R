#' Write a stderr message with a leading date/time stamp
#'
#' @param x a character object with the message to display
#'
#' @return no return
#' @export
stm <- function(x) {
  write(paste0("[",Sys.time(),"] ",x), stderr())
}
