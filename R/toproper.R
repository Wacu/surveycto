### Converting word(s) to proper.

#' Converts String text into Proper Format
#'
#' @param x String or variable to be converted to proper
#'
#' @return
#' @export
#'
#' @examples
#' toproper(data$row)
toproper=function(x) {
  gsub("(?<=\\b)([a-z])", "\\U\\1", tolower(x), perl=TRUE)
}
