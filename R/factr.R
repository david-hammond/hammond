#' Factor based on order
#'
#' This makes a factor out of an ordered array
#'
#' @param x array to factorise
#'
#' @examples load <- factr(x)
#'
#' @keywords formatting
#'
#' @export

factr <- function(x, y = unique(x)) {
  factor(x, levels = y, ordered = T)
}
