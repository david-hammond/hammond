#' Factor based on order
#'
#' This makes a factor out of an ordered array
#'
#' @param x array to factorise
#' @param y order of factors
#'
#' @export

hfactor <- function(x, y = unique(x)) {
  factor(x, levels = y, ordered = T)
}
