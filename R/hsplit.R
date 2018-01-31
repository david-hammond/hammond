#' Create a two factor list
#'
#' @param df name of dataframe
#' @param factor1 first factor to split on
#' @param factor2 second factor to split on
#'
#'
#' @export

split = function(df, factor1, factor2){
  df = split(df, factor(df[,factor1]))
  df = lapply(df, function(x){
    split(x, factor(x[,factor2]))
  })
  return(df)
}
