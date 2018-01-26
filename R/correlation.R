#' hcorrelate
#'
#' This function calculates correlations between variables
#'
#' @param df name of dataframe to use for correlation, needs to be long format 4 column data frame: iso3c, variablename, year, value
#' @param min.pairs minimum number of pairs to correlate
#' @param verbose enable n and p values reporting, TRUE or FALSE
#' @param filter.by.p Do you want to filter for significant p values?
#'
#' @examples
#' #need 4 column data frame, iso3c, variablename, year, value
#' @import Hmisc,dplyr,reshape2
#' @export

h.correlate <- function(df, min.pairs = 20, verbose = TRUE, filter.by.p = FALSE) {
  df = df %>% group_by(iso3c, variablename) %>%
    filter(year == max(year)) %>% ungroup()
  df1 = dcast(df, variablename~iso3c, value.var = "value", length)
  variablename = as.character(df1[,1])
  df = dcast(df, iso3c~variablename, value.var = "value", mean)
  df = as.matrix(df)
  rownames(df) = df[,1]
  df = df[,-1]
  class(df) = "numeric"
  pos = is.infinite(as.matrix(df))
  df[pos] = NA
  pos = is.nan(as.matrix(df))
  df[pos] = NA

  #####A difference occurs because of non GPI countries
  cormatrix = rcorr(df, type = "pearson")
  cormatrix$r[cormatrix$n<=min.pairs] = NA

  if (filter.by.p) {
    cormatrix$r[cormatrix$P>0.05] = NA
  }
  rvalue = cormatrix$r
  rvalue = cbind(variablename,rvalue)
  rvalue = data.frame(rvalue)
  variablename2 = names(rvalue)[-1]
  df = melt(rvalue, "variablename")
  names(df)[3] = "r"
  df$r = as.numeric(df$r)

  if(verbose) {
    num.pairs = cormatrix$n
    num.pairs = cbind(variablename,num.pairs)
    num.pairs= data.frame(num.pairs)
    variablename2 = names(num.pairs)[-1]
    df2 = melt(num.pairs, "variablename")
    names(df2)[3] = "n"
    df2$n = as.numeric(df2$n)
    df = merge(df, df2)
    if (!filter.by.p) {
      p = cormatrix$P
      p = cbind(variablename,p)
      p= data.frame(p)
      variablename2 = names(p)[-1]
      df2 = melt(p, "variablename")
      names(df2)[3] = "p"
      df2$p = as.numeric(df2$p)
      df = merge(df, df2)
      pos = df$p<0.05
      df$signif = ""
      df$signif[pos] = "***"
    } else {
      df = sys.complete.cases(df)
    }
  }
  pos = match(df$variable, variablename2)
  df$variable = variablename[pos]
  df = iep.round(df)
  df = df %>% rename(var1 = variablename, var2 = variable)
  df
}
