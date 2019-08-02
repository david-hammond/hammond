#' hcorr
#'
#' This function calculates correlations between variables
#'
#' @param df name of dataframe to use for correlation, needs to be long format 4 column data frame: geocode, variablename, year, value
#' @param min.pairs minimum number of pairs to correlate
#' @param verbose enable n and p values reporting, TRUE or FALSE
#' @param filter.by.p Do you want to filter for significant p values?
#'
#' @examples
#' #need 4 column data frame, geocode, variablename, year, value
#' library(hammond)
#' corr = hcorr(hcountryexampledata)
#'
#' @export


hcorr <- function(df, min.pairs = 20, verbose = TRUE, filter.by.p = FALSE) {
  require(Hmisc)
  require(reshape2)
  require(dplyr)
  df = df %>% dplyr::group_by(geocode, variablename) %>%
    dplyr::filter(year == max(year)) %>% ungroup()
  df1 = dcast(df, variablename~geocode, value.var = "value", length)
  variablename = as.character(df1[,1])
  df = dcast(df, geocode~variablename, value.var = "value", mean)
  df = as.matrix(df)
  rownames(df) = df[,1]
  df = df[,-1]
  class(df) = "numeric"
  pos = is.infinite(as.matrix(df))
  df[pos] = NA
  pos = is.nan(as.matrix(df))
  df[pos] = NA

  #####A difference occurs because of non GPI countries
  cormatrix = Hmisc::rcorr(df, type = "pearson")
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
      df = df %>% filter(complete.cases(.))
    }
  }
  pos = match(df$variable, variablename2)
  df$variable = variablename[pos]
  df = df %>% dplyr::rename(var1 = variablename, var2 = variable)
  df = df %>% filter(var1!=var2)
  df
}

#' hdb_corr
#'
#' This function calculates correlations between variables
#'
#' @param df name of dataframe to use for correlation, needs to be long format 4 column data frame: geocode, variablename, year, value
#'
#' @examples
#' #need 4 column data frame, geocode, variablename, year, value
#' library(hammond)
#' corr = hcorr(hcountryexampledata)
#'
#' @export

hdb_corr <- function(df) {
  require(Hmisc)
  require(dplyr)
  df = df %>% dplyr::group_by(geocode, uid) %>%
    dplyr::filter(date == max(date)) %>%
    summarise(value = mean(value)) %>% ungroup() %>%
    select(uid, geocode, value)
  df = df %>% spread(uid, value) %>%
    filter(complete.cases(.))
  if(nrow(df) > 4){
    sdev = apply(df[,-1], 2, sd)
    if (min(sdev) > 0){
      df = as.matrix(df)
      df = df[,-1]
      class(df) = "numeric"
      pos = is.infinite(as.matrix(df))
      df[pos] = NA
      pos = is.nan(as.matrix(df))
      df[pos] = NA
      cormatrix = Hmisc::rcorr(df, type = "pearson")
      format_cormatrix = function(i){
        df = cormatrix[[i]] %>% as.data.frame() %>%
          mutate(uid1 = rownames(cormatrix[[i]])) %>%
          gather(uid2, val, -uid1) %>%
          mutate(val = round(val, 2))
        names(df)[3] = i
        return(df)
      }

      cormatrix = lapply(names(cormatrix), format_cormatrix)

      df = suppressMessages(cormatrix[[1]] %>% left_join(cormatrix[[2]]) %>%
                              left_join(cormatrix[[3]]))
      df = df %>% dplyr::filter(uid1!=uid2)
      return(df)
    }else{
      message("No variation of variables")
      break
    }

  }else{
    message("Not enough Pairs")
    break
  }
}
