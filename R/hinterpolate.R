#' hinterpolate
#'
#' This function calculates correlations between variables
#'
#' @param countries list of countries
#'
#' @examples
#' #need 4 column data frame, geocode, variablename, year, value
#'
interpolate.data <- function(df) {
  # linear interpolation

  # need at least 2 non-na values to interpolate
  if (nrow(df[!is.na(df$value),]) > 0) {
    xout <- 1:nrow(df)
    y <- df$value
    interpolation <- approx(x = xout[!is.na(y)], y = y[!is.na(y)], xout = xout,
                            rule = 2)
    df$yhat <- interpolation$y
    #fix for interploation for only 1 dfset of consecutive years
    df$yhat = ifelse(is.na(df$yhat), df$value, df$yhat)
  }

  return(df)
}

#' Interpolates data
#'
#' This is a wrapper function takes a data frame and fills in
#' interpolated and extrapolated data for the whole time series
#'
#' @param df dataframe in iep format
#'
#' @return Returns list with filled in time series, column yhat is the interpolated value.
#' Please check original value with yhat column to make sure you are happy with the results
#'
#' @examples
#' hinterpolate(hcountryexampledata)
#'
#' @keywords imputation
#' @author Dave
#' @export
hinterpolate = function(df){
  require(tidyverse)
  require(padr)
  require(lubridate)
  require(testthat)
  #need to add another variable for entries with only one country-year-car value,
  #interpolate breaks otherwise
  df = df %>% select(geocode, variablename, year, value)
  if(diff(range(df$year))>0){
    num.years.test = df %>% group_by(geocode, variablename) %>%
      summarise(only.one.yr = min(year) == max(year), year = min(year), value = max(value)) %>%
      filter(only.one.yr)
    num.years.test$year = ifelse(num.years.test$year == min(df$year), max(df$year), min(df$year))
    df = bind_rows(df, num.years.test[, names(df)])

    #extend data frame
    df$date = as.Date(paste0(df$year, "-01-01"))
    df = pad(df, start_val = min(df$date),
             end_val = max(df$date), group = c("geocode", "variablename"), interval = "year")
    df$year = lubridate::year(df$date)
    df = df %>% select(-date)

    #test
    num.years.test = df %>% group_by(geocode, variablename) %>%
      summarise(only.one.yr = min(year) == max(year))
    testthat::expect_that(sum(num.years.test$only.one.yr), equals(0),
                info = "You Have only One Entry for Interploation")
    #interpolate
    df <- df %>% group_by(geocode, variablename) %>% do(interpolate.data(.))
    df = as.data.frame(df)
  }else{
    message("***You only have one year of data across all indicators")
    message("***No interpolation or extrapolation possible or needed")
    message("***Original data frame returned untouched")
  }

  return(df)
}

