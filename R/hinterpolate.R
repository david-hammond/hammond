#' hinterpolate
#'
#' This function calculates correlations between variables
#'
#' @param countries list of countries
#'
#' @examples
#' #need 4 column data frame, geocode, variablename, year, value
#'
#' @export
hinterpolate = function(df){
  require(padr)
  df = df %>% mutate(year = as.Date(paste(year, "-01-01", sep = ""))) %>%
    padr::pad(group = c("geocode", "variablename"), interval = "year",
              start_val = as.POSIXct("2013-01-01"), end_val = as.POSIXct("2018-01-01"))
  df$year = lubridate::year(df$year)
  df = df %>% group_by(code, state, variablename) %>%
    mutate(count = zoo::na.fill(count, fill="extend"),
           pop = zoo::na.fill(pop, fill="extend"),
           rate = zoo::na.fill(rate, fill="extend")) %>%
    ungroup() %>% distinct()
  return(df)
}
