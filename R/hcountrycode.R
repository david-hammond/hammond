#' hcountrycode
#'
#' This function calculates correlations between variables
#'
#' @param countries list of countries
#'
#' @examples
#' #need 4 column data frame, iso3c, variablename, year, value
#'
#' @import countrycode
#' @import dplyr
#' @import reshape2
#' @export
#'

hcountrycode = function(countries = c("ALG","ALB","UKG","CAN","USA")){
  require(countrycode)
  tmp = countrycode(countries, "iso3c", "region")
}
