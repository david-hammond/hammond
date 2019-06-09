#' Calculate proportional change
#'
#' This function calculates proportional change in GPI for a country
#' from one year to another.
#'
#' @param all the dataframe to be processed
#'
#' @return Returns a dataframe containing the raw and annual growths in GPI
#' for each country
#'
#'
#' @keywords utilities analysis-utils
#'
#' @export

hpc_change <- function(all) {
  ipak('scales')
  all$year = as.numeric(all$year)
  all$value = as.numeric(all$value)
  temp <- expand.grid(geocode = unique(all$geocode), variablename = unique(all$variablename),
                      from = unique(all$year), to = unique(all$year))
  temp$num.years <- as.numeric(temp$to) - as.numeric(temp$from)
  temp <- subset(temp, num.years > 0)
  all <- subset(all, select = c(geocode, year, variablename, value))
  names(all) <- c("geocode", "from" , "variablename", "from.value")
  temp <- merge(temp, all)
  names(all) <- c("geocode", "to" , "variablename", "to.value")
  temp <- merge(temp, all)
  temp$absolute.diff = round(with(temp, (to.value-from.value)),3)
  temp$raw.proportional.change <- round(with(temp, (to.value-from.value)/from.value),3)
  temp$annual.proportional.change <- round(with(temp, ((to.value/from.value)^(1/num.years))-1),3)
  temp <- temp[, c("geocode", "variablename", "num.years", "from",
                   "from.value", "to", "to.value", "absolute.diff", "raw.proportional.change",
                   "annual.proportional.change") ]
  names(temp) <- c("geocode", "variablename", "num.years", "from",
                   "from.value", "to", "to.value", "absolute.diff", "prop.growth",
                   "annual.prop.growth")
  temp$geocode = as.character(temp$geocode)
  temp$variablename = as.character(temp$variablename)
  return(temp)
}
