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
#' @examples
#' change <- iep.pc.change(df)
#'
#' @keywords utilities analysis-utils
#'
#' @export

h.pc.change <- function(all) {
  ipak('scales')
  temp <- expand.grid(iso3c = unique(all$iso3c), variablename = unique(all$variablename),
                      from = unique(all$year), to = unique(all$year))
  temp$num.years <- as.numeric(temp$to) - as.numeric(temp$from)
  temp <- subset(temp, num.years > 0)
  all <- subset(all, select = c(iso3c, year, variablename, value))
  names(all) <- c("iso3c", "from" , "variablename", "from.value")
  temp <- merge(temp, all)
  names(all) <- c("iso3c", "to" , "variablename", "to.value")
  temp <- merge(temp, all)
  temp$raw.proportional.change <- round(with(temp, (to.value-from.value)/from.value),3)
  #temp$raw.growth = percent(temp$raw.proportional.change)
  temp$annual.proportional.change <- round(with(temp, ((to.value/from.value)^(1/num.years))-1),3)
  #temp$annual.growth = percent(temp$annual.proportional.change)
  #temp$country = iep.gpi.country.spelling(temp$iso3c)
  temp <- temp[, c("iso3c", "variablename", "num.years", "from",
                   "from.value", "to", "to.value", "raw.proportional.change",
                   "annual.proportional.change") ]
  names(temp) <- c("iso3c", "variablename", "num.years", "from",
                   "from.value", "to", "to.value", "prop.growth",
                   "annual.prop.growth")
  temp$pc.growth = percent(temp$prop.growth)
  temp$annual.pc.growth = percent(temp$annual.prop.growth)
  temp$iso3c = as.character(temp$iso3c)
  temp$variablename = as.character(temp$variablename)
  return(temp)
}
