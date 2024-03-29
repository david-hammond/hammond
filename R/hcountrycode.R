#' hcountrycode
#'
#' This function replaces country name or code with iso3c country codes. Can also be used in reverse.
#'
#' @param countries list of countries
#'
#' @examples
#' hcountrycode(hcountryexampledata$geocode)
#'
#' @export

hcountrycode = function(x, source_file = whereami::thisfile())
{
  require(countrycode)
  require(rio)
  require(tidyverse)
  x = iconv(x,from="UTF-8",to="ASCII//TRANSLIT")
  if (max(nchar(x), na.rm=T) == 3){
    y = countrycode(x, "iso3c", "country.name", warn = F)
    pos = is.na(y)
    y[pos] = countrycode(x[pos], "wb", "country.name", warn = F)
  }else{
    y = countrycode(x, "country.name", "iso3c", warn = F)
    pos = is.na(y)
    y[pos] = countrycode(x[pos], 'country.name', 'wb', warn = F)
    pos = is.na(y)
    if(sum(pos) >= 1){
      message("The following countries were not matched, please rename them in your data frame if you know they have an isocode")
      message(paste(unique(x[pos]), collapse = ", "))
      source_file = ifelse(is.null(source_file), "User Called", source_file)
      unmatched = data.frame(unmatched_countries = unique(x[pos]), script = source_file)
      fname = "unmatched_country_codes.xlsx"
      if(file.exists(fname)){
        tmp = rio::import(fname)
        unmatched = tmp %>% rbind(unmatched) %>%
          dplyr::distinct()
      }
      rio::export(unmatched, fname, overwrite = T)
    }else{
      message("All country names converted")
    }
  }

  return(y)
}
