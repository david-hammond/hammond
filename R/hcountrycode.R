#' hcountrycode
#'
#' This function calculates correlations between variables
#'
#' @param countries list of countries
#'
#' @examples
#' #need 4 column data frame, iso3c, variablename, year, value
#'
#' @export

hcountrycode = function(x)
{
  require(countrycode)
  require(rio)
  x = iconv(x,from="UTF-8",to="ASCII//TRANSLIT")
  if (max(nchar(x), na.rm=T) == 3)
  {
    y = countrycode(x, "iso3c", "country.name", warn = F)
    pos = is.na(y)
    y[pos] = countrycode(x[pos], "wb", "country.name", warn = F)
  }else
  {
    y = countrycode(x, "country.name", "iso3c", warn = F)
    pos = is.na(y)
    y[pos] = countrycode(x[pos], 'country.name', 'wb', warn = F)
  }
  pos = is.na(y)
  if(sum(pos) > 1){
    message("The following countries were not matched, please rename them in your data frame if you know they have an isocode")
    message(paste(unique(x[pos]), collapse = ", "))
    unmatched = data.frame(unmatched_countries = unique(x[pos]))
    fname = "unmatched_country_codes.xlsx"
    if(file.exists(fname)){
      tmp = import(fname)
      tmp = tmp %>% rbind(unmatched) %>%
        distinct()
    }
    export(tmp, fname)
  }else{
    message("All country names converted")
  }
  return(y)
}
