#' hpopulation
#'
#' This function appends a column of populations to a dataframe
#'
#' @param df
#'
#' @examples
#' x = hpopulation(hammond::hcountryexampledata)
#'
#' @export
hpopulation = function(df){
    require(wbstats)
    require(dplyr)
    df$year = lubridate::year(df$date)
    pop_data <- wb(indicator = "SP.POP.TOTL", startdate = min(df$year), end = max(df$year))
    pop_data = pop_data %>%
      select(iso3c, date, value) %>%
      rename(geocode = iso3c, year = date, population = value) %>%
      mutate(year = as.numeric(year))

    df = dplyr::left_join(df, pop_data)
    df = df %>% select(-year)
    return(df)
}
