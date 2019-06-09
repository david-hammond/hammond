#' hdb_connect
#'
#' This function calculates correlations between variables
#'
#' @param countries list of countries
#'
#' @examples
#' #need 4 column data frame, iso3c, variablename, year, value
#'
#' @export
hdb_connect = function(db = "postgres",
                       host = my.name <- readline(prompt="Enter database ip address: "),
                       port = 5432,
                       user = "postgres",
                       password = my.name <- readline(prompt="Enter database password: ")){
  require(RPostgreSQL)
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = db,
                   host = host, port = port,
                   user = user, password = password)
  return(con)
}
#' hdb_get_toc
#'
#' This function calculates correlations between variables
#'
#' @param countries list of countries
#'
#' @examples
#' #need 4 column data frame, iso3c, variablename, year, value
#'
#' @export
hdb_get_toc = function(db = "master", host = NULL, password = NULL){
  con <- hdb_connect(db, host, password)
  key = dbReadTable(con, "key")
  dbDisconnect(con)
  return(key)
}
#' hdb_search
#'
#' This function calculates correlations between variables
#'
#' @param countries list of countries
#'
#' @examples
#' #need 4 column data frame, iso3c, variablename, year, value
#'
#' @export
hdb_search = function(vars, db = "master", host = NULL, password = NULL){
  con <- hdb_connect(db)
  key = dbReadTable(con, "key")
  key = key[grep(tolower(vars), tolower(key$variablename)),]
  dbDisconnect(con)
  return(key)
}
#' hdb_get
#'
#' This function calculates correlations between variables
#'
#' @param countries list of countries
#'
#' @examples
#' #need 4 column data frame, iso3c, variablename, year, value
#'
#' @export
hdb_get = function(vars, host = NULL, password = NULL){
  db.get = function(id){
    print(id)
    key = hdb_get_toc(host, password)
    key = key %>% filter(variablename == id)
    con <- hdb_connect(key$db[1], host, password)
    tmp = dbReadTable(con, key$tablename[1])
    tmp = tmp %>% filter(seriescode == key$seriescode[1])
    tmp$value = as.numeric(tmp$value)
    tmp$year = as.numeric(tmp$year)
    tmp$seriescode = as.character(tmp$seriescode)
    tmp = left_join(tmp, key)
    tmp = tmp %>% select(iso3c, variablename, year, value, source)
    dbDisconnect(con)
    return(tmp)
  }
  tmp = pblapply(unique(vars), db.get)
  tmp = bind_rows(tmp)
  if("national" %in% tmp$geolevel){
    tmp = hcountry_spelling(tmp, host = NULL, password = NULL)
    tmp = hcountry_info(tmp, host = NULL, password = NULL)
  }
  return(tmp)
}
#' hcountry_spelling
#'
#' This function calculates correlations between variables
#'
#' @param countries list of countries
#'
#' @examples
#' #need 4 column data frame, iso3c, variablename, year, value
#'
#' @export
hcountry_spelling = function(df, host = NULL, password = NULL){
  con = hdb_connect("master", host, password)
  tmp = dbReadTable(con, "country_spelling")
  df = left_join(df, tmp)
  dbDisconnect(con)
  return(df)
}
#' hcountry_info
#'
#' This function calculates correlations between variables
#'
#' @param countries list of countries
#'
#' @examples
#' #need 4 column data frame, iso3c, variablename, year, value
#'
#' @export
hcountry_info = function(df, host = NULL, password = NULL){
  con = hdb_connect("master", host, password)
  tmp = dbReadTable(con, "country_info")
  df = left_join(df, tmp)
  dbDisconnect(con)
  return(df)
}
