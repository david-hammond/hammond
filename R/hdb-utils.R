#' hdb_login
#'
#' This function allows access to a database by entering an IP adress and passcode.
#'
#' @param countries list of countries
#'
#' @examples
#' hdb_login("192.168.0.64", db = "countrydata", user = "postgres", password = "peace123")
#'
#' @export
hdb_login = function(host = NULL,
                     db = NULL,
                     user = NULL,
                     password = NULL){
  require(RPostgreSQL)
  if(is.null(host)){
    host = Sys.getenv("DB_HOST")
    if(host == ""){
      host = readline(prompt="Enter database ip address: ")
    }
  }
  if(is.null(db)){
    db = Sys.getenv("DB_NAME")
    if(user == ""){
      db = readline(prompt="Enter database db name: ")
    }
  }
  if(is.null(user)){
    user = Sys.getenv("DB_USER")
    if(user == ""){
      user = readline(prompt="Enter database db user name: ")
    }
  }
  if(is.null(password)){
    password = Sys.getenv("DB_PASSWORD")
    if(password == ""){
      password = readline(prompt="Enter db password: ")
    }
  }
  Sys.setenv(DB_HOST = host)
  Sys.setenv(DB_NAME = db)
  Sys.setenv(DB_USER = user)
  Sys.setenv(DB_PASSWORD = password)
  con = try(hdb_connect())
  if(class(con) != "try-error"){
    message("SUCCESS")
  }else{
    message("Credentials failed")
  }
}
#' hdb_connect
#'
#' This function calculates correlations between variables
#'
#' @param countries list of countries
#'
#' @examples
#' #need 4 column data frame, geocode, variablename, year, value
#'
#' @export
hdb_connect = function(port = 5432){
  require(RPostgreSQL)
  host = Sys.getenv("DB_HOST")
  db = Sys.getenv("DB_NAME")
  user = Sys.getenv("DB_USER")
  password = Sys.getenv("DB_PASSWORD")
  if(host == "" | db == "" | user == "" | password == ""){
    hdb_login()
  }
  host = Sys.getenv("DB_HOST")
  db = Sys.getenv("DB_NAME")
  user = Sys.getenv("DB_USER")
  password = Sys.getenv("DB_PASSWORD")
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = db,
                   host = host, port = port,
                   user = user, password = password)
  return(con)
}
#' hdb_get_toc
#'
#' This function retrieves the Table of Contents from a specified database.
#'
#' @param countries list of countries
#'
#' @examples
#' #need 4 column data frame, geocode, variablename, year, value
#' hdb_login("192.168.0.98", password = "peace123")
#' hdb_get_toc()
#'
#' @export
hdb_toc = function(){
  require(scales)
  con <- hdb_connect()
  meta = dbReadTable(con, "meta")
  dbDisconnect(con)
  message("Your table of contents was successfully retrieved...")
  message(paste("Number of unique indicator names:", comma(length(unique(meta$variablename)))))
  message(paste("Number of unique indicator, sex and age combinations:", comma(nrow(meta))))
  return(meta)
}
#' hdb_search
#'
#' This function searches the database and retrieves specified data.
#'
#' @param countries list of countries
#'
#' hdb_login("192.168.0.98", password = "peace123")
#' hdb_search("Criminal)
#' @export
hdb_search = function(vars){
  key = hdb_toc()
  key1 = apply(key, 2, tolower)
  key1 = apply(key1, 1, paste0, collapse ="")
  pos = grep(tolower(vars), key1)
  key = key[pos,]
  return(key)
}

#' hdb_get
#'
#' This function retrieves and caches data from any source in the database.
#'
#' @param countries list of countries
#'
#' @examples
#' #need 4 column data frame, geocode, variablename, year, value
#' hdb_login("192.168.0.98", password = "peace123")
#' db_get("Perceptions of Criminality Raw")
#'
#' @export
hdb_get = function(vars){
  require(tidyverse)
  require(pbapply)
  db_get = function(id, con){
    #print(id)
    tmp = dbReadTable(con, id)
    tmp$value = as.numeric(tmp$value)
    tmp$uid = as.character(tmp$uid)
    tmp = suppressMessages(left_join(tmp, vars))
    tmp$year = lubridate::year(tmp$date)
    return(tmp)
  }
  con <- hdb_connect()
  tmp = pblapply(unique(vars$uid), db_get, con)
  tmp = bind_rows(tmp) %>% distinct()
  dbDisconnect(con)
  tmp = tmp %>% left_join(hammond::countryinfo)
  tmp = tmp %>% select(uid, seriescode, variablename, disaggregation, geocode, gpiname, year, date, value, description,
                 footnote, units, periodicity, income, region, government, include, landlocked, source, num_geos, earliest_yr, latest_yr, sdev, from_file,
                 from_computer, last_updated_in_db) %>% distinct()
  return(tmp)
}
#' haddcountryinfo
#'
#' This function adds country specific information to a dataframe by matching countries to country codes.
#'
#' @param countries list of countries
#'
#' @examples
#' #need 4 column data frame, geocode, variablename, year, value
#' hcountryinfo(hcountryexampledata)

#' hcountry_info
#'
#' This function calculates correlations between variables
#'
#' @param countries list of countries
#'
#' @examples
#' #need 4 column data frame, geocode, variablename, year, value
#'
haddcountryinfo = function(df){
  df = left_join(df, hcountryinfo)
  return(df)
}
