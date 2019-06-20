#' hdb_login
#'
#' This function allows access to a database by entering an IP adress and passcode.
#'
#' @param countries list of countries
#'
#' @examples
#' hdb_login("192.168.0.98", password = "peace123")
#'
#' @export
hdb_login = function(host = NULL,
                     user = NULL,
                     password = NULL){
  require(RPostgreSQL)
  if(is.null(host)){
    host = Sys.getenv("DB_HOST")
    if(host == ""){
      host = readline(prompt="Enter database ip address: ")
    }
  }
  if(is.null(user)){
    user = Sys.getenv("DB_USER")
    if(user == ""){
      user = readline(prompt="Enter database db user name: ")
    }
  }
  if(is.null(password)){
    pword = paste0("DB_", toupper(user), "_PASSWORD")
    password = Sys.getenv(pword)
    if(password == ""){
      password = readline(prompt="Enter db password: ")
    }
  }
  Sys.setenv(DB_HOST = host)
  Sys.setenv(DB_USER = user)
  Sys.setenv(as.name(pword) = password)
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
hdb_connect = function(db = "postgres",
                       port = 5432,
                       user = "guest"){
  require(RPostgreSQL)
  host = Sys.getenv("DB_HOST")
  pword = paste0("DB_", user, "_PASSWORD")
  password = Sys.getenv(pword)
  if(host == ""){
    hdb_login()
  }
  host = Sys.getenv("DB_HOST")
  password = Sys.getenv(pword)
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
hdb_get_toc = function(db = "master"){
  con <- hdb_connect(db)
  key = dbReadTable(con, "key")
  dbDisconnect(con)
  return(key)
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
hdb_search = function(vars, db = "master"){
  con <- hdb_connect(db)
  key = dbReadTable(con, "key")
  key = key[grep(tolower(vars), tolower(key$variablename)),]
  dbDisconnect(con)
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
  db_get = function(id){
    #print(id)
    key = hdb_get_toc()
    key = key %>% filter(uid == id)
    con <- hdb_connect(key$db)
    tmp = dbReadTable(con, key$uid)
    tmp$value = as.numeric(tmp$value)
    tmp$year = as.numeric(tmp$year)
    tmp$uid = as.character(tmp$uid)
    tmp = suppressMessages(left_join(tmp, key))
    tmp = tmp %>% select(geocode, variablename, year, value, units, description, sex, age, periodicity, source, db, last_updated_in_db)
    dbDisconnect(con)
    return(tmp)
  }
  tmp = pblapply(unique(vars$uid), db_get)
  tmp = bind_rows(tmp)
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
