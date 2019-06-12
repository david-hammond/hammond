#' hdb_login
#'
#' This function calculates correlations between variables
#'
#' @param countries list of countries
#'
#' @examples
#' #need 4 column data frame, geocode, variablename, year, value
#'
#' @export
hdb_login = function(host = NULL,
                     password = NULL){
  require(RPostgreSQL)
  if(is.null(host)){
    host = Sys.getenv("DB_HOST")
    if(host == ""){
      host = readline(prompt="Enter database ip address: ")
    }
  }
  if(is.null(password)){
    password = Sys.getenv("DB_PASSWORD")
    if(password == ""){
      password = readline(prompt="Enter database password: ")
    }
  }
  Sys.setenv(DB_HOST = host)
  Sys.setenv(DB_PASSWORD = password)
  return(NULL)
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
                       user = "postgres"){
  require(RPostgreSQL)
  host = Sys.getenv("DB_HOST")
  password = Sys.getenv("DB_PASSWORD")
  if(host == ""){
    hdb_login()
  }
  host = Sys.getenv("DB_HOST")
  password = Sys.getenv("DB_PASSWORD")
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
#' #need 4 column data frame, geocode, variablename, year, value
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
#' This function calculates correlations between variables
#'
#' @param countries list of countries
#'
#' @examples
#' #need 4 column data frame, geocode, variablename, year, value
#'
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
#' This function calculates correlations between variables
#'
#' @param countries list of countries
#'
#' @examples
#' #need 4 column data frame, geocode, variablename, year, value
#'
#' @export
hdb_get = function(vars){
  db_get = function(id){
    print(id)
    key = hdb_get_toc()
    key = key %>% filter(variablename == id)
    con <- hdb_connect(key$db[1])
    tmp = dbReadTable(con, key$tablename[1])
    tmp = tmp %>% filter(seriescode == key$seriescode[1])
    tmp$value = as.numeric(tmp$value)
    tmp$year = as.numeric(tmp$year)
    tmp$seriescode = as.character(tmp$seriescode)
    tmp = left_join(tmp, key)
    tmp = tmp %>% select(geocode, variablename, year, value, units, description, source, last_updated)
    dbDisconnect(con)
    return(tmp)
  }
  tmp = lapply(unique(vars), db_get)
  tmp = bind_rows(tmp)
  return(tmp)
}
#' hcountry_spelling
#'
#' This function calculates correlations between variables
#'
#' @param countries list of countries
#'
#' @examples
#' #need 4 column data frame, geocode, variablename, year, value
#'
hcountryspelling = function(df){
  con = hdb_connect("master")
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
#' #need 4 column data frame, geocode, variablename, year, value
#'
hcountryinfo = function(df){
  con = hdb_connect("master")
  tmp = dbReadTable(con, "country_info")
  df = left_join(df, tmp)
  dbDisconnect(con)
  return(df)
}
