#' hdb_login
#'
#' This function calculates correlations between variables
#'
#' @param countries list of countries
#'
#' @examples
#' hdb_login("192.168.0.98", password = "peace123")
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
#' This function calculates correlations between variables
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
#' This function calculates correlations between variables
#'
#' @param countries list of countries
#'
#' @examples
#' #need 4 column data frame, geocode, variablename, year, value
#' hdb_login("192.168.0.98", password = "peace123")
#' db_get("Perceptions of Criminality Raw")
#'
#' @export
hdb_get = function(uids){
  require(pbapply)
  key = hdb_get_toc()
  key = key %>% filter(uid %in% uids)
  db_get = function(db){
    tmp <- key %>% filter(db == db)
    con <- hdb_connect(db)
    all = NULL
    for (id in unique(tmp$uid)){
      tmp = dbReadTable(con, id)
      tmp$value = as.numeric(tmp$value)
      tmp$year = as.numeric(tmp$year)
      tmp$uid = as.character(tmp$uid)
      tmp = suppressMessages(left_join(tmp, key))
      tmp = tmp %>% select(geocode, variablename, year, value, units, description, sex, age, periodicity, source, db, last_updated_in_db)
      all = rbind(all, tmp)
    }
    dbDisconnect(con)
    return(all)
  }
  tmp = pblapply(unique(key$db), db_get)
  tmp = bind_rows(tmp)
  return(tmp)
}
#' haddcountryinfo
#'
#' This function calculates correlations between variables
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
