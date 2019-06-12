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
dbkill = function(){
  all_cons <- dbListConnections(dbDriver("PostgreSQL"))
  for(con in all_cons)
    dbDisconnect(con)
}
#' hdb_create_db
#'
#' This function calculates correlations between variables
#'
#' @param countries list of countries
#'
#' @examples
#' #need 4 column data frame, geocode, variablename, year, value
#'
#' @export

hdb_create_db = function(db){
  con = hdb_connect()
  query = paste("DROP DATABASE IF EXISTS", db)
  dbSendQuery(con, query)
  query = paste("CREATE DATABASE", db)
  dbSendQuery(con, query)
  dbDisconnect(con)
  con <- db_connect(db)
  return(con)
}

#' hdb_update_master
#'
#' This function calculates correlations between variables
#'
#' @param countries list of countries
#'
#' @examples
#' #need 4 column data frame, geocode, variablename, year, value
#'
#' @export
hdb_update_master = function(){
  con = db_connect()
  dbs = dbGetQuery(con, "SELECT datname FROM pg_database
  WHERE datistemplate = false;")
  dbDisconnect(con)
  dbs = dbs %>% filter(!(datname %in% c("postgres", "master")))
  master_key = NULL
  for (db in dbs$datname){
    con <- db_connect(db)
    key = dbReadTable(con, "key")
    key = key %>% filter(tablename %in% dbListTables(con))
    key = key %>% select(seriescode, geolevel, variablename, description, units, age, sex, source, tablename, last_updated)
    key$db = db
    master_key = rbind(master_key, key)
    dbDisconnect(con)
  }
  db = "master"
  con = db_create(db)
  dbWriteTable(con, "key", master_key, overwrite = T, row.names = F)
  dbDisconnect(con)
}