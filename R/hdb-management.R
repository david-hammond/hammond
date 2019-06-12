#' hdb_kill
#'
#' This function calculates correlations between variables
#'
#' @param countries list of countries
#'
#' @examples
#' #need 4 column data frame, geocode, variablename, year, value
#'
#' @export
hdbkill = function(){
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
  con <- hdb_connect(db)
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
  require(uuid)
  con = hdb_connect()
  dbs = dbGetQuery(con, "SELECT datname FROM pg_database
  WHERE datistemplate = false;")
  dbDisconnect(con)
  dbs = dbs %>% filter(!(datname %in% c("postgres", "master")))
  master_key = NULL
  for (db in dbs$datname){
    con <- hdb_connect(db)
    key = dbReadTable(con, "key")
    key = key %>% filter(tablename %in% dbListTables(con))

    key = key %>% select(seriescode, geolevel, variablename, description, periodicity, units, age, sex, source, tablename, last_updated)
    key$uid = sapply(seq_along(1:nrow(key)), uuid::UUIDgenerate)
    key$db = db
    key$uid = sapply(seq_along(1:nrow(key)), uuid::UUIDgenerate)
    # for (tab in key$tablename){
    #   tmp = dbReadTable(con, tab)
    #   tmp = left_join(key %>% select(uid, seriescode), tmp) %>% select(-seriescode)
    #   dbWriteTable(con, tab, tmp, overwrite = T)
    # }
    master_key = rbind(master_key, key)
    dbDisconnect(con)
  }
  db = "master"
  con = hdb_create_db(db)
  dbWriteTable(con, "key", master_key, overwrite = T, row.names = F)
  dbDisconnect(con)
}
