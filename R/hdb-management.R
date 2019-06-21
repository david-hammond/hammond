#' hdb_kill
#'
#' This function kills all connections to the database, use as a last resort if you get a db connection error
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
#' This function creates a database, use only if you know what you are doing
#'
#' @param countries list of countries
#'
#' @examples
#' #need 4 column data frame, geocode, variablename, year, value
#'
#' @export

hdb_create_db = function(host, db, user, password){
  con = hdb_connect()
  query = paste("DROP DATABASE IF EXISTS", db)
  dbSendQuery(con, query)
  query = paste("CREATE DATABASE", db)
  dbSendQuery(con, query)
  dbDisconnect(con)
  return()
}


#' hdb_backup
#'
#' This function backsup a database, use only if you know what you are doing
#'
#' @param countries list of countries
#'
#' @examples
#' #need 4 column data frame, geocode, variablename, year, value
#'
#' @export

hdb_backup = function(host = "192.168.0.98", user = "postgres", password = "peace123", port = 5432){
  db = Sys.getenv("DB_NAME")
  con = hdb_connect()
  drv <- dbDriver("PostgreSQL")
  con_backup <- dbConnect(drv, dbname = "postgres",
                   host = host, port = port,
                   user = user, password = password)
  query = paste("DROP DATABASE IF EXISTS", db)
  dbSendQuery(con_backup, query)
  query = paste("CREATE DATABASE", db)
  backup = function(tbl){
    tmp = dbReadTable(con, tbl)
    dbWriteTable(con_backup, tbl, tmp, row.names = F)
  }
  pblapply(dbListTables(con), backup)
  dbDisconnect(con)
  dbDisconnect(con_backup)
  return(con)
}


