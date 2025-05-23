
connect_to_db <- function(db, fun, ...){
  if(stringr::str_detect(db, "md:")){
    db_conn <- DBI::dbConnect(duckdb::duckdb())
    DBI::dbExecute(db_conn, "INSTALL 'motherduck'") 
    DBI::dbExecute(db_conn, "LOAD 'motherduck'") 
    token <- Sys.getenv("motherduck_token")
    auth_query <- glue::glue_sql("SET motherduck_token={token}", .con = db_conn)
    DBI::dbExecute(db_conn, auth_query)
    DBI::dbExecute(db_conn, "PRAGMA MD_CONNECT")
    DBI::dbExecute(db_conn, paste("USE", stringr::str_remove(db, "md:")))
  } else{
    db_conn <- DBI::dbConnect(duckdb::duckdb(), db)
  }
  
  tryCatch({
    fun(db_conn, ...)
  }, 
  finally = {
    DBI::dbDisconnect(db_conn)
  })
}

query_db <- function(query, db){
  connect_to_db(db, DBI::dbGetQuery, query)
}

save_db_backup <- function(db){
  folder <- paste0("data/backup_", stringr::str_replace_all(Sys.Date(), "-", ""))
  cmd <- stringr::str_glue("EXPORT DATABASE '{folder}'")
  execute_in_db(cmd, db)
}
