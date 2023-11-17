#' Connect to a database
#'
#' @param driver A database driver. Defaults to the Rpostgres Postgres database driver
#'
#' @return A database connection
#' @export
#'
#' @importFrom RPostgres Postgres
#' @importFrom DBI dbConnect
#'
connect_to_db <- function(driver = RPostgres::Postgres()
){
  DBI::dbConnect(
    drv = driver,
    user = get_env_var('RESPIRE_DB_UNAME'),
    password = get_env_var('RESPIRE_DB_PASSWORD'),
    host = get_env_var('RESPIRE_DB_HOST'),
    port = get_env_var('RESPIRE_DB_PORT'),
    dbname = get_env_var('RESPIRE_DB_NAME')
  )
}

get_env_var <- function(name){
  val <- Sys.getenv(name)
  if (val == ''){
    stop(sprintf("Required value %s not set, ensure you have run set_db_credentials and try again", name))
  }

  return(val)

}
