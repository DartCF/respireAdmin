#' Set database credentials and optionally store in a .Renviron file
#'
#' @param host Database host
#' @param port Database port
#' @param username Username
#' @param password Password
#' @param db Database to connect to, defaults to 'postgres'
#' @param install If TRUE, store values in .Renviron for later use
#'
#' @export
#'
#' @importFrom utils menu
#' @importFrom utils read.table
#' @importFrom utils write.table
#' @importFrom crayon green
#' @importFrom crayon red
#' @importFrom cli symbol
#'

set_db_credentials <- function(host,
                               port,
                               username,
                               password,
                               db = 'postgres',
                               install = FALSE
){
  stopifnot(is.logical(install))
  stopifnot(is.numeric(port))
  stopifnot(is.character(host))
  stopifnot(is.character(username))
  stopifnot(is.character(password))

  values_to_store = list(RESPIRE_DB_PASSWORD = password,
                         RESPIRE_DB_HOST = host,
                         RESPIRE_DB_PORT = port,
                         RESPIRE_DB_UNAME = username,
                         RESPIRE_DB_NAME = db
  )

  if (install) {

    home <- Sys.getenv("HOME")
    renv <- file.path(home, ".Renviron")

    if (!file.exists(renv)){

      file.create(renv)

    }
    # Backup original .Renviron before doing anything else here.
    message("Your original .Renviron will be backed up and stored in your R HOME directory if needed.")
    file.copy(renv, file.path(home, ".Renviron_backup"))

    tv <- readLines(renv)

    for (key in names(values_to_store)){
      check_and_update(tv, renv, key, values_to_store[[key]])
    }

    message(crayon::green(cli::symbol$tick), ' Your database credentials have been stored in your .Renviron. \n  To use now, restart R or run `readRenviron("~/.Renviron")`')
    return(invisible(key))


  } else {

    do.call(Sys.setenv, values_to_store)

    message(crayon::green(cli::symbol$tick), " Database credentials set for current session. To install your databse credentials key for use in future sessions, run this function with `install = TRUE`.")
  }

}

check_and_update <- function(tv, renv, key, value){
  if(any(grepl(key, tv))){

    ans <- utils::menu(c(paste(crayon::green(cli::symbol$tick), 'Yes'),
                         paste(crayon::red(cli::symbol$cross), 'No')),
                       title = sprintf("An %s already exists. Do you want to overwrite it?", key))

    if (ans == 1){

      oldenv <- utils::read.table(renv, stringsAsFactors = FALSE)
      newenv <- oldenv[-grep(key, oldenv$V1),]

      utils::write.table(newenv, renv, quote = FALSE, sep = "\n",
                         col.names = FALSE, row.names = FALSE
      )

    } else {

      stop(crayon::red(cli::symbol$cross), sprintf("Your %s was not updated.", key), call. = FALSE)

    }
  }


  keyconcat <- sprintf("%s='%s'", key, value)

  # Append API key to .Renviron file
  write(keyconcat, renv, sep = "\n", append = TRUE)
}
