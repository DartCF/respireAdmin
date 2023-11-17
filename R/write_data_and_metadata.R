#' Write metadata to the database
#'
#' @param respire_module A respireModule class instance
#' @param metadata A dataframe containing metadata
#' @param conn A database connection. Defaults to result of `connect_to_db()`, but will accept any DBI database connection
#' @param metadata_table The metadata table to update, one of "study" or "sample"
#' @param collision_strategy How to handle existing data. One of "append" or "overwrite"
#'
#' @export
#' @importFrom DBI dbWriteTable
#'
write_metadata <- function(respire_module, metadata, conn = connect_to_db(), metadata_table = "study", collision_strategy = "append") {

  strategy <- match.arg(collision_strategy, c('append', 'overwrite'))
  tbl      <- match.arg(metadata_table, c('study', 'sample'))

  if (!respire_module$has_sample_meta & tbl == 'sample'){
    stop("Module specification does not include sample metadata.", call. = FALSE)
  }

  if (strategy == 'overwrite'){
    ans <- menu(c('Yes', "No"), title = "Are you sure you want to overwrite the existing data? This action cannot be reversed.")

    if (ans != 1){
      stop("Data write aborted", call. = FALSE)
    }

  }

  struct <- if (tbl == 'study'){ respire_module$study_metadata_struct } else { respire_module$sample_metadata_struct }
  dbid   <- if (tbl == 'study'){ respire_module$study_metadata_db_id } else { respire_module$sample_metadata_db_id }

  if (is.null(dbid)){
    stop(sprintf("No available table id for %s metadata. Confirm the the module UI is correctly configured.", tbl))
  }

  field_names <- names(unlist(struct$fields, recursive = FALSE))
  field_types <- unlist(struct$fields, recursive = TRUE)

  validate_data(metadata, field_names, field_types)

  DBI::dbWriteTable(conn,
                    name = dbid,
                    value = metadata,
                    append = strategy == 'append',
                    overwrite = strategy == 'overwrite')
  DBI::dbDisconnect(conn)
  message("Success: Metadata has been updated")

}

#' Write metadata to the database
#'
#' @param respire_module A respireModule class instance
#' @param data A dataframe containing data
#' @param conn A database connection. Defaults to result of `connect_to_db()`, but will accept any DBI database connection
#' @param collision_strategy How to handle existing data. One of "append" or "overwrite"
#'
#' @export
#' @importFrom DBI dbWriteTable dbDisconnect
#'
write_data <- function(respire_module, data, conn = connect_to_db(), collision_strategy = "append") {

  strategy <- match.arg(collision_strategy, c('append', 'overwrite'))

  if (strategy == 'overwrite'){
    ans <- menu(c('Yes', "No"), title = "Are you sure you want to overwrite the existing data? This action cannot be reversed.")

    if (ans != 1){
      stop("Data write aborted", call. = FALSE)
    }

  }

  field_names <- names(unlist(respire_module$data_struct$fields, recursive = FALSE))
  field_types <- unlist(respire_module$data_struct$fields, recursive = TRUE)

  validate_data(data, field_names, field_types)

  DBI::dbWriteTable(conn,
                    name = respire_module$data_db_id,
                    value = data,
                    append = strategy == 'append',
                    overwrite = strategy == 'overwrite')

  DBI::dbDisconnect(conn)
  message("Success: Data has been updated")

}
