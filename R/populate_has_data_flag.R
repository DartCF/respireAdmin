#' Automatically update the field `has_data` in the study metadata based on the shared key
#'
#' @param respire_module A respireModule object
#' @param conn A valid database connection
#'
#' @return Count of records updated
#' @export
#' @importFrom glue glue_sql
#' @importFrom DBI dbExecute dbDisconnect
populate_has_data_flag <- function(respire_module, conn = connect_to_db()){
  if (is.null(respire_module$shared_key)){
    stop("A shared key is required")
  }

  if (!"has_data" %in% respire_module$study_metadata_fields){
    stop("Cannot set has_data flag because variable has_data not present in metadata specification", call. = FALSE)
  }

  if (!(respire_module$shared_key %in% respire_module$data_fields & respire_module$shared_key %in% respire_module$study_metadata_fields)){
    stop("Shared key not present in both metadata and data field specifications", call. = FALSE)
  }

  query <- glue::glue_sql("update
                          {`respire_module$study_metadata_schema`}.{`respire_module$study_metadata_table`}
                          set has_data = 1 where {`respire_module$study_metadata_schema`}.{`respire_module$study_metadata_table`}.{`respire_module$shared_key`}
                          in
                          (select distinct {`respire_module$shared_key`} from {`respire_module$data_schema`}.{`respire_module$data_table`})
                          ", .con = conn)

  res <- DBI::dbExecute(conn, query)

  DBI::dbDisconnect(conn)
  return(sprintf("Records updated: %s", res))
}
