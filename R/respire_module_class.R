#' @title Admin class for RESPIRE modules
#' @description
#' Using RESPIRE module API endpoint admin/dataStructure, the respireModule class extracts information
#' to validate and update RESPIRE data and metadata.
#'
respireModule <- R6::R6Class(
  classname = "respireModule",
  public = list(
    #' @param module_api_stub Stub of API location preceding required admin/dataStructure endpoint
    #' @import httr
    #' @importFrom dplyr `%>%`
    #' @import httr2
    #' @return Self
    #' @export
    initialize = function(module_api_stub){
      tryCatch({
        # cat(paste(module_api_stub, "admin/dataStructure", sep = "/"))
        private$struct <- httr2::request(paste(module_api_stub, "admin/dataStructure", sep = "/")) %>%
          httr2::req_error(body = error_body) %>%
          httr2::req_perform() %>%
          httr2::resp_body_json()

      },
      error = function(e){
        # browser()
        if (e$message == "Empty reply from server"){
          stop("Empty response from the API", call. = FALSE)
        }

        if (e$status == 404){

          stop("The specified endpoint is incorrect. Please ensure there are no trailing slashes or typos in your API stub and try again.", call. = FALSE)

        } else if (e$status >= 500){
          stop("500: API service is down altogether")
        }
      })

      valid_keys <- c("study_metadata_table", "sample_metadata_table", "data_table", "shared_key")

      if (!all(names(private$struct) %in% valid_keys)){

        bad_names <- names(private$struct)[!names(private$struct) %in% valid_keys]
        msg <- sprintf("Error: incorrect names in API response.\nIncorrect names: %s \nValid keys: %s",
                       paste0(bad_names, collapse = ", "),
                       paste0(valid_keys, collapse = ", ")
                       )
        stop(msg, call. = FALSE)
      }

      valid_keys <- c("db_schema", "table", "fields")

      if (!all(names(private$struct[['study_metadata_table']]) %in% valid_keys)){


        bad_names <- names(private$struct)[['study_metadata_table']][!names(private$struct)[['study_metadata_table']] %in% valid_keys]

        msg <- sprintf("Error: incorrect names in API response for `study_metadata_table`.\nIncorrect names: %s \nValid keys: %s",
                       paste0(bad_names, collapse = ", "),
                       paste0(valid_keys, collapse = ", ")
        )

        stop(msg, call. = FALSE)

      }

      if (!all(names(private$struct[['data_table']]) %in% valid_keys)){

        bad_names <- names(private$struct)[['data_table']][!names(private$struct)[['data_table']] %in% valid_keys]

        msg <- sprintf("Error: incorrect names in API response for `data_table`.\nIncorrect names: %s \nValid keys: %s",
                       paste0(bad_names, collapse = ", "),
                       paste0(valid_keys, collapse = ", ")
        )

        stop(msg, call. = FALSE)

      }
      # browser()
      self$study_metadata_struct <- private$struct[['study_metadata_table']]
      self$data_struct           <- private$struct[['data_table']]
      self$study_metadata_db_id  <- DBI::Id(schema = self$study_metadata_struct$db_schema,
                                            table = self$study_metadata_struct$table)

      self$data_db_id            <- DBI::Id(schema = self$data_struct$db_schema,
                                            table = self$data_struct$table)

      self$shared_key            <- private$struct[['shared_key']]

      self$has_sample_meta <- 'sample_metadata_table' %in% names(private$struct)

      if (self$has_sample_meta){

        if (!all(names(private$struct[['sample_metadata_table']]) %in% valid_keys)){

          bad_names <- names(private$struct)[['sample_metadata_table']][!names(private$struct)[['sample_metadata_table']] %in% valid_keys]

          msg <- sprintf("Error: incorrect names in API response for `sample_metadata_table`.\nIncorrect names: %s \nValid keys: %s",
                         paste0(bad_names, collapse = ", "),
                         paste0(valid_keys, collapse = ", ")
          )

          stop(msg, call. = FALSE)
        }

        self$sample_metadata_struct <- private$struct[['sample_metadata_table']]
        self$sample_metadata_db_id <- DBI::Id(schema = self$sample_metadata_struct$db_schema,
                                              table = self$sample_metadata_struct$table)

      }

      return(invisible(self))
    },
    #' @importFrom jsonlite prettify toJSON
    #' @return Self
    #' @export
    print = function(){
      cat("RESPIRE Module: \n")
      cat(jsonlite::prettify(jsonlite::toJSON(private$struct)), "\n")
      return(invisible(self))
    },
    #' @param dir Directory for output, defaults to current working directory
    #' @importFrom readr write_csv
    #' @return Self
    #' @export
    #'
    create_templates = function(dir = getwd()){
      cur_time <- strftime(Sys.time(), '%Y_%m_%d')
      readr::write_csv(data.frame(self$data_struct$fields),
                       file.path(
                         dir,
                         sprintf("respire_data_template_%s.csv",
                                 cur_time
                         )
                       )
      )

      readr::write_csv(data.frame(self$study_metadata_struct$fields),
                       file.path(
                         dir,
                         sprintf("respire_study_metadata_template_%s.csv",
                                 cur_time
                         )
                       )
      )

      if (self$has_sample_meta){
        readr::write_csv(data.frame(self$sample_metadata_struct$fields),
                         file.path(
                           dir,
                           sprintf("respire_sample_metadata_template_%s.csv",
                                   cur_time
                           )
                         )
        )
      }
    },
    #' @field study_metadata_struct Structure and location of study metadata table for module
    study_metadata_struct = NULL,
    #' @field sample_metadata_struct Structure and location of sample metadata table for module if applicable
    sample_metadata_struct = NULL,
    #' @field data_struct Structure and location of data table for module
    data_struct = NULL,
    #' @field shared_key Common key between data and metadata tables
    shared_key = NULL,
    #' @field study_metadata_db_id Database ID
    study_metadata_db_id = NULL,
    #' @field sample_metadata_db_id Database ID
    sample_metadata_db_id = NULL,
    #' @field data_db_id Database ID
    data_db_id = NULL,
    #' @field has_sample_meta TRUE if a sample metadata table exists for this module
    has_sample_meta = NULL
  ),
  active = list(
    #' @field study_metadata_fields Fields for study metadata
    study_metadata_fields = function(value){
      if (missing(value)){
        names(unlist(private$struct$study_metadata_table$fields))
      } else {
        stop("`$metadata_fields` is read only", call. = FALSE)
      }
    },
    #' @field data_fields Fields for data table
    data_fields = function(value){
      if (missing(value)){
        names(unlist(private$struct$data_table$fields))
      } else {
        stop("`$data_fields` is read only", call. = FALSE)
      }
    },
    #' @field study_metadata_schema Database schema of study metadata
    study_metadata_schema = function(value){
      if (missing(value)){
        private$struct$study_metadata_table$db_schema
      } else {
        stop("`$metadata_schema` is read only", call. = FALSE)
      }
    },
    #' @field data_schema Database schema of module data
    data_schema = function(value){
      if (missing(value)){
        private$struct$data_table$db_schema
      } else {
        stop("`$data_schema` is read only", call. = FALSE)
      }
    },
    #' @field study_metadata_table Name of database table containing study metadata
    study_metadata_table = function(value){
      if (missing(value)){
        private$struct$study_metadata_table$table
      } else {
        stop("`$study_metadata_table` is read only", call. = FALSE)
      }
    },
    #' @field data_table Name of database table containing module data
    data_table = function(value){
      if (missing(value)){
        private$struct$data_table$table
      } else {
        stop("`$data_table` is read only", call. = FALSE)
      }
    }
  ),
  private = list(
    struct = NULL
  )
)

error_body <- function(resp) {
  resp_body_json(resp)$error
}
