#' Title
#'
#' @param data A data frame
#' @param field_names A vector of field names
#' @param field_types A vector of field types
#'
#' @importFrom purrr walk2
validate_data <- function(data, field_names, field_types){
  if(!all(field_types %in% c("character", "numeric", "integer", "logical"))){
    stop(
      sprintf("Field types from the API specification are not compatible with R. Contact the administrator of your module API to resolve.
                 \nValid Field Types: %s
                 \nCurrent Field Types: %s",
              paste(c("character", "numeric", "integer", "logical"), collapse = ", "),
              paste(field_types, collapse = ", ")
      )
    )
  }

  if (!all(field_names %in% names(data))){
    stop(sprintf("Data is missing required fields: %s. \nRequired fields are %s", paste0(names(data)[!names(data) %in% field_names], collapse = ", "), paste0(field_names, collapse = ", ")))
  } # check for required fields

  classes <- lapply(data, class)

  # check for class compatibility
  purrr::walk2(field_names, field_types, ~{
    if(!classes[[.x]] == .y){
      stop(sprintf("Fields in the provided data do not match the requirement provided by the module API Field %s is of type %s, type %s expected", .x, classes[[.x]], .y))
    }
  })
}

# DBI::dbWriteTable()
