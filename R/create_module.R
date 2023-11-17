#' Create a new RESPIRE module
#'
#' @param api_stub Stub of API location preceding required admin/dataStructure endpoint
#'
#' @return A respireModule instance
#' @export
#'
create_module <- function(api_stub){
  respireModule$new(api_stub)
}

#' Create template data
#'
#' @param module A respireModule instance returned by `create_module` or `respireModule$new()`
#' @param dir The directory to create the templates in. Defaults to current working directory
#'
#' @export
create_templates <- function(module, dir = getwd()){
  module$create_templates(dir = dir)
  message("Templates created")
}
