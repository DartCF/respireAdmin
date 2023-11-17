#' Register a new module with a RESPIRE registry API
#'
#' @param registry_api_stub Stub of the registry API
#' @param module_spec A named list with the structure `list(module_name = "Unique, descriptive name here", module_api = "https://my/api/here")`. If NULL, you will receive an interactive prompt to define a module specification.
#'
#' @export
#'
#' @importFrom httr2 request req_body_json req_perform
#'
register_new_module <- function(registry_api_stub, module_spec = NULL){

  if (is.null(module_spec)){

    name     <- readline(prompt = "Enter a unique, descriptive module name: ")
    api_stub <- readline(prompt = "Enter the sub of the module API: ")

    spec <- list(module_name = name, module_api = api_stub)

  } else {

    if (!is.list(module_spec)){
      stop("`module_spec` must be a named list or NULL", call. = FALSE)
    }

    if (!all(names(module_spec) %in% c("module_name", "module_api"))){
      stop("`module_spec` must be a named list or with the names module_name and module_api", call. = FALSE)
    }

    spec <- module_spec

  }

  print(jsonlite::prettify(jsonlite::toJSON(spec, auto_unbox = TRUE)), "/n/n")

  ans <- menu(c("Yes", "No"), title = "Is the module specification printed above correct?")

  if (ans != 1){

    stop("Module registration aborted", call. = FALSE)

  }

  spec$module_api <- paste0(gsub("/+$", "", spec$module_api), "/")

  processed_stub <- gsub("/+$", "",registry_api_stub)
  path <- c(processed_stub, "register_module")

  httr2::request(paste0(path, collapse = '/')) %>%
    httr2::req_body_json(spec) %>%
    httr2::req_perform()

  message("Module registered. Refresh your deployed application and confirm the module is available")


}

# register_new_module("https://respire-registry.dartmouth.edu")
