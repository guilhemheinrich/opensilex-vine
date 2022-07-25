pheno_config <- new.env()
source(file = "./R/default_etl_config.R", local = pheno_config)

#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  authentification <- mod_authentification_server("auth")
  file_loader <- mod_file_loader_server("input")
  pheno_etl <- mod_etl_server("pheno", DATA = file_loader$data, server_configuration = pheno_config$config)
}
