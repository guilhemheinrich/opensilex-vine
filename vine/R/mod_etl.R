library(magrittr)
#' etl UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_etl_ui <- function(id, client_configuration = list()){
  ns <- NS(id)
  tagList(
    rhandsontable::rHandsontableOutput(ns("edit_rht_dat1")),
    DT::dataTableOutput(ns('inputTable'))
  )
}
    
#' etl Server Functions
#'
#' @noRd 
mod_etl_server <- function(id, DATA, server_configuration = list()){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Check the reactive / input value
    output$inputTable <- DT::renderDataTable({
      DT::datatable(DATA(),
                    options = list(
                      pageLength = 25,
                      autoWidth = FALSE,
                      scrollX = TRUE
                    ))
    })

    # output$edit_rht_dat1 <- rhandsontable::renderRHandsontable(
    #   rhandsontable::rhandsontable(data.frame(Variable=c(colnames(DATA()),"",""),
    #                           Correspondance=c(rep(NA,ncol(DATA())),"Nom","Date"),
    #                           Expression="")) %>%
    #     rhandsontable::hot_col(col="Variable",readOnly=TRUE) %>%
    #     rhandsontable::hot_col(col="Correspondance",type="dropdown",
    #             allowInvalid=FALSE) %>%
    #     rhandsontable::hot_col(col="Expression",type="dropdown",
    #             source=c("",
    #                     "as.numeric(format(as.Date(COLONNE),format='%j'))",
    #                     "paste0('B',bloc,'R',Rang)",
    #                     'paste0(paste0(substr(Cepage,1,1),"-",substring(Cepage,2)),"_",Modalite)')) %>%
    #     rhandsontable::hot_cols(manualColumnResize=TRUE,colWidths=c(150,275,275)))
  })
}
    
## To be copied in the UI
# mod_etl_ui("etl_1")
    
## To be copied in the server
# mod_etl_server("etl_1")
