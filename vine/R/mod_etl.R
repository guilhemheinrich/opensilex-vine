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
mod_etl_ui <- function(id, client_configuration = list()) {
  ns <- shiny::NS(id)
  tagList(
    shiny::h3("Input data summary"),
    # DT::dataTableOutput(ns("summaryInput")),
    fluidPage(div(
      style = "width:50%;padding-left:5%;", fluidRow(shiny::verbatimTextOutput(ns("terminal")))
    )),
    # shiny::verbatimTextOutput(ns("terminal")),
    shiny::h2("Transformation summary"),
    # rhandsontable::rHandsontableOutput(ns("edit_rht_dat1")),
    fluidPage(div(
      style = "padding-left:5%;", fluidRow(rhandsontable::rHandsontableOutput(ns("edit_rht_dat1")))
    )),
    shiny::h2("Overview result"),
    DT::dataTableOutput(ns("outputTable"))
    # fluidPage(
    #   div(style="padding-left:5%;",fluidRow(DT::dataTableOutput(ns("outputTable"))))
    # )
  )
}

#' etl Server Functions
#'
#' @noRd
mod_etl_server <-
  function(id, DATA, server_configuration = list()) {
    column_names <- sapply(server_configuration$output_format$static,
                           function(column_configuration) {
                             return(column_configuration$name)
                           })
    validation_functions <- sapply(server_configuration$output_format$static,
                                  function(column_configuration) {
                                    to_parse <- paste("list(", column_configuration$name, " = column_configuration$validation)")
                                    return(eval(parse(text = to_parse)))
                                  })

    moduleServer(id, function(input, output, session) {
      ns <- session$ns


      OutputData <- shiny::reactiveVal()

      shiny::observeEvent(DATA(), {
        output <- data.frame(DATA())
        for (name in rev(column_names)) {
          # output[name] <- NA
          print(name)
          parsed_function <-
            paste("tibble::add_column(output,", name, " = NA, .before = 1)")
          output <- eval(parse(file = NULL, text =  parsed_function))
        }
        OutputData(output)
      })


      shiny::observeEvent(OutputData(), {
        data <- data.frame(OutputData())
        for (name in column_names) {
          validator <- validation_functions[[name]](data[name])
          print(paste(name, " is ", validator))
        }
      })

      # Input summary info
      output$terminal <- shiny::renderPrint({
        summary(DATA())
      })

      # Check the reactive / input value
      output$outputTable <- DT::renderDataTable({
        DT::datatable(OutputData(),
                      options = list(
                        pageLength = 25,
                        autoWidth = FALSE,
                        scrollX = TRUE
                      ))
      })

      output$edit_rht_dat1 <- rhandsontable::renderRHandsontable(
        # configuration <- server_configuration$output_format$static

        rhandsontable::rhandsontable(
          data.frame(
            # Variable = c("", "", "", ""),
            "Output Variable" = column_names,
            # "Output Variable" = c("", "name"),
            Transformation = "",
            "Column selection" = TRUE,
            check.names = FALSE
          ),
          overflow = "visible"
        ) %>%
          # rhandsontable::hot_col(col = "Variable", readOnly = TRUE) %>%
          rhandsontable::hot_col(
            col = "Output Variable",
            type = "dropdown",
            allowInvalid = TRUE
          ) %>%
          rhandsontable::hot_col(
            col = "Transformation",
            type = "dropdown",
            source = c(
              "",
              "as.numeric(format(as.Date(COLONNE),format='%j'))",
              "paste0('B',bloc,'R',Rang)",
              'paste0(paste0(substr(Cepage,1,1),"-",substring(Cepage,2)),"_",Modalite)'
            )
          ) %>%
          rhandsontable::hot_col(
            col = "Column selection",
            type = "checkbox"
          ) %>%
          rhandsontable::hot_cols(
            manualColumnResize = TRUE,
            colWidths = c(150, 275, 275)
          )
          %>%
          rhandsontable::hot_row((1:length(column_names)), readOnly = TRUE)
          %>%
          rhandsontable::hot_col(1, readOnly = FALSE)
# WITH RENDERER
#         %>%
#           hot_cols(renderer = "
# function(instance, td, row, col, prop, value, cellProperties) {
# Handsontable.renderers.NumericRenderer.apply(this, arguments);
# if (row === 2) {
# td.style.background = 'lightgrey';
# cellProperties.readOnly = true;
# }
# }")
      )
    })
  }

## To be copied in the UI
# mod_etl_ui("etl_1")

## To be copied in the server
# mod_etl_server("etl_1")
