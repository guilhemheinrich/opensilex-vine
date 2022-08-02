#' summary_visualisation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_summary_visualisation_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(div(
      style = "width:90%;padding-left:5%;", fluidRow(shiny::verbatimTextOutput(ns("terminal")))
    )),
    uiOutput(ns("plots"))
  )
}

#' summary_visualisation Server Functions
#'
#' @noRd
mod_summary_visualisation_server <- function(id, DATA) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    print("ns")
    print(ns)
    column_names <- reactiveVal()
    column_types <- reactiveVal()

    shiny::observeEvent(DATA(), {
      column_names(colnames(DATA()))
      column_types(sapply(DATA(), class))
    })

    output$terminal <- shiny::renderPrint({
      summary(DATA())
    })

  #######################################################
  # Take a look at <https://gist.github.com/wch/5436415/>
  #######################################################
  # Insert the right number of plot output objects into the web page
  output$plots <- shiny::renderUI({
    plot_output_list <- lapply(column_names(), function(col_name) {
      shiny::plotOutput(ns(col_name), height = 280, width = 250)
    })

    # Convert the list to a tagList - this is necessary for the list of items
    # to display properly.
    do.call(shiny::tagList, plot_output_list)
  })

  shiny::observeEvent(column_names(), {
    # Call renderPlot for each one. Plots are only actually generated when they
    # are visible on the web page.
    for (col_name in column_names()) {
      # Need local so that each item gets its own number. Without it, the value
      # of i in the renderPlot() will be the same across all instances, because
      # of when the expression is evaluated.
      local({
        my_name <- col_name
        print(paste("processing", col_name))
        #    _______ ____  _____   ____  
        #   |__   __/ __ \|  __ \ / __ \ 
        #      | | | |  | | |  | | |  | |
        #      | | | |  | | |  | | |  | |
        #      | | | |__| | |__| | |__| |
        #      |_|  \____/|_____/ \____/ 
        #                                
        # 
        # Plot nice ggplot graphics
        # Maybe facet it, add a parameter to the UI interface which regulate the layout ?
        # Also, the reactive value column_types() store the types, maybe plot something different
        # for charcter/factor/numeric ?
        #######################################################
        # Take a look at <https://stackoverflow.com/a/52869824>
        #######################################################
        
        output[[col_name]] <- shiny::renderPlot({
          plot(1:10, rnorm(10),
              main = my_name
          )
        })
      })
    }
  })


  })
}

## To be copied in the UI
# mod_summary_visualisation_ui("summary_visualisation_1")

## To be copied in the server
# mod_summary_visualisation_server("summary_visualisation_1")
