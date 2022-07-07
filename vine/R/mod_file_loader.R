#' file_loader UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shinydashboard dashboardPage dashboardSidebar
mod_file_loader_ui <- function(id) {
  ns <- NS(id)


  shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(title = "Source des données"),
    shinydashboard::dashboardSidebar(
      shinyWidgets::radioGroupButtons(
        ns("fileOrigin"),
        label = "Origine des données",
        choices = c("Fichier", "Copier-coller"),
        status = "primary"
      ),
      shinyWidgets::prettySwitch(
          ns("header2"),
          label = "Header",
          value = TRUE,
          inline = TRUE,
          status = "success",
          bigger = TRUE
      ),
      shiny::numericInput("skipLines", label="Nb lignes à passer",
        value=0),
      shiny::conditionalPanel(
        #### OPTION 1: upload file
        condition = "input.fileOrigin == 'Fichier'",
        ns = ns,
        fileInput(
          ns('file1'),
          'Choix du fichier de données',
          accept = c(".xlsx",
                     ".xls",
                     ".csv")
        ),

        uiOutput(ns("dropdownUI")),


      ),
      # end shiny::conditionalPanel
      shiny::conditionalPanel(
        ##### OPTION 2: copy paste
        condition = "input.fileOrigin == 'Copier-coller'",
        ns = ns,
        textAreaInput(
          inputId = ns("user_text"),
          label = "Importation par copier-coller",
          height = 200,
          width = 700,
          placeholder =
            "Copier-coller ici votre tableau de données : CTRL+C puis CTRL+V
Préférer l'importation à partir d'un fichier pour une meilleure traçabilité"
        ),
        # submit button
        actionButton(ns("submit"), label = "Importer"),
        actionButton(ns("reset_input"), "Nettoyer le texte"),
      )
    ),
    shinydashboard::dashboardBody(
      fluidRow(
        uiOutput(ns("separator")),
        uiOutput(ns("decimal")),
        uiOutput(ns("header"))
      ),
      DT::dataTableOutput(ns('inputTable'))
    )
  )
}

#' file_loader Server Functions
#'
#' @noRd
mod_file_loader_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


    Workbook <- reactiveVal()
    Sheets <- reactiveVal()
    Data <- reactiveVal()
    Type <- reactiveVal()
    # Workbook <- eventReactive(input$file1, {
    #   openxlsx::loadWorkbook(input$file1$datapath)
    # })


    # Sheets <- eventReactive(Workbook(), {
    #   names(Workbook())
    # })

    # output$dropdownUI <- renderUI({
    #   req(Sheets())
    #   selectInput(ns("sheet"), "Choix d'une feuille", Sheets())
    # })

    # observeEvent(Sheets(), {
    #
    # })

    observeEvent(Type(), {
      switch(Type(),
             "xslx" = {
               Workbook(openxlsx::loadWorkbook(input$file1$datapath))
               Sheets(names(Workbook()))
               output$dropdownUI <- renderUI({
                 selectInput(ns("sheet"), "Choix d'une feuille", Sheets())
               })
             },
             "csv" = {
               Data(read.csv(input$file1$datapath, header = TRUE))
               output$dropdownUI <- renderUI({
                 NULL
               })
             })
    })

    #### OPTION 1 : load Excel file

    observeEvent(input$file1, {
      extension <- gsub(".*\\.(.*)$", "\\1", input$file1$datapath)
      # gsub(".*\.(.*)\\..*", "\\1", input$file1$datapath)
      # print(extension)

      # MIME type will be there if we launch the application through the browser, not the debug view
      if (input$file1$type != "") {
        switch(input$file1$type,
               "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" = {
                 Type("xslx")
                 # Workbook(openxlsx::loadWorkbook(input$file1$datapath))
                 # Sheets(names(Workbook()))
               },
              "text/csv" = {
                Type("csv")
                # Data(read.csv(input$file1$datapath, header = TRUE))
        })
      } else {
        switch(extension,
             "xlsx" = {
               Type("xslx")
               # Workbook(openxlsx::loadWorkbook(input$file1$datapath))
               # Sheets(names(Workbook()))
              },
             "xls"= {
               Type("xslx")
               # Workbook(openxlsx::loadWorkbook(input$file1$datapath))
               # Sheets(names(Workbook()))
             },
             "csv" = {
               Type("csv")
               # Data(read.csv(input$file1$datapath, header = TRUE))
             }
        )
      }

    })


    observeEvent(c(input$sheet), {
      Dat1 <- readxl::read_xlsx(
        path = req(input$file1$datapath),
        sheet = req(input$sheet)
      )
      colnames(Dat1) <-
        iconv(colnames(Dat1), from = "UTF-8", to = "ASCII//TRANSLIT//IGNORE")
      colnames(Dat1) <-
        gsub("[[:punct:]]", "", x = colnames(Dat1))  ## to remove all punctuation
      colnames(Dat1) <-
        gsub(x = colnames(Dat1),
             pattern = " ",
             replacement = "")
      colnames(Dat1) <-
        gsub(x = colnames(Dat1),
             pattern = "-",
             replacement = "_")
      Data(Dat1)
    })


    ######### OPTION 2 copy and paste data
    # reactive expression
    observeEvent(input$submit, {
      ## when the user clic on the "importer" button
      # could be adapted with user's inputs for decimal per instance
      # Dat2 <- read.table(text = input$user_text, as.is = TRUE,
      #                    skipNul = TRUE, header=T, sep="\t",dec=".")
      Dat2 <-
        data.table::fread(input$user_text, encoding = "UTF-8", dec = ",")
      Dat2 <- Dat2[, complete.cases(t(Dat2))] # Omit NAs by columns
      colnames(Dat2) <-
        iconv(colnames(Dat2), from = "UTF-8", to = "ASCII//TRANSLIT//IGNORE")
      colnames(Dat2) <-
        gsub("[[:punct:]]", "", x = colnames(Dat2))  ## to remove all punctuation
      colnames(Dat2) <-
        gsub(x = colnames(Dat2),
             pattern = " ",
             replacement = "")
      colnames(Dat2) <-
        gsub(x = colnames(Dat2),
             pattern = "-",
             replacement = "_")
      Data(Dat2) ## reactiveValue Dat() set to Dat2
    })
    # to clear the text
    observeEvent(input$reset_input, {
      updateTextInput(session, "user_text", value = c(""))
    })

    output$inputTable <- DT::renderDataTable({
      DT::datatable(Data(),
                    options = list(
                      pageLength = 25,
                      autoWidth = FALSE,
                      scrollX = TRUE
                    ))
    })
    return(list(
      data = Data
      ))
  })
}

## To be copied in the UI
# mod_file_loader_ui("file_loader_1")

## To be copied in the server
# mod_file_loader_server("file_loader_1")
