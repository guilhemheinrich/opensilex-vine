#' authentification UI Function
#'
#' @description The UI part of the module handling the connection to an opensilex instance. Return a token
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @export
#' @importFrom shiny NS tagList textInput passwordInput actionButton verbatimTextOutput
mod_authentification_ui <- function(id, default_host = "http://138.102.159.36:8081/rest", default_user = "admin@opensilex.org", default_password = "admin") {
  ns <- NS(id)
  tagList(
    shiny::textInput(NS(id, "host"), "Host", value = default_host),
    shiny::textInput(NS(id, "user"), "Username", value = default_user),
    shiny::passwordInput(NS(id, "password"), "Password",  value = default_password),
    shiny::actionButton(NS(id, "test"), "Test connection"),
    shiny::verbatimTextOutput(NS(id, "terminal"))
  )
}


#' authentification Server Functions
#'
#' @description The server part of the module handling the connection to an opensilex instance.
#' @param id Internal parameter for {shiny}.
#' @return A named list with various reactive values
#' \describe{
#'  \item{connect}{A function handling the connection to an Opensilex WebAPI}
#'  \item{user}{The currently connected user}
#'  \item{password}{The current password}
#'  \item{host}{The current opensilex WebAPI host address}
#' }
#' @export
#' @importFrom shiny renderPrint reactive observeEvent
#' @importFrom evaluate try_capture_stack evaluate
#' @importFrom utils capture.output
#' @importFrom opensilexR get_token
mod_authentification_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    get_token <- function() {
      cat('Connecting...\n')
      cat('User: ', input$user, '\n')
      cat('url: ', input$host, '\n')
      token <- evaluate::try_capture_stack(
          # utils::capture.output(
            opensilexR::get_token(
              host = input$host,
              user = input$user,
              password = input$password
            )
          # )
      ,
        environment()
      )
      # print("token")
      # print(class(token))
      # print(token)
      if (is(token,"character")) {
        output$terminal <- shiny::renderPrint({
          cat('Token retrieved !\n')
          cat(token)
        })
        return(token)
      } else {
        output$terminal <- shiny::renderPrint({
          ## base::message send text to sderr, triggering an error state
          message(
'Something went wrong.
Check your credentials then the logs')
          message(token)
        })
        output$terminal <- shiny::renderPrint({
          message(token)
        })
        return(NULL)
      }
    }
    connect <- shiny::reactive({
      attempt_connect()
    })

    shiny::observeEvent(input$test, {
      get_token()
    })

    return(list(
      connect = connect,
      user = shiny::reactive(input$user),
      password = shiny::reactive(input$password),
      host = shiny::reactive(input$host)
    ))
  })
}

