
mod_errorModal_ui <- function(id) {}


mod_errorModal_server <- function(id, 
                                  title = NULL,
                                  text = NULL, 
                                  footer = modalButton("Close")){
    

    shiny::moduleServer(id,
        function(input, output, session) {
            observeEvent(TRUE, ignoreInit = FALSE, {
                shiny::showModal(
                    div(
                        id = 'errModal',
                        tags$style("#errModal .modal-dialog{width: 600px;}"),
                        shiny::modalDialog(
                            h3(title),
                            tags$style("#tPanel {overflow-y:scroll; color: red; background: white;}"),
                            shiny::wellPanel(id = "tPanel",
                                HTML(paste(text, collapse = "<br/>")),
                                width = '250px'
                            ),
                            footer = footer,
                            easyClose = TRUE)
                    ))
            })
            }
        )
}
    




library(shiny)
library(shinyBS)
ui <- fluidPage(
)
server <- function(input, output) {
    mod_errorModal_server('test',
                          title = 'title',
                          text = 'reactive')
}
shinyApp(ui, server)

