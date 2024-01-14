#' @export
mod_SweetAlert_ui <- function(id) {}

#' @export
mod_SweetAlert_server <- function(id, 
                                  title = NULL,
                                  text = NULL, 
                                  showClipBtn = TRUE,
                                  type = 'error'){
    
    
    shiny::moduleServer(id,
                        function(input, output, session) {
                            
                            sendSweetAlert(
                                session = session,
                                title = NULL,
                                text = tags$div(style = "display:inline-block; vertical-align: top;",
                                                p(text),
                                                if (showClipBtn)
                                                    rclipButton(inputId = "clipbtn",
                                                            label = "",
                                                            clipText = text,
                                                            icon = icon("copy"),
                                                            class = actionBtnClass)
                                ),
                                type = type,  #success, info, question, error,
                                danger_mode = FALSE,
                                closeOnClickOutside = T,
                                showCloseButton = FALSE
                            )
                            
                        }
    )
}





library(shiny)
library(shinyBS)

ui <- fluidPage(
    actionButton('test', 'Test')
)

server <- function(input, output) {
    
    
    observeEvent(input$test, {
        mod_SweetAlert_server('test',
                              title = 'title',
                              text = 'reactive',
                              showClipBtn = FALSE,
                              type = 'warning')
    })
}

shinyApp(ui, server)





