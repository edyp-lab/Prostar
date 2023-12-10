
mod_errorModal_ui <- function(id) {}


mod_errorModal_server <- function(id, 
                                  title = reactive({NULL}),
                                  text = reactive({NULL})){
    
    if (!requireNamespace("shiny", quietly = TRUE)) {
        stop("Please install shiny: BiocManager::install('shiny')")
    }
    
    shiny::moduleServer(
        id,
        function(input, output, session) {
            
            
            
            observeEvent(TRUE, ignoreInit = FALSE, {
                # shiny::showModal(
                #     shiny::modalDialog('test')
                # )
                
                shiny::showModal(
                    div(
                        id = 'errModal',
                        tags$style("#errModal .modal-dialog{width: 600px;}"),
                        shiny::modalDialog(
                            h3(title()),
                            tags$style("#tPanel {overflow-y:scroll; color: red;}"),
                            shiny::wellPanel(
                                id = "tPanel",
                                HTML(paste('> ', text(), collapse = "<br/>"))
                            )
                            ,easyClose = TRUE)
                    ))
            })
            }
        )
}
    




