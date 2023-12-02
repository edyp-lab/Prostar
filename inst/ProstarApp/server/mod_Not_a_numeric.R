
#' @title   mod_insert_md_ui and mod_insert_md_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#'
#' @rdname mod_Not_a_numeric
#'


#' @export 
#' @importFrom shiny NS tagList 
mod_Not_a_numeric_ui <- function(id){
    ns <- NS(id)
    
    uiOutput(ns("msg_not_numeric"))

}

# Module Server

#' @rdname mod_Not_a_numeric
#' @export
mod_Not_a_numeric_server <- function(id, n){
    
    
    moduleServer(id, function(input, output, session){
        ns <- session$ns
        
        output$msg_not_numeric <- renderUI({
                n()
                if (is.na(as.numeric(n()))) {
                    tags$p("Please choose a number, with dot as decimal separator")
                }
            })
    })
}


# ui <- fluidPage(
#     mod_insert_md_ui('tree')
# )
# 
# 
# server <- function(input, output) {
#     
#     mod_insert_md_server('tree', 'http://www.prostar-proteomics.org/md/presentation.md')
#     
# }
# 
# shinyApp(ui = ui, server = server)
