# Module UI

#' @title   mod_insert_md_ui and mod_insert_md_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param url internal
#'
#' @rdname mod_insert_md
#'


#' @export 
#' @importFrom shiny NS tagList 
mod_insert_md_ui <- function(id){
    ns <- NS(id)
    htmlOutput(ns("insertMD"))
}

# Module Server

#' @rdname mod_insert_md
#' @export
mod_insert_md_server <- function(id, url){
    
    
    moduleServer(id, function(input, output, session){
        ns <- session$ns
        
        
        output$insertMD <- renderUI({
            tryCatch(
                {
                    includeMarkdown(readLines(url))
                }
                , warning = function(w) {
                    tags$p("URL not found<br>",conditionMessage(w))
                }, error = function(e) {
                    shinyjs::info(paste("URL not found:", conditionMessage(e), sep=" "))
                }, finally = {
                    #cleanup-code 
                })
            
        })
        
        
    })
    
}




ui <- fluidPage(
    mod_insert_md_ui('tree')
)


server <- function(input, output) {
    
    mod_insert_md_server('tree', 'http://www.prostar-proteomics.org/md/presentation.md')
    
}

shinyApp(ui = ui, server = server)
