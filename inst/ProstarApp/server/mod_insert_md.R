# Module UI

#' @title   mod_insert_md_ui and mod_insert_md_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_insert_md
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_insert_md_ui <- function(id){
    ns <- NS(id)
    uiOutput(ns("insertMD"))
}

# Module Server

#' @rdname mod_insert_md
#' @export
#' @keywords internal

mod_insert_md_server <- function(id, url){
    
    
    moduleServer(id, function(input, output, session){
        ns <- session$ns
        
        
        output$insertMD <- renderUI({
            tryCatch(
                {
                    includeMarkdown(url)
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