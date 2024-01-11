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
    
    tagList(
        uiOutput(ns('openURLButton_UI')),
        htmlOutput(ns("insertMD"))
    )
}

# Module Server

#' @rdname mod_insert_md
#' @export
mod_insert_md_server <- function(id, 
                                 url,
                                 link_URL = NULL){
    
    
    moduleServer(id, function(input, output, session){
        ns <- session$ns
        
        output$openURLButton_UI <- renderUI({
            req(!is.null(link_URL))
            shiny::actionLink(inputId = ns("openURL"), 
                              label = "Open in new tab")
        })
        
        
        observeEvent(input$openURL,{
            browseURL(link_URL)
        })
        
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
    
    mod_insert_md_server('tree', 
                         url = 'http://www.prostar-proteomics.org/md/presentation.md',
                         link_URL = 'https://www.prostar-proteomics.org/#Frequently_asked_questions')
    
}

shinyApp(ui = ui, server = server)
