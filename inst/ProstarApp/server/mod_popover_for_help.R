#' @title   popover_for_help_ui and popover_for_help_server
#' @description  A shiny Module.
#'
#' @export
#' @importFrom shiny NS tagList
#' @importFrom shinyjs inlineCSS useShinyjs
#' @param id xxx
#' @rdname popover_for_help
#'
popover_for_help_ui <- function(id) {
    ns <- NS(id)
    tagList(
        shinyjs::useShinyjs(),
        shinyjs::inlineCSS(pop_css),
        
        div(style = "display:inline-block; vertical-align: middle; padding-bottom: 5px;",
            uiOutput(ns("write_title_ui"))
            ),
        div(style = "display:inline-block; vertical-align: middle;padding-bottom: 5px;",
            uiOutput(ns("dot")),
                uiOutput(ns("show_Pop"))
            )
    )
}

# Module Server

#' @title xxx
#'
#' @description xxx
#'
#' @export
#'
#' @importFrom shinyBS bsPopover addPopover bsTooltip
#'
#' @return xxx
#'
#' @examples
#' if (interactive()) {
#'     library(shiny)
#'     library(shinyBS)
#'     ui <- fluidPage(
#'         popover_for_help_ui("Title")
#'     )
#'     server <- function(input, output) {
#'         popover_for_help_server(
#'             id = "Title",
#'             data = list(
#'                 title = "Test",
#'                 content = "Test"
#'             )
#'         )
#'     }
#'     shinyApp(ui, server)
#' }
#'
#' @rdname popover_for_help
#'
#' @param id xxx
#' @param data xxx
#'
popover_for_help_server <- function(id, title, content) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        
        output$write_title_ui <- renderUI({
            req(title)
            title
        })
        
        output$dot <- renderUI({
            tags$button(tags$sup("[?]"), class = "Prostar_tooltip")
        })
        
        output$show_Pop <- renderUI({
            shinyBS::bsTooltip(ns("dot"), HTML(content), trigger = "hover")
        })
    })
}



pop_css <- "button.Prostar_tooltip {
    background:none;
    color: #2EA8B1;
    border:none;
    padding-left:1ch;
    font: inherit;
    /*border is optional*/
        /*border-bottom:1px solid #444;*/
    cursor: pointer;
    font-weight: bold;
    display: inline-block;
    padding:0;
}

button.Prostar_tooltip_white {
    background:none;
    color: white;
    border:none;
    padding-left:1ch;
    font: inherit;
    /*border is optional*/
        /*border-bottom:1px solid #444;*/
    cursor: pointer;
    font-weight: bold;
    display: inline-block;
    padding:0;
}

.input-color {
    position: relative;
}
.input-color input {
    padding-left: 15px;
    border: 0px;
    background: transparent;
}
.input-color .color-box {
    width: 15px;
    height: 15px;
    display: inline-block;
    background-color: #ccc;
    position: absolute;
    left: 5px;
    top: 5px;

}"



library(shiny)
library(shinyBS)
ui <- fluidPage(
    popover_for_help_ui("Title")
)
server <- function(input, output) {
    txt <- "To perform the selection using a FDR threshold of x% :<br><ul><li>Display in the table below the adjusted p-values. The proteins are then automatically sorted by increasing adjusted p-values</li><li>number two</li></ul>"
    
    #"Test:<br> <ul><li>number one</li><li>number two</li></ul>"
    
    popover_for_help_server(
        id = "Title",
        title = "Test",
        content = HTML(txt)
    )
}
shinyApp(ui, server)