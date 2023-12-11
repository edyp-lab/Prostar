source(system.file("ProstarApp/server", "mod_popover_for_help.R", package = 'Prostar'), local = TRUE)$value
source(system.file("ProstarApp/server", "mod_errorModal.R", package = 'Prostar'), local = TRUE)$value



#' @title   popover_for_help_ui and popover_for_help_server
#' @description  A shiny Module.
#'
#' @export
#' @importFrom shiny NS tagList
#' @importFrom shinyjs inlineCSS useShinyjs
#' @param id xxx
#' @rdname mod_set_pval_threshold
#'
mod_set_pval_threshold_ui <- function(id) {
    ns <- NS(id)
    wellPanel(
        width = '150px',
        useShinyjs(),
        popover_for_help_ui(ns("modulePopover_pValThreshold")),
        br(),
        tags$div(style = "align: center;display:inline-block; vertical-align: top;",
                 tags$style(HTML(".form-control {height: 20px; font-size: 13px;}")),
                 tags$style(HTML(".radio {padding-right: 0px; padding-bottom: 10px;}")),
                          radioButtons(ns('toto'), NULL, 
                                       choices = c('p-value' = 'pval', 
                                                   '-log10(p-value)' = 'logpval'))
                 ),
                 tags$div(style = "align: center;display:inline-block; vertical-align: top;",
                          uiOutput(ns('text1_UI')),
                          disabled(uiOutput(ns('text2_UI')))
                 ),
        tags$div(style = "align: center;display:inline-block; vertical-align: center; ",
                 actionButton(ns('ApplyThreshold'), 'Apply threshold')
        ),
        tags$div(style = "align: center;display:inline-block; vertical-align: center; padding-left: 20px;",
                 uiOutput(ns('showFDR_UI'))
        )
        
    )
}

#' @rdname mod_set_pval_threshold
#'
#' @param id xxx
#' @export
#'
mod_set_pval_threshold_server <- function(id,
                                          logpval_init = reactive({0}),
                                          fdr = reactive({NULL})) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        
        dataOut <- reactiveVal()
        
       
        .head <- "To perform the selection using a FDR threshold of x% : "
        .pt1 <- "Display in the table below the adjusted p-values. The proteins are then automatically sorted by increasing adjusted p-values"
        .pt2 <- "Spot the protein P which has the largest adjusted p-value below x%"
        .pt3 <- "Tune the p-value (or log p-value) threshold using a value between the p-value (or log p-value) of P and of the next protein below in the list."
        popover_for_help_server("modulePopover_pValThreshold",
                                title = h4("Significant threshold"),
                                content = HTML(paste0(.head, "<br>", 
                                                      "<ul>", 
                                                      "<li>", .pt1, "</li>", 
                                                      "<li>", .pt2, "</li>",
                                                      "<li>", .pt3, "</li>",
                                                      "</ul>"))
        )
        
        output$showFDR_UI <- renderUI({
            req(fdr())
            p(paste0('FDR = ', fdr(), ' %'))
        })
        
        
        output$text1_UI <- renderUI({
            logpval_init()
            textInput(ns('text1'), NULL, 
                      value = 10^logpval_init(), 
                      width = '100px')
        })
        
        output$text2_UI <- renderUI({
            logpval_init()
            textInput(ns('text2'), NULL, 
                      value = logpval_init(), 
                      width = '100px')
        })
        
        observe({
            shinyjs::toggleState('text2', condition = input$toto == 'logpval')
            shinyjs::toggleState('text1', condition = input$toto == 'pval')
        })
        
        observeEvent(input$text1, {
            req(input$toto == 'pval')
            updateTextInput(session, 'text2', value = -log10(as.numeric(input$text1)))
        })
        
        observeEvent(input$text2, {
            req(input$toto == 'logpval')
            updateTextInput(session, 'text1', value = 10^as.numeric((input$text2)))
        })
        
        
        observeEvent(input$ApplyThreshold, {
            dataOut(as.numeric(input$text2))
        })
        
        return(reactive({dataOut()}))
    }
    )
}



#------------------------------------------------

library(shiny)
library(shinyBS)
ui <- fluidPage(
    mod_set_pval_threshold_ui("Title")
)
server <- function(input, output) {
    logpval <- mod_set_pval_threshold_server(id = "Title",
                                             logpval_init = reactive({0.22}),
                                             fdr = reactive({3.8}))
    
    observe({
        print(logpval())
    })
}

shinyApp(ui, server)
