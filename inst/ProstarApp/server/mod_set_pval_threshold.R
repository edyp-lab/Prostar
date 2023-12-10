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
   tagList(
       useShinyjs(),
       popover_for_help_ui(ns("modulePopover_pValThreshold")),
       fluidRow(
            column(width=2,
                #tags$style(HTML(".radio {padding-bottom: 30px; font-size: 16px;}")),
                radioButtons(ns('toto'), NULL, 
                                choices = c('p-value' = 'pval', 
                                            '-log10(p-value)' = 'logpval'))
            ),
            column(width=3,
                textInput(ns('text1'), NULL, value = 1, width = '100px'),
                disabled(textInput(ns('text2'), NULL, value = 0, width = '100px'))
            ),
            column(width=3,
                   actionButton(ns('ApplyThreshold'), 'Apply threshold'))
            )
   )
}

#' @rdname mod_set_pval_threshold
#'
#' @param id xxx
#' @export
#'
mod_set_pval_threshold_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

    dataOut <- reactiveVal()
    
    
    .head <- "To perform the selection using a FDR threshold of x% : "
    .pt1 <- "Display in the table below the adjusted p-values. The proteins are then automatically sorted by increasing adjusted p-values"
    .pt2 <- "Spot the protein P which has the largest adjusted p-value below x%"
    .pt3 <- "Tune the p-value (or log p-value) threshold using a value between the p-value (or log p-value) of P and of the next protein below in the list."
    popover_for_help_server("modulePopover_pValThreshold",
                            title = "Significant threshold",
                            content = HTML(paste0(.head, "<br>", 
                                                  "<ul>", 
                                                  "<li>", .pt1, "</li>", 
                                                  "<li>", .pt2, "</li>",
                                                  "<li>", .pt3, "</li>",
                                                  "</ul>"))
    )
    
    
    
    
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
    logpval <- mod_set_pval_threshold_server(id = "Title")
    
    observe({
        print(logpval())
    })
}

shinyApp(ui, server)
