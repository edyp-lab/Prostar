source(system.file("ProstarApp/server", "mod_popover_for_help.R", package = 'Prostar'), local = TRUE)$value
source(system.file("ProstarApp/server", "mod_errorModal.R", package = 'Prostar'), local = TRUE)$value



#' @title   popover_for_help_ui and popover_for_help_server
#' @description  A shiny Module.
#'
#' @export
#' @importFrom shiny NS tagList
#' @importFrom shinyjs inlineCSS useShinyjs
#' @param id xxx
#' @rdname mod_DetQuantImpValues
#'
mod_DetQuantImpValues_ui <- function(id) {
    ns <- NS(id)
    tagList(
        h5("The missing values will be imputed by the following values :"),
        DT::dataTableOutput(ns("detQuantValues_DT"))
    )
}

#' @rdname mod_DetQuantImpValues
#'
#' @param id xxx
#' @export
#'
mod_DetQuantImpValues_server <- function(id,
                                         obj = reactive({NULL}),
                                         quant = reactive({1}),
                                          factor = reactive({1})
                                         ) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        
        output$detQuantValues_DT <- DT::renderDataTable(server = TRUE, {
            req(obj())
            
            values <- getQuantile4Imp(
                Biobase::exprs(obj()),
                quant() / 100, factor()
            )
            DT::datatable(as.data.frame(t(values$shiftedImpVal)),
                          rownames = FALSE,
                          options = list(initComplete = initComplete(),
                                         dom = "t",
                                         bLengthChange = FALSE
                          )
            )
        })
        
    }
    )
}



#------------------------------------------------

library(shiny)
library(shinyBS)
ui <- fluidPage(
    mod_DetQuantImpValues_ui("Title")
    
)
server <- function(input, output) {
    
    data("Exp1_R25_prot")
    mod_DetQuantImpValues_server(id = "Title",
                                 obj = reactive({Exp1_R25_prot}))
}

shinyApp(ui, server)
