#'
#'@examples
#' ui <- fluidPage(
#'    modulePopoverUI('test')
#')
#'server <- function(input, output) {
#'    callModule(modulePopover, "test",
#'        data = reactive(list(
#'            title = "Title",
#'            content = "Content"
#'        ))
#'    )
#'}
#'
#' shinyApp(ui = ui, server = server)





modulePopoverUI <- function(id) {
    ns <- NS(id)
    uiOutput(ns("customPopover"))
}

modulePopover <- function(input, output, session, data) {
    ns <- session$ns
    if (!requireNamespace("shinyBS", quietly = TRUE)) {
        stop("Please install shinyBS: BiocManager::install('shinyBS')")
    }
    output$customPopover <- renderUI({
        req(data())
        
        tagList(
        div(
            div(
                # edit1
                style = "display:inline-block; vertical-align: middle;
                padding-bottom: 5px;",
                HTML(paste0("<strong>", data()$title, "</strong>"))
            ),
            div(
                # edit2
                style = "display:inline-block; vertical-align: middle;
                padding-bottom: 5px;",
                if (!is.null(data()$color) && ("white" == data()$color)) {
                    tags$button(
                        id = ns("q1"), tags$sup("[?]"),
                        class = "Prostar_tooltip_white"
                    )
                } else {
                     tags$button(
                        id = ns("q1"), tags$sup("[?]"),
                        class = "Prostar_tooltip"
                    )
                },
                shinyBS::bsPopover(
                    id = ns("q1"), title = "",
                    content = data()$content,
                    placement = "right",
                    trigger = "hover",
                    options = list(container = "body")
                )
            )
        )
        )
    })
}
