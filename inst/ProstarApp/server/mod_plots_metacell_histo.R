mod_plotsMetacellHistos_ui <- function(id) {
    ns <- NS(id)
    tagList(
        shinyjs::useShinyjs(),
        uiOutput(ns('info')),
        uiOutput(ns('chooseTagUI')),
        fluidRow(
            column(width = 4,
                   highchartOutput(ns("histo_Metacell")), height = "600px"
                   ),
            column(width = 4,
                   highchartOutput(ns("histo_Metacell_per_lines"))
                   ),
            column(width = 4,
                   highchartOutput(ns("histo_Metacell_per_lines_per_conditions"))
                   )
            )
        )
}


mod_plotsMetacellHistos_server <- function(id, 
                                           obj = reactive({NULL}), 
                                           pal = reactive({NULL}), 
                                           pattern = reactive({NULL}),
                                           showSelect = reactive({TRUE})) {
    moduleServer(id, function(input, output, session) {
            ns <- session$ns
            
            rv <- reactiveValues(
                chooseTag = pattern(),
                showSelect = if(is.null(pattern())) TRUE else showSelect()
            )
            
            
            # observeEvent(id, {
            #     rv$showSelect <- !is.null(pattern())
            # })
            
            # output$info <- renderUI({
            #     req(is.null(rv$chooseTag))
            #     print(rv$chooseTag)
            #     p("Info: no data to view. 'Pattern' is NULL")
            #     })
            

            output$chooseTagUI <- renderUI({
                req(showSelect())
                req(obj())
                meta <- DAPAR::metacell.def(GetTypeofData(obj()))$node
                .ch <- 
                    selectInput(ns('chooseTag'), 
                                'Select at least one tag to display statistics about', 
                                choices = meta[-which(meta == 'Any')],
                                width = '200px',
                                multiple = TRUE,
                                selected = rv$chooseTag)

            })
            
            observeEvent(input$chooseTag, ignoreInit = TRUE,{
                rv$chooseTag <- input$chooseTag
            })

            output$histo_Metacell <- renderHighchart({
               #req(rv$chooseTag)
                #obj()

                tmp <- NULL
                # isolate({
                tmp <- metacellHisto_HC(obj = obj(),
                                        pattern = rv$chooseTag,
                                        pal = pal()
                                        )
                # future(createPNGFromWidget(tmp,pattern))
                #  })
                tmp
            })



            output$histo_Metacell_per_lines <- renderHighchart({
               # req(rv$chooseTag)
                #obj()
                tmp <- NULL
                # isolate({
                # pattern <- paste0(GetCurrentObjName(),".MVplot2")
                tmp <-
                    metacellPerLinesHisto_HC(obj = obj(),
                                             pattern = rv$chooseTag,
                                             indLegend = c(2:length(colnames(Biobase::pData(obj()))))
                                             )
                # future(createPNGFromWidget(tmp,pattern))
                # })
                tmp
            })



            output$histo_Metacell_per_lines_per_conditions <- renderHighchart({
               # req(rv$chooseTag)
                #obj()
                tmp <- NULL
                # isolate({
                # pattern <- paste0(GetCurrentObjName(),".MVplot2")
                tmp <- metacellPerLinesHistoPerCondition_HC(obj = obj(),
                                                            pattern = rv$chooseTag,
                                                            pal = pal()
                                                            )
                # future(createPNGFromWidget(tmp,pattern))
                # })
                tmp
            })
        }
    )
}



# Example
# 

ui <- fluidPage(
    mod_plotsMetacellHistos_ui('test')
)

server <- function(input, output) {
    utils::data("Exp1_R25_prot", package='DAPARdata')
    
    pattern <- c('Missing POV', 'Missing MEC')
    pattern <- NULL
    observe({
        mod_plotsMetacellHistos_server('test',
                                   obj = reactive({Exp1_R25_prot}),
                                   pal = reactive({NULL}),
                                   pattern = reactive({pattern}),
                                   showSelect = reactive({TRUE})
                                   )
    })

}

shinyApp(ui = ui, server = server)


