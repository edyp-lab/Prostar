mod_plotsMetacellHistos_ui <- function(id) {
    ns <- NS(id)
    tagList(
        
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
                                           obj, 
                                           pal, 
                                           pattern = reactive({NULL}),
                                           showSelect = reactive({TRUE})) {
    moduleServer(
        id,
        function(input, output, session) {
            ns <- session$ns
            
            rv <- reactiveValues(
                chooseTag = pattern()
            )
            
            output$chooseTagUI <- renderUI({
                obj()
                meta <- DAPAR::metacell.def(GetTypeofData(obj()))$node
                .ch <- meta[-which(meta == 'Any')]
                
                shinyjs::toggle(selectInput(ns('chooseTag'), 'Choose tag', 
                            choices = .ch,
                            width = '200px',
                            multiple = TRUE,
                            selected = pattern()),
                            condition = showSelect())
            })
            
            observeEvent(input$chooseTag, ignoreInit = TRUE,{
                rv$chooseTag <- input$chooseTag
            })

            output$histo_Metacell <- renderHighchart({
               req(rv$chooseTag)
                obj()

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
                req(rv$chooseTag)
                obj()
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
                req(rv$chooseTag)
                obj()
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
data("Exp1_R25_prot")

ui <- fluidPage(
    mod_plotsMetacellHistos_ui('test')
)

server <- function(input, output) {
    
    pattern <- c('Missing POV', 'Missing MEC')
    
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


