mod_plotsMetacellHistos_ui <- function(id) {
    ns <- NS(id)
    tagList(
        shinyjs::useShinyjs(),

        #shinyjs::hidden(uiOutput(ns('chooseTagUI'))),
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
                                           showSelect = reactive({'auto'})) {
    moduleServer(id, function(input, output, session) {
            ns <- session$ns
            
            setShowSelect <- reactive({
                # Auto mode
                show <- FALSE
                if( showSelect() == 'auto')
                    show <- is.null(pattern())
                else
                    show <- showSelect()
                print(show)
                show
            })
            
            rv <- reactiveValues(
                chooseTag = pattern(),
                showSelect = setShowSelect()
            )
            
            tmp.tags <- mod_metacell_tree_server('tree_plot_metacell', obj = reactive({obj()}))
            
            observeEvent(tmp.tags()$values, ignoreNULL = FALSE, ignoreInit = TRUE,{
                
                rv$chooseTag <- tmp.tags()$values
            })
            
            
            # observe({
            #     shinyjs::toggle('div_tree_plot_metacell',
            #                     condition = !is.null(rv$showSelect)) 
            # })
            output$chooseTagUI <- renderUI({
                req(rv$showSelect)
                req(obj())
                mod_metacell_tree_ui(ns('tree_plot_metacell'))
             })

            output$histo_Metacell <- renderHighchart({
               tmp <- NULL
               tmp <- metacellHisto_HC(obj = obj(),
                                        pattern = rv$chooseTag,
                                        pal = pal()
                                        )
                tmp
            })



            output$histo_Metacell_per_lines <- renderHighchart({
               tmp <- NULL
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
                                   showSelect = reactive({'auto'})
                                   )
    })

}

shinyApp(ui = ui, server = server)


