module_DensityplotU_ui <- function(id) {
    ns <- NS(id)
    highchartOutput(ns("Densityplot"))
}



#------------------------------------------------------------
module_Densityplot_server <- function(id, obj = reactive({NULL})) {
    
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        
        rv <- reactiveValues(
            
        )
        
        
    output$Densityplot <- renderHighchart({
        obj()
        rv$PlotParams$paletteForConditions
        rv$PlotParams$legendForSamples
        tmp <- NULL
        isolate({
            withProgress(message = "Making plot", value = 100, {
                pattern <- paste0(GetCurrentObjName(), ".densityplot")
                tmp <- DAPAR::densityPlotD_HC(obj(),
                                              legend = rv$PlotParams$legendForSamples,
                                              pal = unique(rv$PlotParams$paletteForConditions)
                )
                future(createPNGFromWidget(rv$tempplot$boxplot, pattern))
            })
        })
        tmp
    })
    
    }
    )
}