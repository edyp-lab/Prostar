mod_Boxplot_ui <- function(id) {
    ns <- NS(id)
    tagList(
        useShinyjs(),
        highchartOutput(ns("BoxPlot")),
        imageOutput(ns("viewViolinPlot")),
        selectInput(ns("choosePlot"),
                    "Choose plot",
                    choices = c(
                        "violinplot" = "violinplot",
                        "boxplot" = "boxplot"
                    ),
                    width = "100px"
        )
    )
}




#------------------------------------------------------------
mod_Boxplot_server <- function(id, 
                               obj = reactive({NULL}),
                               name = NULL,
                               pal = reactive({NULL}),
                               params = reactive({NULL})) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        
        rv <- reactiveValues(
            
        )
        
        
    observeEvent(input$choosePlot, {
        switch(input$choosePlot,
               boxplot = {
                   shinyjs::hide("viewViolinPlot")
                   shinyjs::show("BoxPlot")
               },
               violinplot = {
                   shinyjs::hide("BoxPlot")
                   shinyjs::show("viewViolinPlot")
               }
        )
    })
    
    
    output$BoxPlot <- renderHighchart({
        obj()
        params()
        
        tmp <- NULL
        isolate({
            pattern <- paste0(name, ".boxplot")
            tmp <- DAPAR::boxPlotD_HC(obj(),
                                      conds = Biobase::pData(obj())$Condition,
                                      legend = params()$legendForSamples,
                                      pal = pal()
            )
            # future(createPNGFromWidget(tmp,pattern))
        })
        tmp
    })
    
    output$viewViolinPlot <- renderImage(
        {
            if (!requireNamespace("grDevices", quietly = TRUE)) {
                stop("Please install grDevices: BiocManager::install('grDevices')")
            }
            obj()
            params()
            tmp <- NULL
            
            isolate({
                
                # A temp file to save the output. It will be deleted after
                # renderImage
                # sends it, because deleteFile=TRUE.
                outfile <- tempfile(fileext = ".png")
                
                # Generate a png
                # png(outfile, width = 640, height = 480, units = "px")
                grDevices::png(outfile)
                pattern <- paste0(name, ".violinplot")
                tmp <- DAPAR::violinPlotD(obj(),
                                          conds = Biobase::pData(obj())$Condition,
                                          legend = params()$legendForSamples,
                                          pal = pal()
                )
                # future(createPNGFromWidget(tmp,pattern))
                dev.off()
            })
            # Return a list
            list(
                src = outfile,
                alt = "This is alternate text"
            )
        },
        deleteFile = TRUE
    )
    }
    )
}




# Example
#
ui <- fluidPage(
    mod_Boxplot_ui('tree')
)

server <- function(input, output) {
    
    utils::data('Exp1_R25_prot')
    mod_Boxplot_server('tree',
                       obj = reactive({Exp1_R25_prot}))
    
}

shinyApp(ui = ui, server = server)



