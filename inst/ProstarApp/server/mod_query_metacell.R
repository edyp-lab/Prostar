

options(shiny.reactlog=TRUE) 


mod_query_metacell_ui <- function(id) {
    ns <- NS(id)
    tagList(
        div(
            fluidRow(
                column(2, uiOutput(ns('testUI'))),
                column(2, uiOutput(ns("Choose_keepOrRemove_ui"))),
                column(2, uiOutput(ns("choose_metacellFilters_ui"))),
                column(6, tagList(
                    uiOutput(ns("show_example_ui")),
                    uiOutput(ns("MetacellFilters_widgets_set2_ui"))
                ))
            ),
            div(
                style = "display:inline-block; vertical-align: middle;
                align: center;",
                uiOutput(ns("metacellFilter_request_ui"))
            )
        )
    )
}





mod_query_metacell_server <- function(id,
                                      obj = reactive({NULL}),
                                      keep_vs_remove = reactive({NULL}),
                                      filters = reactive({NULL}),
                                      val_vs_percent = reactive({NULL}),
                                      operator = reactive({NULL}),
                                      reset = reactive({NULL})
                                      ){
    rv <- reactiveValues(
        indices = NULL,
        trigger = NULL,
        params = NULL,
        query = NULL,
        tags = NULL
    )

    dataOut <- reactiveValues(
        trigger = NULL,
        params = list(),
        query = NULL
    )
    
    
    observeEvent(id,{
        if (is.null(obj()))
            stop('obj() is NULL')
    }, priority = 1000)
    
    moduleServer(id, function(input, output, session) {
            ns <- session$ns

            # callModule(modulePopover, "metacellTag_help",
            #     data = reactive(list(
            #         title = "Nature of data to filter",
            #         content = "See the FAQ at prostar-proteomics.org"
            #     ))
            # )


            # observeEvent(id, {
            #     print(paste0("mod_query_metacell_server(id = '", id, "')"))
            #     #print(obj())
            #     print(paste0('keep_vs_remove() = ', keep_vs_remove()))
            #     print(paste0('filters() = ', filters()))
            #     print(paste0('val_vs_percent() = ', val_vs_percent()))
            #     print(paste0('operator() = ', operator()))
            #     print(paste0('reset() = ', reset()))
            #     
            #    # browser()
            # })
            
            
            output$testUI <- renderUI({
                mod_metacell_tree_ui(ns('tree'))
            })
            
            callModule(modulePopover, "filterScope_help",
                data = reactive(list(
                    title = "Scope",
                    content = HTML(
                        paste0("To filter the missing values, the choice of
                        the lines to be kept is made by different options:"),
                        ("<ul>"),
                        ("<li><strong>None</strong>: No filtering, the
                        quantitative data is left unchanged.</li>"),
                        ("<li><strong>(Remove) Empty lines</strong>: All the
                        lines with 100% of missing values are filtered
                            out.</li>"),
                        ("<li><strong>Whole Matrix</strong>: The lines
                        (across all conditions) which contain less quantitative
                        value than a user-defined threshold are kept;</li>"),
                        ("<li><strong>For every condition</strong>: The lines
                        for which each condition contain less quantitative
                        value than a user-defined threshold are deleted;</li>"),
                        ("<li><strong>At least one condition</strong>: The
                        lines for which at least one condition contain less
                        quantitative value than a user-defined threshold are
                            deleted.</li>"),
                        ("</ul>")
                    )
                ))
            )

            rv.widgets <- reactiveValues(
                MetacellTag = NULL,
                MetacellFilters = "None",
                KeepRemove = "delete",
                metacell_value_th = 0,
                metacell_percent_th = 0,
                val_vs_percent = "Count",
                metacellFilter_operator = "<="
            )

            rv <- reactiveValues(
                tags = NULL
            )
            
            
            observeEvent(req(reset()), {
                print('In reset mode')
                rv.widgets$MetacellTag <- NULL
                rv.widgets$MetacellFilters <- "None"
                rv.widgets$KeepRemove <- "delete"
                rv.widgets$metacell_value_th <- 0
                rv.widgets$metacell_percent_th <- 0
                rv.widgets$val_vs_percent <- "Count"
                rv.widgets$metacellFilter_operator <- "<="
                
                rv$tags <- NULL
                dataOut$trigger <- NULL
                dataOut$params <- list()
                dataOut$query <- NULL
                dataOut$indices <- NULL
            })

           
            observe({
                req(obj())
                rv$tmp.tags <- mod_metacell_tree_server('tree', 
                                                        level = GetTypeofData(obj()),
                                                        reset = reactive({reset()})
                                                        )
            })

            observeEvent(req(rv$tmp.tags()), ignoreNULL = TRUE, {
                rv.widgets$MetacellTag <- rv$tmp.tags()
            })
            
            
            observeEvent(input$ChooseKeepRemove, {
                .value <- input$ChooseKeepRemove
                rv.widgets$KeepRemove <- .value
            })
            observeEvent(input$ChooseMetacellFilters, {
                .value <- input$ChooseMetacellFilters
                rv.widgets$MetacellFilters <- .value
            })
            observeEvent(input$choose_val_vs_percent, {
                .value <- input$choose_val_vs_percent
                rv.widgets$val_vs_percent <- .value
            })
            observeEvent(input$choose_metacell_value_th, {
                .value <- input$choose_metacell_value_th
                rv.widgets$metacell_value_th <- .value
            })
            observeEvent(input$choose_metacell_percent_th, {
                .value <- input$choose_metacell_percent_th
                rv.widgets$metacell_percent_th <- .value
            })
            observeEvent(input$choose_metacellFilter_operator, {
                .value <- input$choose_metacellFilter_operator
                rv.widgets$metacellFilter_operator <- .value
            })

            # output$toto <- renderUI({
            #     
            #     
            # })

            
            output$Choose_keepOrRemove_ui <- renderUI({
                req(rv.widgets$MetacellTag)
                
                radioButtons(ns("ChooseKeepRemove"),
                             "Type of filter operation",
                             choices = keep_vs_remove(),
                             selected = rv.widgets$KeepRemove
                             )
            })


            output$choose_metacellFilters_ui <- renderUI({
                req(rv.widgets$MetacellTag)
                selectInput(ns("ChooseMetacellFilters"),
                            modulePopoverUI(ns("filterScope_help")),
                            choices = filters(),
                            selected = rv.widgets$MetacellFilters,
                            width = "200px"
                            )
            })



            output$show_example_ui <- renderUI({
                req(rv.widgets$MetacellTag)
                req(rv.widgets$MetacellFilters != "None")

                mod_filtering_example_server(id = "filteringExample",
                                             obj = reactive({obj()}),
                                             indices = reactive({CompileIndices()}),
                                             params = reactive({rv.widgets}),
                                             txt = reactive({WriteQuery()})
                                             )

                mod_filtering_example_ui(ns("filteringExample"))
            })


            output$MetacellFilters_widgets_set2_ui <- renderUI({
                req(rv.widgets$MetacellTag)
                req(!(rv.widgets$MetacellFilters %in% c("None", "WholeLine")))

                callModule(modulePopover, "choose_val_vs_percent_help",
                    data = reactive(list(
                        title = paste("#/% of values to ", rv.widgets$KeepRemove),
                        content = "Define xxx"
                    ))
                )

                tagList(
                    fluidRow(
                        column(4,
                            radioButtons(ns("choose_val_vs_percent"),
                                         modulePopoverUI(ns("choose_val_vs_percent_help")),
                                         choices = val_vs_percent(),
                                         selected = rv.widgets$val_vs_percent
                                         )
                        ),
                        column(8,
                            selectInput(ns("choose_metacellFilter_operator"),
                                        "Choose operator()",
                                        choices = operator(),
                                        selected = rv.widgets$metacellFilter_operator,
                                        width = "100px"
                                        ),
                            uiOutput(ns("choose_value_ui")),
                            uiOutput(ns("choose_percentage_ui"))
                        )
                    )
                )
            })

            output$choose_value_ui <- renderUI({
                req(rv.widgets$val_vs_percent == "Count")
                req(!(rv.widgets$MetacellFilters %in% c("None", "WholeLine")))

                callModule(modulePopover, "metacell_value_th_help",
                    data = reactive(list(
                        title = "Count threshold",
                        content = "Define xxx"
                    ))
                )

                tagList(
                    modulePopoverUI(ns("modulePopover_keepVal")),
                    selectInput(ns("choose_metacell_value_th"),
                        modulePopoverUI(ns("metacell_value_th_help")),
                        choices = getListNbValuesInLines(obj(), type = rv.widgets$MetacellFilters),
                        selected = rv.widgets$metacell_value_th,
                        width = "150px"
                    )
                )
            })



            output$choose_percentage_ui <- renderUI({
                req(rv.widgets$val_vs_percent == "Percentage")
                req(!(rv.widgets$MetacellFilters %in% c("None", "WholeLine")))

                callModule(modulePopover, "metacell_percent_th_help",
                    data = reactive(list(
                        title = "Percentage threshold",
                        content = "Define xxx"
                    ))
                )
                tagList(
                    modulePopoverUI(ns("modulePopover_keepVal_percent")),
                    sliderInput(ns("choose_metacell_percent_th"),
                        modulePopoverUI(ns("metacell_percent_th_help")),
                        min = 0,
                        max = 100,
                        step = 1,
                        value = rv.widgets$metacell_percent_th,
                        width = "250px"
                    )
                )
            })


            WriteQuery <- reactive({
                req(rv.widgets$MetacellTag)
                if (rv.widgets$MetacellFilters == "None") {
                    txt_summary <- "No filtering is processed."
                } else if (rv.widgets$MetacellFilters == "WholeLine") {
                    txt_summary <- paste(
                        rv.widgets$KeepRemove,
                        "lines that contain only ",
                        paste0(rv.widgets$MetacellTag, collapse=', ')
                    )
                } else {
                    text_method <- switch(rv.widgets$MetacellFilters,
                        "WholeMatrix" = "the whole matrix.",
                        "AllCond" = "each condition.",
                        "AtLeastOneCond" = "at least one condition."
                    )

                    if (rv.widgets$val_vs_percent == "Count") {
                        text_threshold <- rv.widgets$metacell_value_th
                    } else {
                        text_threshold <- paste(
                            as.character(rv.widgets$metacell_percent_th),
                            " %", sep = ""
                        )
                    }

                    txt_summary <- paste(rv.widgets$KeepRemove,
                                         " lines where number of (",
                                         paste0(rv.widgets$MetacellTag, collapse=', '),
                                         ") data ",
                                         rv.widgets$metacellFilter_operator,
                                         " ",
                                         text_threshold,
                                         " in ",
                                         text_method
                                         )
                }
                txt_summary
            })

            output$metacellFilter_request_ui <- renderUI({
                txt_summary <- paste("You are about to ", WriteQuery())
                tags$p(txt_summary, style = "font-size: small; text-align : center; color: purple;")
            })

            # Set useless widgets to default values
            observeEvent(rv.widgets$MetacellFilters == "WholeLine",
                {
                    rv.widgets$metacell_percent_th <- 0
                    rv.widgets$metacell_value_th <- 0
                    rv.widgets$val_vs_percent <- "Percentage"
                },
                priority = 1000
            )


            CompileIndices <- reactive({
                req(obj())
                req(rv.widgets$MetacellTag)
                req(rv.widgets$MetacellFilters != "None")

                
                th <- switch(rv.widgets$val_vs_percent,
                    Percentage = rv.widgets$metacell_percent_th / 100,
                    Count = as.integer(rv.widgets$metacell_value_th)
                )
                
                
                DAPAR::GetIndices_MetacellFiltering(
                    obj = obj(),
                    level = GetTypeofData(obj()),
                    pattern = rv.widgets$MetacellTag,
                    type = rv.widgets$MetacellFilters,
                    percent = rv.widgets$val_vs_percent == "Percentage",
                    op = rv.widgets$metacellFilter_operator,
                    th = th
                )
            })


            
            observeEvent(req(CompileIndices()), {
                dataOut$trigger <- as.numeric(Sys.time())
                dataOut$params<- list(
                    MetacellTag = rv.widgets$MetacellTag,
                    KeepRemove = rv.widgets$KeepRemove,
                    MetacellFilters = rv.widgets$MetacellFilters,
                    metacell_percent_th = rv.widgets$metacell_percent_th,
                    metacell_value_th = rv.widgets$metacell_value_th,
                    val_vs_percent = rv.widgets$val_vs_percent,
                    metacellFilter_operator = rv.widgets$metacellFilter_operator
                )
                
                dataOut$query <- WriteQuery()
                dataOut$indices <- CompileIndices()
                
                # # SELF RESET
                # rv.widgets$MetacellTag <- NULL
                # rv.widgets$MetacellFilters <- "None"
                # rv.widgets$KeepRemove <- "delete"
                # rv.widgets$metacell_value_th <- 0
                # rv.widgets$metacell_percent_th <- 0
                # rv.widgets$val_vs_percent <- "Count"
                # rv.widgets$metacellFilter_operator <- "<="
                # 
                # rv$tags <- NULL
                # dataOut <- list()
            })

            reactive({dataOut})
        }
    )
}




# Example
# 
library(DAPAR)
library(DAPARdata)
library(Prostar)
library(shinyBS)

    ui <- fluidPage(
    tagList(
        useShinyjs(),
        actionButton('reset', 'Reset'),
        mod_query_metacell_ui('query'),
        uiOutput('res'),
        shinyjs::disabled(actionButton('perform', 'Perform')),
        
    )
)

server <- function(input, output) {
    
    
    
    utils::data("Exp1_R25_prot")
    
    tmp <- mod_query_metacell_server('query', 
                                     obj = reactive({Exp1_R25_prot}),
                                     keep_vs_remove = reactive({'delete'}),
                                     filters = reactive({'WholeLine'}),
                                     reset = reactive({input$perform})
                                         )
 
    observeEvent(req(tmp()$trigger), {
        print(tmp()$indices)
        shinyjs::toggleState("perform",
                             condition = length(tmp()$indices) > 0
        )
        
    })
    
    output$res <- renderUI({
        p(paste0(tmp()$params$MetacellTag, collapse='\n'))
    })
}

shinyApp(ui = ui, server = server)

