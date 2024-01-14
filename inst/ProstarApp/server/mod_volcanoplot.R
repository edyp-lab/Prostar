

#' @title   popover_for_help_ui and popover_for_help_server
#' @description  A shiny Module.
#'
#' @export
#' @importFrom shiny NS tagList
#' @importFrom shinyjs inlineCSS useShinyjs
#' @param id xxx
#' @rdname mod_volcanoplot
#'
mod_volcanoplot_ui <- function(id) {
    ns <- NS(id)
    tagList(
        highchartOutput(ns("volcanoPlot"), width = "600px", height = "600px"),
        uiOutput(ns("quantiDT"))
    )
}

#' @rdname mod_volcanoplot
#'
#' @param id xxx
#' @export
#'
mod_volcanoplot_server <- function(id,
                                   data = reactive({NULL}),
                                   comp = reactive({NULL}),
                                   tooltip = reactive({NULL})) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        
        output$quantiDT <- renderUI({
            req(input$eventPointClicked)
            
            if (DAPAR::GetTypeofData(rv$current.obj) == "protein") {
                if (is.null(DAPAR::GetMatAdj(rv$current.obj))) {
                    shinyBS::bsCollapse(id = ns("collapseVolcanoInfos"),
                        open = "Protein",
                        multiple = TRUE,
                        shinyBS::bsCollapsePanel("Protein",
                                                 tagList(
                                                     uiOutput(ns("Warning_Infos")),
                                                     DT::dataTableOutput(ns("Infos"))
                                                 ), style = "info")
                    )
                } else {
                    shinyBS::bsCollapse(id = ns("collapseVolcanoInfos"),
                        open = c("Protein", "Specific peptides", "Shared peptides"),
                        multiple = TRUE,
                        shinyBS::bsCollapsePanel("Protein",
                                                 tagList(
                                                     uiOutput(ns("Warning_Infos")),
                                                     DT::dataTableOutput(ns("Infos"))
                                                 ), style = "info"),
                        shinyBS::bsCollapsePanel("Specific peptides",
                                                 tagList(
                                                     uiOutput(ns("Warning_specificPeptidesInfos")),
                                                     DT::dataTableOutput(ns("specificPeptidesInfos"))
                                                 ), style = "primary"),
                        shinyBS::bsCollapsePanel("Shared peptides",
                                                 tagList(
                                                     uiOutput(ns("Warning_sharedPeptidesInfos")),
                                                     DT::dataTableOutput(ns("sharedPeptidesInfos"))
                                                 ), style = "primary")
                    )
                }
            } else if (GetTypeofData(rv$current.obj) == "peptide") {
                shinyBS::bsCollapse(id = ns("collapseVolcanoInfos"),
                    open = "Peptide",
                    multiple = TRUE,
                    shinyBS::bsCollapsePanel("Peptide",
                                             tagList(
                                                 uiOutput(ns("Warning_Infos")),
                                                 DT::dataTableOutput(ns("Infos"))
                                             ),
                                             style = "info"
                    )
                )
            }
        })
        
        
        
        GetSortingIndices <- reactive({
            req(comp())
            
            condition1 <- gsub("[()]", "", strsplit(comp(), "_vs_")[[1]][1])
            condition2 <- gsub("[()]", "", strsplit(comp(), "_vs_")[[1]][2])
            .ind <- (which(Biobase::pData(rv$current.obj)$Condition == condition1))
            if (length(grep("all", condition2)) == 0) {
                ind <- c(
                    which(Biobase::pData(rv$current.obj)$Condition == condition1),
                    which(Biobase::pData(rv$current.obj)$Condition == condition2)
                )
            } else {
                ind <- c(
                    which(Biobase::pData(rv$current.obj)$Condition == condition1),
                    c(1:nrow(Biobase::pData(rv$current.obj)))[-.ind]
                )
            }
            ind
        })
        
        GetBorderIndices <- reactive({
            conds <- (Biobase::pData(rv$current.obj)$Condition)[GetSortingIndices()]
            ## build index for border-formatting
            borders_index <- unlist(lapply(
                unique(conds),
                function(x) {
                    which.max(x == conds)
                }
            ))
            borders_index
        })
        
        
        
        output$Warning_sharedPeptidesInfos <- renderUI({
            GetDataFor_sharedPeptidesInfos()
            if (nrow(GetDataFor_sharedPeptidesInfos()) > 153) {
                p(MSG_WARNING_SIZE_DT)
            }
        })
        
        
        
        
        GetDataFor_sharedPeptidesInfos <- reactive({
            req(comp())
            
            ind <- GetSortingIndices()
            borders_index <- GetBorderIndices()
            
            .ind <- last(grep(pattern = "peptide", names(rv$dataset)))
            prev.dataset <- rv$dataset[[names(rv$dataset)[.ind]]]
            
            prot <- GetExprsClickedProtein()
            prot.indice <- rownames(prot)
            
            data <- getDataForExprs(prev.dataset, rv$settings_nDigits)
            data <- data[, c(ind, (ind + ncol(data) / 2))]
            
            Xspec <- DAPAR::GetMatAdj(rv$current.obj)$matWithUniquePeptides
            Xshared <- DAPAR::GetMatAdj(rv$current.obj)$matWithSharedPeptides
            
            i <- which(colnames(Xspec) == prot.indice)
            specificPeptidesIndices <- which(Xspec[, i] == 1)
            allPeptidesIndices <- which(Xshared[, i] == 1)
            peptidesIndices <- setdiff(allPeptidesIndices, specificPeptidesIndices)
            data <- data[peptidesIndices, ]
            data
        })
        
        output$sharedPeptidesInfos <- DT::renderDataTable(server = TRUE, {
            data <- GetDataFor_sharedPeptidesInfos()
            c.tags <- BuildColorStyles(rv$current.obj)$tags
            c.colors <- BuildColorStyles(rv$current.obj)$colors
            
            dt <- DT::datatable(
                data,
                extensions = c("Scroller"),
                options = list(
                    initComplete = initComplete(),
                    dom = "frtip",
                    blengthChange = FALSE,
                    displayLength = 20,
                    ordering = FALSE,
                    server = FALSE,
                    columnDefs = list(
                        list(
                            targets = c(((ncol(data) / 2) + 1):(ncol(data))),
                            visible = FALSE
                        )
                    )
                )
            ) %>%
                DT::formatStyle(
                    colnames(data)[1:(ncol(data) / 2)],
                    colnames(data)[((ncol(data) / 2) + 1):(ncol(data))],
                    backgroundColor = DT::styleEqual(c.tags, c.colors)
                ) %>%
                DT::formatStyle(GetBorderIndices(), borderLeft = "3px solid #000000")
            
            dt
        })
        
        output$Warning_specificPeptidesInfos <- renderUI({
            GetDataFor_specificPeptidesInfos()
            if (nrow(GetDataFor_specificPeptidesInfos()) > 153) {
                p(MSG_WARNING_SIZE_DT)
            }
        })
        
        GetDataFor_specificPeptidesInfos <- reactive({
            req(comp())
            
            ind <- GetSortingIndices()
            borders_index <- GetBorderIndices()
            .ind <- last(grep(pattern = "peptide", names(rv$dataset)))
            prev.dataset <- rv$dataset[[names(rv$dataset)[.ind]]]
            
            prot <- GetExprsClickedProtein()
            prot.indice <- rownames(prot)
            
            data <- getDataForExprs(prev.dataset, rv$settings_nDigits)
            data <- data[, c(ind, (ind + ncol(data) / 2))]
            
            
            Xspec <- DAPAR::GetMatAdj(rv$current.obj)$matWithUniquePeptides
            
            i <- which(colnames(Xspec) == prot.indice)
            peptidesIndices <- which(Xspec[, i] == 1)
            data <- data[peptidesIndices, ]
            data
        })
        
        
        output$specificPeptidesInfos <- DT::renderDataTable(server = TRUE, {
            data <- GetDataFor_specificPeptidesInfos()
            c.tags <- BuildColorStyles(rv$current.obj)$tags
            c.colors <- BuildColorStyles(rv$current.obj)$colors
            
            dt <- DT::datatable(
                data,
                extensions = c("Scroller"),
                options = list(
                    initComplete = initComplete(),
                    dom = "frtip",
                    blengthChange = FALSE,
                    displayLength = 20,
                    ordering = FALSE,
                    columnDefs = list(
                        list(
                            targets = c(
                                ((ncol(data) / 2) + 1):(ncol(data))
                            ),
                            visible = FALSE
                        )
                    )
                )
            ) %>%
                DT::formatStyle(
                    colnames(data)[1:(ncol(data) / 2)],
                    colnames(data)[((ncol(data) / 2) + 1):(ncol(data))],
                    backgroundColor = DT::styleEqual(c.tags, c.colors)
                ) %>%
                DT::formatStyle(GetBorderIndices(), borderLeft = "3px solid #000000")
            
            dt
        })
        
        
        ## ---------------------------------------------------------
        GetExprsClickedProtein <- reactive({
            req(rv$current.obj)
            req(comp() != "None")
            req(input$eventPointClicked)
            rv$widgets$hypothesisTest$th_logFC
            rv$widgets$anaDiff$th_pval
            data()
            
            ind <- GetSortingIndices()
            # browser()
            this.index <- as.integer(strsplit(input$eventPointClicked, "_")[[1]][1])
            this.series.name <- strsplit(input$eventPointClicked, "_")[[1]][2]
            
            data <- getDataForExprs(rv$current.obj, rv$settings_nDigits)
            data <- data[, c(ind, (ind + ncol(data) / 2))]
            
            .thlogfc <- rv$widgets$hypothesisTest$th_logFC
            index.g1 <- which((-log10(data()$P_Value) >= rv$widgets$anaDiff$th_pval
            ) & (abs(data()$logFC) >= as.numeric(.thlogfc)))
            
            data.g1 <- data[index.g1, ]
            data.g2 <- data[-index.g1, ]
            
            switch(this.series.name,
                   g1 = data <- data.g1[this.index + 1, ],
                   g2 = data <- data.g2[this.index + 1, ]
            )
            data
        })
        
        
        
        output$Warning_Infos <- renderUI({
            GetDataFor_Infos()
            if (nrow(GetDataFor_Infos()) > 153) {
                p(MSG_WARNING_SIZE_DT)
            }
        })
        
        
        
        
        GetDataFor_Infos <- reactive({
            req(comp())
            data <- GetExprsClickedProtein()
            data
        })
        
        ## -------------------------------------------------------------
        output$Infos <- DT::renderDataTable(server = TRUE, {
            req(comp())
            
            borders_index <- GetBorderIndices()
            data <- GetExprsClickedProtein()
            c.tags <- BuildColorStyles(rv$current.obj)$tags
            c.colors <- BuildColorStyles(rv$current.obj)$colors
            
            dt <- DT::datatable(
                data,
                extensions = c("Scroller"),
                options = list(
                    initComplete = initComplete(),
                    dom = "frtip",
                    blengthChange = FALSE,
                    displayLength = 20,
                    ordering = FALSE,
                    header = FALSE,
                    columnDefs = list(
                        list(
                            targets = c(((ncol(data) / 2) + 1):(ncol(data))),
                            visible = FALSE
                        )
                    )
                )
            ) %>%
                DT::formatStyle(
                    colnames(data)[1:(ncol(data) / 2)],
                    colnames(data)[((ncol(data) / 2) + 1):(ncol(data))],
                    backgroundColor = DT::styleEqual(c.tags, c.colors)
                ) %>%
                DT::formatStyle(borders_index, borderLeft = "3px solid #000000")
            
            
            
            dt
        })
        
        ## ---------------------------------------------------------------------
        output$volcanoPlot <- renderHighchart({
            rv$widgets$anaDiff$th_pval
            rv$widgets$hypothesisTest$th_logFC
            rv$colorsVolcanoplot
            data()$P_Value
            req(length(data()$logFC) > 0)
            tooltip()
            
            # browser()
            isolate({
                withProgress(message = "Building plot...", detail = "", value = 0, {
                    m <- match.metacell(DAPAR::GetMetacell(rv$current.obj),
                                        pattern = c("Missing", "Missing POV", "Missing MEC"),
                                        level = DAPAR::GetTypeofData(rv$current.obj)
                    )
                    if (length(which(m)) > 0) {
                        return(NULL)
                    }
                    df <- data.frame(
                        x = data()$logFC,
                        y = -log10(data()$P_Value),
                        index = 1:nrow(Biobase::fData(rv$current.obj))
                    )
                    
                    if (length(tooltip()) > 0 && sum(is.na(tooltip())) == 0) {
                        df <- cbind(
                            df,
                            Biobase::fData(rv$current.obj)[tooltip()]
                        )
                    }
                    
                    colnames(df) <- gsub(".", "_", colnames(df), fixed = TRUE)
                    if (ncol(df) > 3) {
                        colnames(df)[4:ncol(df)] <-
                            paste("tooltip_", colnames(df)[4:ncol(df)], sep = "")
                    }
                    
                    clickFun <-
                        JS(paste0(
                            "function(event) {Shiny.onInputChange('",
                            ns("eventPointClicked"),
                            "', [this.index]+'_'+ [this.series.name]);}"
                        ))
                    
                    cond <- c(data()$condition1, data()$condition2)
                    rv$tempplot$volcano <- diffAnaVolcanoplot_rCharts(
                        df,
                        threshold_logFC = as.numeric(
                            rv$widgets$hypothesisTest$th_logFC
                        ),
                        threshold_pVal = as.numeric(rv$widgets$anaDiff$th_pval),
                        conditions = cond,
                        clickFunction = clickFun,
                        pal = rv$colorsVolcanoplot
                    )
                })
                
                rv$tempplot$volcano
            })
            
        })
    })
}



#------------------------------------------------

library(shiny)
library(shinyBS)
ui <- fluidPage(
    mod_volcanoplot_ui("Title")
)
server <- function(input, output) {
    logpval <- mod_volcanoplot_server(id = "Title",
                                             pval_init = reactive({1}),
                                             fdr = reactive({3.8}))
    
    observeEvent(logpval(), {
        print(logpval())
    })
}

shinyApp(ui, server)
