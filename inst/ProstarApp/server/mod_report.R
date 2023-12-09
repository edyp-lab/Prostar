
#' @title xxx
#' @description xxx
#' @param id xxx
#' @param ll.obj xxx
#' 
#' @name mod_report
#' 
NULL



ll_descrStats <- list(
    "boxplot" = "boxplot",
    "densityplot" = "densityplot",
    "heatmap" = "heatmap",
    "CVDistr" = "CVDistr",
    "violinplot" = "violinplot",
    "corrMatrix" = "corrMatrix",
    "MV plots" = list(
        "MVPlot1" = "MVPlot1",
        "MVPlot2" = "MVPlot2",
        "MVPlot3" = "MVPlot3"
    ),
    "PCA plots" = list(
        "PCA_Ind" = "PCA_Ind",
        "PCA_Var" = "PCA_Var",
        "PCA_Eigen" = "PCA_Eigen"
    )
)



#' @export
#' @rdname mod_report
#' 
mod_report_ui <- function(id) {
    ns <- NS(id)
    
    tagList(
        shinyjs::useShinyjs(),
        tagList(
            format_DT_ui(ns("viewProstarVersions")),
            downloadButton(ns("downloadReport"),
                           "Build and download pdf report",
                           class = actionBtnClass),
            hr(),
            div(
                div(style = "display:inline-block; vertical-align: top;",
                    format_DT_ui(ns("viewProcessingData"))
                ),
                div(style = "display:inline-block; vertical-align: top;",
                    shinyBS::bsCollapse(
                        id = ns("collapseDataProcessingReport"), open = "",
                        shinyBS::bsCollapsePanel(
                            "Plots for data processing tools",
                            div(
                                div(
                                    style = "display:inline-block;
                                        vertical-align: top;",
                                    hidden(
                                        div(id = "treeFor_Original_protein",
                                        tagList(
                                            p(tags$b("Original protein")),
                                            shinyTree::shinyTree(
                                                ns("plotsFor_Original_protein"),
                                                theme = "proton",
                                                themeIcons = FALSE,
                                                themeDots = FALSE,
                                                checkbox = TRUE
                                            )
                                        )
                                    ))
                                ),
                                div(style = "display:inline-block; vertical-align: top;",
                                    hidden(div(
                                        id = ns("treeFor_Original_peptide"),
                                        tagList(
                                            p(tags$b("Original peptide")),
                                            shinyTree::shinyTree(
                                                ns("plotsFor_Original_peptide"),
                                                theme = "proton",
                                                themeIcons = FALSE,
                                                themeDots = FALSE,
                                                checkbox = TRUE
                                            )
                                        )
                                    ))
                                ),
                                div(
                                    style = "display:inline-block; vertical-align: top;",
                                    hidden(div(
                                        id = ns("treeFor_Filtered_protein"),
                                        tagList(
                                            p(tags$b("Filtered protein")),
                                            shinyTree::shinyTree(
                                                ns("plotsFor_Filtered_protein"),
                                                theme = "proton",
                                                themeIcons = FALSE,
                                                themeDots = FALSE,
                                                checkbox = TRUE
                                            )
                                        )
                                    ))
                                ),
                                div(style = "display:inline-block; vertical-align: top;",
                                    hidden(div(
                                        id = ns("treeFor_Filtered_peptide"),
                                        tagList(
                                            p(tags$b("Filtered peptide")),
                                            shinyTree::shinyTree(
                                                ns("plotsFor_Filtered_peptide"),
                                                theme = "proton",
                                                themeIcons = FALSE,
                                                themeDots = FALSE,
                                                checkbox = TRUE
                                            )
                                        )
                                    ))
                                ),
                                div(style = "display:inline-block; vertical-align: top;",
                                    hidden(div(id = ns("treeFor_Normalized_protein"),
                                        tagList(
                                            p(tags$b("Normalized protein")),
                                            shinyTree::shinyTree(
                                                ns("plotsFor_Normalized_protein"),
                                                theme = "proton",
                                                themeIcons = FALSE,
                                                themeDots = FALSE,
                                                checkbox = TRUE
                                            )
                                        )
                                    ))
                                ),
                                div(
                                    style = "display:inline-block; vertical-align: top;",
                                    hidden(div(id = "treeFor_Normalized_peptide",
                                        tagList(
                                            p(tags$b("Normalized peptide")),
                                            shinyTree::shinyTree(
                                                ns("plotsFor_Normalized_peptide"),
                                                theme = "proton",
                                                themeIcons = FALSE,
                                                themeDots = FALSE,
                                                checkbox = TRUE
                                            )
                                        )
                                    ))
                                ),
                                div(style = "display:inline-block; vertical-align: top;",
                                    hidden(div(id = ns("treeFor_Aggregated_protein"),
                                        tagList(
                                            p(tags$b("Aggregated protein")),
                                            shinyTree::shinyTree(ns("plotsFor_Aggregated_protein"),
                                                                 theme = "proton",
                                                                 themeIcons = FALSE,
                                                                 themeDots = FALSE,
                                                                 checkbox = TRUE
                                            )
                                        )
                                    ))
                                ),
                                div(style = "display:inline-block; vertical-align: top;",
                                    hidden(div(id = ns("treeFor_Aggregated_peptide"),
                                        tagList(
                                            p(tags$b("Aggregated peptide")),
                                            shinyTree::shinyTree(ns("plotsFor_Aggregated_peptide"),
                                                                 theme = "proton",
                                                                 themeIcons = FALSE,
                                                                 themeDots = FALSE,
                                                                 checkbox = TRUE
                                            )
                                        )
                                    ))
                                ),
                                div(style = "display:inline-block; vertical-align: top;",
                                    hidden(div(id = ns("treeFor_Imputed_protein"),
                                        tagList(
                                            p(tags$b("Imputed protein")),
                                            shinyTree::shinyTree(ns("plotsFor_Imputed_protein"),
                                                                 theme = "proton",
                                                                 themeIcons = FALSE,
                                                                 themeDots = FALSE,
                                                                 checkbox = TRUE
                                            )
                                        )
                                    ))
                                ),
                                div(style = "display:inline-block; vertical-align: top;",
                                    hidden(div(id = ns("treeFor_Imputed_peptide"),
                                        tagList(
                                            p(tags$b("Imputed peptide")),
                                            shinyTree::shinyTree(ns("plotsFor_Imputed_peptide"),
                                                                 theme = "proton",
                                                                 themeIcons = FALSE,
                                                                 themeDots = FALSE,
                                                                 checkbox = TRUE
                                            )
                                        )
                                    ))
                                ),
                                div(style = "display:inline-block; vertical-align: top;",
                                    hidden(div(id = ns("treeFor_HypothesisTest_protein"),
                                        tagList(
                                            p(tags$b("HypothesisTest protein")),
                                            shinyTree::shinyTree(ns("plotsFor_HypothesisTest_protein"),
                                                                 theme = "proton",
                                                                 themeIcons = FALSE,
                                                                 themeDots = FALSE,
                                                                 checkbox = TRUE
                                            )
                                        )
                                    ))
                                ),
                                div(style = "display:inline-block; vertical-align: top;",
                                    hidden(div(id = ns("treeFor_HypothesisTest_peptide"),
                                        tagList(
                                            p(tags$b("HypothesisTest peptide")),
                                            shinyTree::shinyTree(ns("plotsFor_HypothesisTest_peptide"),
                                                                 theme = "proton",
                                                                 themeIcons = FALSE,
                                                                 themeDots = FALSE,
                                                                 checkbox = TRUE
                                            )
                                        )
                                    ))
                                )
                            ),
                            style = "primary"
                        )
                    )
                )
            ),
            br(), br(), br(),
            div(
                div(
                    style = "display:inline-block; vertical-align: top;",
                    format_DT_ui("viewDataMining")
                ),
                div(
                    style = "display:inline-block; vertical-align: top;",
                    shinyBS::bsCollapse(id = ns("collapseDataProcessingReport2"),
                        open = "",
                        shinyBS::bsCollapsePanel("Plots for data mining tools",
                                                 tagList(),
                                                 style = "primary"
                        )
                    )
                )
            )
        )
    )
    
}



#' @export
#' @rdname mod_report
#' 
mod_report_server <- function(id, 
                              ll.obj = reactive({NULL})) {

    
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        
        
        BuildParamDataProcessingDT <- reactive({
            req(ll.obj())
            #req(input$datasets)
            #tmp.params <- rv$current.obj@experimentData@other$Params
            df <- data.frame(
                Dataset = names(ll.obj()),
                Process = rep("", length(names(ll.obj()))),
                Parameters = rep("", length(names(ll.obj()))),
                stringsAsFactors = FALSE
            )
            
            lapply(length(ll.obj()), function(x){
                p <- ll.obj()[[x]]
                tmp.params <- p@experimentData@other$Params
                
                df[x, "Process"] <- ifelse(is.null(names(tmp.params)), "-",
                                           names(tmp.params))
                
                if (length(tmp.params[[x]][[processName]]) == 0) {
                    df[iData, "Parameters"] <- "-"
                } else {
                    df[iData, "Parameters"] <- do.call(
                        paste0("getTextFor", processName),
                        list(l.params = tmp.params[[x]][[processName]])
                    )
                }
                
            })

            df
        })
        
        
        GetCurrentObj <- reactive({
            ll.obj()[input$datasets]
        })
        
        BuildParamDataMiningDT <- reactive({
            req(rv$current.obj)
            
            nbLines <- sum(
                (as.character(input$selectComparison) != "None"),
                !is.null(rv$params.GO)
            )
            if (nbLines == 0) {
                df <- NULL
            } else {
                df <- data.frame(
                    Dataset = rep(input$datasets, length(names(nbLines))),
                    Process = rep("", length(names(nbLines))),
                    Parameters = rep("", length(names(nbLines))),
                    stringsAsFactors = FALSE
                )
                
                if (!is.null(as.character(input$selectComparison))) {
                    df[1, "Dataset"] <- input$datasets
                    df[1, "Process"] <- "Differential analysis"
                    df[1, "Parameters"] <- getTextForAnaDiff(rv$widgets$anaDiff)
                } else {}
            }
            df
        })
        
        
        format_DT_server("viewProcessingData",
                         data = reactive({BuildParamDataProcessingDT()}),
                         filename = "processingData")
        
        format_DT_server("viewDataMining",
                         data = reactive({BuildParamDataMiningDT()}),
                         filename = "datamining_view")
        
        format_DT_server("viewProstarVersions",
                         data = reactive({as.data.frame(GetLocalVersions())}),
                         filename = "Prostar_Versions")
        
        
        
        GetLocalVersions <- function(){
            local.version <- list()
            local.version <- list(
                Prostar = installed.packages(lib.loc = Prostar.loc)["Prostar", "Version"],
                DAPAR = installed.packages(lib.loc = DAPAR.loc)["DAPAR", "Version"],
                DAPARdata = installed.packages(lib.loc = DAPARdata.loc)["DAPARdata", "Version"]
            )
            
            return(local.version)
        }
        
        
        output$plotsFor_Original_protein <- shinyTree::renderTree({
            list("Descr stats" = ll_descrStats)
        })
        
        output$plotsFor_Original_peptide <- shinyTree::renderTree({
            list("Descr stats" = ll_descrStats)
        })
        
        output$plotsFor_Filtered_protein <- shinyTree::renderTree({
            list("Descr stats" = ll_descrStats)
        })
        
        output$plotsFor_Filtered_peptide <- shinyTree::renderTree({
            list("Descr stats" = ll_descrStats)
        })
        
        output$plotsFor_Normalized_protein <- shinyTree::renderTree({
            list("Descr stats" = ll_descrStats, "compNorm" = "compNorm")
        })
        
        output$plotsFor_Normalized_peptide <- shinyTree::renderTree({
            list("Descr stats" = ll_descrStats, "compNorm" = "compNorm")
        })
        
        output$plotsFor_Imputed_protein <- shinyTree::renderTree({
            list("Descr stats" = ll_descrStats)
        })
        
        output$plotsFor_Imputed_peptide <- shinyTree::renderTree({
            list("Descr stats" = ll_descrStats)
        })
        
        output$plotsFor_HypothesisTest_protein <- shinyTree::renderTree({
            list("Descr stats" = ll_descrStats, "logFCDistr" = "logFCDistr")
        })
        
        output$plotsFor_HypothesisTest_peptide <- shinyTree::renderTree({
            list("Descr stats" = ll_descrStats, "logFCDistr" = "logFCDistr")
        })
        
        output$plotsFor_Aggregated_protein <- shinyTree::renderTree({
            list("Descr stats" = ll_descrStats)
        })
        output$plotsFor_Aggregated_peptide <- shinyTree::renderTree({
            list("Descr stats" = ll_descrStats)
        })
        
        
        
        
        shinyTree_GetSelected <- reactive({
            tmp <- unlist(
                shinyTree::get_selected(input$plotsFor_Original_peptide,
                                        format = "names"
                )
            )
            tmp
        })
        
        
        
        
        output$choosedataToExportMSnset <- renderUI({
            req(ll.obj())
            
            dnames <- unlist(lapply(
                names(ll.obj()),
                function(x) {
                    unlist(strsplit(x, " - "))[[1]]
                }
            ))
            .choices <- names(ll.obj())
            names(.choices) <- dnames
            radioButtons("chooseDatasetToExportToMSnset",
                         "Choose which dataset to export",
                         choices = c("None" = "None", .choices)
            )
        })
        
        
        
        observeEvent(ll.obj(), {
            for (i in 1:length(names(ll.obj()))) {
                txt <- paste0("treeFor.", names(ll.obj())[i])
                txt <- tags$b(".", "_", txt, fixed = TRUE)
                shinyjs::toggle(txt)
            }
        })
        
        
        
        
        
        GetDatasetShortNames <- reactive({
            req(ll.obj())
            
            dnames <- unlist(lapply(
                names(ll.obj()),
                function(x) {
                    unlist(strsplit(x, ".", fixed = TRUE))[[1]]
                }
            ))
            dnames
        })
        
        
        
        output$exportOptions <- renderUI({
            req(input$chooseDatasetToExportToMSnset != "None")
            
            tagList(
                fluidRow(
                    column(
                        width = 2,
                        popover_for_help_ui("modulePopover_exportFileFormat")
                    ),
                    column(
                        width = 10,
                        selectInput("fileformatExport", "", choices = gFileFormatExport)
                    )
                ),
                br(),
                fluidRow(
                    column(width = 2, popover_for_help_ui("modulePopover_exportFilename")),
                    column(width = 10, uiOutput("chooseExportFilename"))
                ),
                br(),
                downloadButton("downloadMSnSet", "Download")
            )
        })
        
        
        popover_for_help_server("modulePopover_exportMetaData",
                                title = "Metadata",
                                content = "Select the columns you want to keep as metadata. By default,
    if any column is specified, all metadata in your dataset
    will be exported."
        )
        
        
        popover_for_help_server("modulePopover_exportFileFormat",
                                title = "File format",
                                content = "File format"
        )
        
        
        
        
        popover_for_help_server("modulePopover_exportFilename",
                                title = "Filename",
                                content = "Enter the name of the files to be created"
        )
        
        
        
        
        
        output$chooseExportFilename <- renderUI({
            textInput("nameExport", label = "", value = rv$current.obj.name)
        })
        
        
        
        
        output$downloadMSnSet <- downloadHandler(
            filename = function() {
                if (input$fileformatExport == gFileFormatExport$excel) {
                    paste(input$nameExport, gFileExtension$excel, sep = "")
                } else if (input$fileformatExport == gFileFormatExport$msnset) {
                    paste(input$nameExport, gFileExtension$msnset, sep = "")
                } else if (input$fileformatExport == gFileFormatExport$zip) {
                    paste(input$nameExport, gFileExtension$zip, sep = "")
                }
            },
            content = function(file) {
                withProgress(
                    message = "Export process",
                    detail = "Initialisation",
                    value = 0,
                    {
                        incProgress(0.3, detail = "Preparing dataset")
                        .data <- input$chooseDatasetToExportToMSnset
                        dataToExport <- ll.obj()[[.data]]
                        colnames(Biobase::fData(dataToExport)) <- gsutags$b(".", "_",
                                                                       colnames(Biobase::fData(dataToExport)),
                                                                       fixed = TRUE
                        )
                        names(dataToExport@experimentData@other) <- gsutags$b(".", "_",
                                                                         names(dataToExport@experimentData@other),
                                                                         fixed = TRUE
                        )
                        
                        incProgress(0.5, detail = "Recording versions")
                        dataToExport@experimentData@other$Prostar_Version <-
                            installed.packages(lib.loc = Prostar.loc)["Prostar", "Version"]
                        dataToExport@experimentData@other$DAPAR_Version <-
                            installed.packages(lib.loc = DAPAR.loc)["DAPAR", "Version"]
                        
                        .protId <- (ll.obj()[[.data]])@experimentData@other$proteinId
                        if (is.null(.protId)) {
                            dataToExport@experimentData@other$proteinId <- gsutags$b(".", "_",
                                                                                rv$proteinId,
                                                                                fixed = TRUE
                            )
                        } else {
                            dataToExport@experimentData@other$proteinId <- .protId
                        }
                        
                        
                        if (rv$typeOfDataset == "peptide") {
                            if (is.null(DAPAR::GetMatAdj(dataToExport))) {
                                ## Export adjacency matrices
                                incProgress(0.7,
                                            detail = "Exporting adjacency matrices"
                                )
                                dataToExport <- SetMatAdj(
                                    dataToExport,
                                    ComputeAdjacencyMatrices()
                                )
                            }
                            
                            if (is.null(GetCC(dataToExport))) {
                                ## Export CC
                                incProgress(0.7,
                                            detail = "Exporting connected components"
                                )
                                dataToExport <- SetCC(
                                    dataToExport,
                                    ComputeConnectedComposants()
                                )
                            }
                        }
                        
                        
                        
                        if (input$fileformatExport == gFileFormatExport$excel) {
                            incProgress(0.9, detail = "Save dataset as Excel file")
                            fname <- paste(input$nameExport,
                                           gFileExtension$excel,
                                           sep = ""
                            )
                            writeMSnsetToExcel(dataToExport, input$nameExport)
                            file.copy(fname, file)
                            file.remove(fname)
                        } else if (input$fileformatExport == gFileFormatExport$msnset) {
                            incProgress(0.9, detail = "Save dataset as MSnSet file")
                            fname <- paste(input$nameExport,
                                           gFileExtension$msnset,
                                           sep = ""
                            )
                            saveRDS(dataToExport, file = fname)
                            file.copy(fname, file)
                            file.remove(fname)
                        } else if (input$fileformatExport == gFileFormatExport$zip) {
                            incProgress(0.9, detail = "Save dataset as csv file")
                            fname <- paste(input$nameExport,
                                           gFileExtension$zip,
                                           sep = ""
                            )
                            writeMSnsetToCSV(dataToExport, fname)
                            file.copy(fname, file)
                            file.remove(fname)
                        }
                    }
                )
            }
        )
        
        
        
        
        
        output$choosedataTobuildReport <- renderUI({
            req(ll.obj())

            
            checkboxGroupInput("chooseDatasetToExport",
                               "Datasets to export",
                               choices = names(ll.obj()),
                               selected = names(ll.obj())
            )
        })
        
        
        ###### -----------------------------------------------------------------
        output$downloadReport <- downloadHandler(
            input$reportFilename,
            filename = function() {
                paste0(input$reportFilename, sep = ".pdf")
            },
            content = function(file) {
                toto()
                filename <- rv$outfile
                require(rmarkdown)
                out <- rmarkdown::render(rv$outfile,
                                         output_file = file,
                                         pdf_document()
                ) # END render
            }
        )
        
       
        

        
        
    })
}


# Example
#
ui <- fluidPage(
    tagList(
        mod_report_ui('tree'),
        uiOutput('res')
    )
)

server <- function(input, output) {
    
    utils::data('Exp1_R25_pept', package='DAPARdata')
    utils::data('Exp1_R25_prot', package='DAPARdata')
    ll.obj <- list(first = Exp1_R25_pept,
                   second = Exp1_R25_pept,
                   third = Exp1_R25_pept)
    tags <- mod_report_server('tree', ll.obj = reactive({ll.obj}))
    
}

shinyApp(ui = ui, server = server)

