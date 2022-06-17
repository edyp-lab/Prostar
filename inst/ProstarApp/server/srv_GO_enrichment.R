
callModule(moduleProcess, "moduleProcess_GO",
    isDone = reactive({
        rvModProcess$moduleGODone
    }),
    pages = reactive({
        rvModProcess$moduleGO
    }),
    rstFunc = resetModuleGO,
    forceReset = reactive({
        rvModProcess$moduleGOForceReset
    })
)



resetModuleGO <- reactive({
    ## update rv$widgets values (reactive values)
    resetModuleProcess("GO")

    rv$widgets$go$sourceOfProtID <- NULL
    rv$widgets$go$idFrom <- "UNIPROT"
    rv$widgets$go$Organism <- character(0)
    rv$widgets$go$Ontology <- character(0)
    rv$widgets$go$UniprotIDCol <- character(0)
    rv$widgets$go$UNIPROTID_File <- NULL
    rv$widgets$go$GO_level <- 2
    rv$widgets$go$universe <- NULL
    rv$widgets$go$UniverseFile <- NULL
    rv$widgets$go$pvalueCutoff <- 0.01

    rv$widgets$go$ProtIDList <- NULL
    rv$widgets$go$gene <- NULL
    rv$widgets$go$proteinsNotMapped <- NULL
    rv$widgets$go$ratio <- NULL
    rv$widgets$go$uniprotID <- NULL
    rv$widgets$go$universeData <- NULL
    rv$widgets$go$enrichGO_data <- NULL
    rv$widgets$go$groupGO_data <- NULL


    rvModProcess$moduleGODone <- rep(FALSE, 4)
})

## --------------------------------------------------------
## ---------------------------------------------------------

observeEvent(input$sourceOfProtID, {
    rv$widgets$go$sourceOfProtID <- input$sourceOfProtID
})
observeEvent(input$idFrom, {
    rv$widgets$go$idFrom <- input$idFrom
})
observeEvent(input$Organism, {
    rv$widgets$go$Organism <- input$Organism
})
observeEvent(input$Ontology, {
    rv$widgets$go$Ontology <- input$Ontology
})
observeEvent(input$UniprotIDCol, {
    rv$widgets$go$UniprotIDCol <- input$UniprotIDCol
})
observeEvent(input$UNIPROTID_File, {
    rv$widgets$go$UNIPROTID_File <- input$UNIPROTID_File
})
observeEvent(input$GO_level, {
    rv$widgets$go$GO_level <- input$GO_level
})
observeEvent(input$universe, {
    rv$widgets$go$universe <- input$universe
})
observeEvent(input$UniverseFile, {
    rv$widgets$go$UniverseFile <- input$UniverseFile
})
observeEvent(input$pvalueCutoff, {
    rv$widgets$go$pvalueCutoff <- input$pvalueCutoff
})

observeEvent(rvModProcess$moduleGoForceReset, {
    rv$widgets$go$UNIPROTID_File <- NULL
    rv$widgets$go$UniverseFile <- NULL
})


output$resettableUniprotidFile <- renderUI({
    rv$widgets$go$UNIPROTID_File
    tagList(
        p("Select file containing protein IDs"),
        fileInput("uniprotFile", rv$widgets$go$UNIPROTID_File$name)
    )
})

output$resettableUniverseFile <- renderUI({
    req(c(rv$widgets$go$UniverseFile, rv$widgets$go$universe))
    if (rv$widgets$go$universe == "Custom") {
        fileInput("UniverseFile", rv$widgets$go$UniverseFile$name,
            multiple = FALSE
        )
    }
})


output$screenGO1 <- renderUI({
    print("output$screenGO1 <- renderUI")
    tagList(
        tags$div(
            tags$div(
                style = "display:inline-block; vertical-align: middle;
        padding-right: 20px;",
                radioButtons("sourceOfProtID", "Source of protein ID",
                    choices = G_sourceOfProtID_Choices,
                    selected = rv$widgets$go$sourceOfProtID_Choices
                )
            ),
            tags$div(
                style = "display:inline-block; vertical-align: middle;
        padding-right: 20px;",
                uiOutput("chooseSourceForProtID")
            ),
            tags$div(
                style = "display:inline-block; vertical-align: middle;
        padding-right: 20px;",
                selectInput("idFrom", "Id From",
                    choices = c("UNIPROT", "ENTREZID"),
                    selected = rv$widgets$go$idFrom
                ), width = "200px"
            ),
            tags$div(
                style = "display:inline-block; vertical-align: middle;
        padding-right: 20px;",
                modulePopoverUI("modulePopover_GenomeWide"),
                selectInput("Organism", NULL,
                    choices = GetListInstalledOrgdDB(),
                    selected = rv$widgets$go$Organism
                ), width = "200px"
            ),
            tags$div(
                style = "display:inline-block; vertical-align: middle;
        padding-right: 20px;",
                selectInput("Ontology", "Ontology",
                    choices = G_ontology_Choices,
                    selected = rv$widgets$go$Ontology
                ), width = "150px"
            )
        ),
        actionButton("mapProtein.GO.button", "Map proteins IDs",
            class = actionBtnClass
        ),
        uiOutput("warnDifferentSizeID"),
        tags$hr(),
        uiOutput("infoIDProt_NA"),
        br(), br(),
        uiOutput("GeneMappedRatio"),
        br(), br(),
        if (nrow(pData(rv$current.obj)) > 153) {
            p("The size of the table is too big to be exported with the buttons
      below (only the first 154 rows will be exported). It is advised to use
      the Export tool of Prostar.")
        },
        DT::dataTableOutput("nonIdentifiedProteins", width = "80%")
    )
})




output$screenGO2 <- renderUI({
    tagList(
        tags$div(
            tags$div(
                style = "display:inline-block; vertical-align: middle;",
                modulePopoverUI("modulePopover_GOlevel"),
                checkboxGroupInput("GO_level", NULL,
                    choices = c(2:4),
                    selected = rv$widgets$go$GO_level
                )
            ),
            tags$div(
                style = "display:inline-block; vertical-align: middle;
      padding-right: 20px;",
                actionButton("group.GO.perform.button", "Perform GO grouping",
                    class = actionBtnClass
                )
            )
        ),
        tags$hr(),
        withProgress(message = "Building plot", detail = "", value = 0, {
            incProgress(1 / 3, detail = "Goup level 2")
            highchartOutput("GOplotGroup_level2", width = "80%")
            incProgress(2 / 3, detail = "Goup level 3")
            highchartOutput("GOplotGroup_level3", width = "80%")
            incProgress(3 / 3, detail = "Goup level 4")
            highchartOutput("GOplotGroup_level4", width = "80%")
        })
    )
})



output$screenGO3 <- renderUI({
    tagList(
        tags$div(
            tags$div(
                style = "display:inline-block; vertical-align: middle;
        padding-right: 20px;",
                modulePopoverUI("modulePopover_GOuniverse"),
                radioButtons("universe", NULL,
                    choices = G_universe_Choices,
                    selected = rv$widgets$go$universe
                )
            ),
            tags$div(
                style = "display:inline-block; vertical-align: middle;
        padding-right: 20px;",
                uiOutput("resettableUniverseFile")
            ),
            tags$div(
                style = "display:inline-block; vertical-align: middle;
    padding-right: 20px;",
                modulePopoverUI("modulePopover_GOfdr"),
                numericInput("pvalueCutoff", NULL,
                    min = 0, max = 1, step = 0.01,
                    value = rv$widgets$go$pvalueCutoff, width = "100px"
                )
            )
        ),
        actionButton("perform.GO.button", "Perform enrichment analysis",
            class = actionBtnClass
        ),
        tags$hr(),
        withProgress(message = "Building plot", detail = "", value = 0, {
            incProgress(1 / 3, detail = "Bar plot")
            highchartOutput("GObarplotEnrich", width = "80%")
            incProgress(1 / 3, detail = "Dotplot")
            highchartOutput("GOdotplotEnrich", width = "80%")
        })
    )
})


output$screenGO4 <- renderUI({
    DT::dataTableOutput("GO_resumeParams")
})




callModule(modulePopover, "modulePopover_GOlevel",
    data = reactive(list(
        title = "Level",
        content = "Level"
    ))
)


callModule(modulePopover, "modulePopover_GOuniverse",
    data = reactive(list(
        title = "Universe",
        content = "universe"
    ))
)
callModule(modulePopover, "modulePopover_GOfdr",
    data = reactive(list(
        title = "FDR",
        content = "BH Adjusted P-value cutoff"
    ))
)



callModule(modulePopover, "modulePopover_GenomeWide",
    data = reactive(list(
        title = "Genome Wide Annotation",
        content = paste0(
            tags$p("If the expected annotation database is not proposed in the
        dropdown menu, please find "),
            tags$a("here",
                href = "http://bioconductor.org/packages/release/BiocViews.html#___OrgDb",
                target = "_blank"
            ),
            tags$p(" the corresponding package. Then, install it (or have it
        installed by the administrator of the ProStaR server) and restart
        ProStaR.")
        )
    ))
)



GetListInstalledOrgdDB <- function() {
    l <- installed.packages()[, "Package"]
    l <- l[grep("^org.", l)]
    res <- list_org_db[l, ]$longName
    names(l) <- res

    names(l)[which(is.na(names(l)))] <- l[which(is.na(names(l)))]

    return(l)
}


GetDataIndexForAnalysis <- reactive({
    req(rv$current.obj)

    index <- NULL
    if ("Significant" %in% names(fData(rv$current.obj))) {
        index <- which(fData(rv$current.obj)$Significant == TRUE)
    } else {
        index <- seq(1:nrow(rv$current.obj))
    }

    return(index)
})



output$chooseSourceForProtID <- renderUI({
    req(rv$current.obj)
    req(rv$widgets$go$sourceOfProtID)

    if (rv$widgets$go$sourceOfProtID == "colInDataset") {
        selectInput("UniprotIDCol", "Protein IDs",
            choices = c("", colnames(fData(rv$current.obj))),
            selected = rv$widgets$go$UniprotIDCol
        )
    } else if (rv$widgets$go$sourceOfProtID == "extFile") {
        uiOutput("resettableUniprotidFile")
    }
})








observeEvent(req(rv$widgets$go$UniprotIDCol), ignoreInit = TRUE, {
    if ((rv$widgets$go$UniprotIDCol == "")) {
        rv$widgets$go$ProtIDList <- return(NULL)
    } else {
        rv$widgets$go$ProtIDList <-
            fData(rv$current.obj)[, rv$widgets$go$UniprotIDCol]
    }
})


observeEvent(rv$widgets$go$UNIPROTID_File, ignoreInit = TRUE, {
    rv$widgets$go$ProtIDList <- read.table(
        rv$widgets$go$UNIPROTID_File$datapath,
        header = FALSE,
        stringsAsFactors = FALSE
    )$V1
})


output$warnDifferentSizeID <- renderUI({
    req(rv$widgets$go$ProtIDList)
    if (length(rv$widgets$go$ProtIDList) != nrow(rv$current.obj)) {
        h4("Warning : the protein ID list has not the same number of
          entities as the dataset.")
        br()
        h4("Please select another list of ID")
    }
})


observeEvent(input$mapProtein.GO.button, ignoreInit = TRUE, {
    print("IN MAP button")
    req(rv$widgets$go$UniprotIDCol)
    rv$widgets$go$Organism
    rv$widgets$go$idFrom

    if (rv$widgets$go$UniprotIDCol == "") {
        print("toto")
        rv$widgets$go$ProtIDList <- NULL
        return(NULL)
    }

    require(clusterProfiler)
    isolate({
        rv$widgets$go$gene <- NULL
        rv$widgets$go$ProtIDList <-
            fData(rv$current.obj)[, rv$widgets$go$UniprotIDCol]
        index <- GetDataIndexForAnalysis()

        tryCatch(
            {
                rv$widgets$go$gene <- bitr(rv$widgets$go$ProtIDList[index],
                    fromType = rv$widgets$go$idFrom, toType = "ENTREZID",
                    OrgDb = rv$widgets$go$Organism
                )
                rv$widgets$go$proteinsNotMapped <- which((rv$widgets$go$ProtIDList[index] %in% rv$widgets$go$gene[, rv$widgets$go$idFrom]) == FALSE)
                rv$widgets$go$ratio <- 100 * length(rv$widgets$go$proteinsNotMapped) / length(index)
                rvModProcess$moduleGODone[1] <- TRUE
            },
            warning = function(w) {
                rv$widgets$go$gene <- bitr(rv$widgets$go$ProtIDList[index],
                    fromType = rv$widgets$go$idFrom, toType = "ENTREZID", OrgDb = rv$widgets$go$Organism
                )
                rv$widgets$go$proteinsNotMapped <- which((rv$widgets$go$ProtIDList[index] %in% rv$widgets$go$gene[, rv$widgets$go$idFrom]) == FALSE)
                rv$widgets$go$ratio <- 100 * length(rv$widgets$go$proteinsNotMapped) / length(index)
                rvModProcess$moduleGODone[1] <- TRUE
            },
            error = function(e) {
                rv$widgets$go$ratio <- 100
            },
            finally = {
            }
        )
    })
})


##' Reactive behavior : GO analysis of data
##' @author Samuel Wieczorek
observeEvent(input$perform.GO.button, ignoreInit = TRUE, {
    rv$widgets$go$universe
    rv$widgets$go$Organism
    rv$widgets$go$Ontology
    rv$widgets$go$pvalueCutoff
    req(rv$widgets$go$ProtIDList)
    rv$widgets$go$idFrom
    rv$widgets$go$uniprotID
    # req(rv$widgets$go$perform.GO.button)
    req(rv$widgets$go$ratio)

    print(rv$widgets$go$ratio)
    if (rv$widgets$go$ratio == 100) {
        return(NULL)
    }

    require(clusterProfiler)

    withProgress(message = "", detail = "", value = 0, {
        incProgress(0.2, detail = "Get universe data")

        if (rv$widgets$go$universe == "Entire dataset") {
            rv$widgets$go$universeData <- rv$widgets$go$ProtIDList
        } else if (rv$widgets$go$universe == "Entire organism") {
            rv$widgets$go$universeData <- DAPAR::univ_AnnotDbPkg(rv$widgets$go$Organism)
        } else {
            rv$widgets$go$universeData <- read.table(rv$widgets$go$UniverseFile$datapath,
                header = FALSE, stringsAsFactors = FALSE
            )
        }

        incProgress(0.4, detail = "Get data to analyze")
        index <- GetDataIndexForAnalysis()
        incProgress(1, detail = "Computing enrichment")
        rv$widgets$go$enrichGO_data <- enrich_GO(rv$widgets$go$ProtIDList[index],
            idFrom = rv$widgets$go$idFrom,
            orgdb = rv$widgets$go$Organism,
            ont = rv$widgets$go$Ontology,
            pval = rv$widgets$go$pvalueCutoff,
            universe = rv$widgets$go$universeData
        )
    })
    rvModProcess$moduleGODone[3] <- TRUE
})





observeEvent(input$group.GO.perform.button, ignoreInit = TRUE, {
    rv$widgets$go$Organism
    rv$widgets$go$Ontology
    req(rv$widgets$go$ProtIDList)
    rv$widgets$go$uniprotID
    rv$widgets$go$idFrom
    rv$widgets$go$GO_level
    req(rv$widgets$go$ratio)

    if (rv$widgets$go$ratio == 100) {
        return(NULL)
    }
    levelIndex <- sort(rv$widgets$go$GO_level)

    withProgress(message = "", detail = "", value = 0, {
        incProgress(1 / (1 + length(levelIndex)), detail = "Get data for analysis")

        index <- GetDataIndexForAnalysis()
        rv$widgets$go$groupGO_data <- list()
        for (i in 1:length(levelIndex)) {
            incProgress(1 / (1 + i), detail = paste0("Building plot for level ", i))
            rv$widgets$go$groupGO_data[[i]] <- list(
                level = as.numeric(levelIndex[i]),
                ggo_res = group_GO(rv$widgets$go$ProtIDList[index],
                    idFrom = rv$widgets$go$idFrom,
                    orgdb = rv$widgets$go$Organism,
                    ont = rv$widgets$go$Ontology,
                    level = as.numeric(levelIndex[i])
                )
            )
        }
    })
    rvModProcess$moduleGODone[2] <- TRUE
})


##########################################
GOplotGroup_level2 <- reactive({
    req(rv$widgets$go$groupGO_data)

    isolate({
        if (length(rv$widgets$go$groupGO_data) >= 1) {
            barplotGroupGO_HC(rv$widgets$go$groupGO_data[[1]]$ggo_res,
                title = paste("Groups at level ",
                    rv$widgets$go$groupGO_data[[1]]$level,
                    sep = ""
                )
            )
        }
    })
})

##########################################
output$GOplotGroup_level2 <- renderHighchart({
    GOplotGroup_level2()
})


##########################################
GOplotGroup_level3 <- reactive({
    req(rv$widgets$go$groupGO_data)

    if ((length(rv$widgets$go$groupGO_data) < 2)) {
        return(NULL)
    }
    isolate({
        barplotGroupGO_HC(rv$widgets$go$groupGO_data[[2]]$ggo_res,
            title = paste(
                "Groups at level ",
                rv$widgets$go$groupGO_data[[2]]$level
            )
        )
    })
})


output$GOplotGroup_level3 <- renderHighchart({
    GOplotGroup_level3()
})

GOplotGroup_level4 <- reactive({
    req(rv$widgets$go$groupGO_data)

    if ((length(rv$widgets$go$groupGO_data) != 3)) {
        return(NULL)
    }
    isolate({
        barplotGroupGO_HC(rv$widgets$go$groupGO_data[[3]]$ggo_res,
            title = paste(
                "Groups at level ",
                rv$widgets$go$groupGO_data[[3]]$level
            )
        )
    })
})


output$GOplotGroup_level4 <- renderHighchart({
    GOplotGroup_level4()
})

GObarplotEnrich <- reactive({
    req(rv$widgets$go$enrichGO_data)
    barplotEnrichGO_HC(rv$widgets$go$enrichGO_data)
})

output$GObarplotEnrich <- renderHighchart({
    GObarplotEnrich()
})

GOdotplotEnrich <- reactive({
    req(rv$widgets$go$enrichGO_data)

    scatterplotEnrichGO_HC(rv$widgets$go$enrichGO_data)
})

output$GOdotplotEnrich <- renderHighchart({
    GOdotplotEnrich()
})


output$GODatatable <- renderDataTable(server = TRUE, {
    req(rv$widgets$go$enrichGO_data)
    req(rv$widgets$go$groupGO_data)


    dt <- DT::datatable(as.data.frame(rv$widgets$go$groupGO_data@result),
        extensions = c("Scroller"),
        options = list(
            dom = "frtip",
            initComplete = initComplete(),
            displayLength = 20,
            deferRender = TRUE,
            bLengthChange = FALSE,
            scrollX = 400,
            scrollY = 600,
            scroller = TRUE,
            ordering = FALSE,
            server = TRUE
        )
    )

    dt
})


output$GeneMappedRatio <- renderUI({
    req(rv$widgets$go$ProtIDList)
    req(rv$current.obj)
    req(rv$widgets$go$gene)
    rv$widgets$go$idFrom

    req(rv$widgets$go$ratio)

    index <- GetDataIndexForAnalysis()
    rv$widgets$go$proteinsNotMapped <- which((rv$widgets$go$ProtIDList[index] %in% rv$widgets$go$gene[, rv$widgets$go$idFrom]) == FALSE)
    nProtMapped <- length(rv$widgets$go$proteinsNotMapped)
    nProtTotal <- length(index)

    tagList(
        h5(paste(round(rv$widgets$go$ratio, digits = 2),
            " % of the proteins have not been mapped (",
            nProtMapped, " / ", nProtTotal, ").",
            sep = ""
        )),
        helpText("These proteins are listed in the table below."),
        if (rv$widgets$go$ratio == 100) {
            h3(paste("Tip: You should check the organism which has
              been selected.", sep = ""))
        }
    )
})



output$Warning_nonIdentifiedProteins <- renderUI({
    GetDataFor_nonIdentifiedProteins()
    if (nrow(GetDataFor_nonIdentifiedProteins()) > 153) {
        p(MSG_WARNING_SIZE_DT)
    }
})


GetDataFor_nonIdentifiedProteins <- reactive({
    req(rv$widgets$go$ProtIDList)
    req(rv$current.obj)
    req(rv$widgets$go$gene)
    rv$widgets$go$idFrom

    index <- GetDataIndexForAnalysis()
    rv$widgets$go$proteinsNotMapped <- which((rv$widgets$go$ProtIDList[index] %in% rv$widgets$go$gene[, rv$widgets$go$idFrom]) == FALSE)
    data <- as.data.frame(fData(rv$current.obj)[index[rv$widgets$go$proteinsNotMapped], ])

    data
})

output$nonIdentifiedProteins <- renderDataTable(server = TRUE, {
    req(rv$widgets$go$ProtIDList)
    req(rv$current.obj)
    req(rv$widgets$go$gene)
    rv$widgets$go$idFrom

    i
    data <- GetDataFor_nonIdentifiedProteins()
    if (nrow(data) != 0) {
        dt <- DT::datatable(data,
            extensions = c("Scroller"),
            options = list(
                dom = "frtip",
                initComplete = initComplete(),
                displayLength = 20,
                deferRender = TRUE,
                bLengthChange = FALSE,
                scrollX = 400,
                scrollY = 600,
                scroller = TRUE,
                ordering = FALSE,
                server = TRUE
            )
        )

        dt
    }
})





output$GO_resumeParams <- DT::renderDataTable(server = TRUE, {
    req(c(
        rv$widgets$go$sourceOfProtID,
        rv$widgets$go$idFrom,
        rv$widgets$go$Organism,
        rv$widgets$go$Ontology
    ))

    if (length(rv$widgets$go$sourceOfProtID) == 0) {
        return(NULL)
    }


    rvModProcess$moduleGODone[4] <- TRUE

    l.params <- data.frame(
        param = "sourceOfProtID",
        value = rv$widgets$go$sourceOfProtID
    )
    l.params <- rbind(l.params, data.frame(
        param = "idFrom",
        value = as.character(rv$widgets$go$idFrom)
    ))
    l.params <- rbind(l.params, data.frame(
        param = "Organism",
        value = rv$widgets$go$Organism
    ))
    l.params <- rbind(l.params, data.frame(
        param = "Ontology",
        value = rv$widgets$go$Ontology
    ))

    DT::datatable(l.params,
        escape = FALSE,
        rownames = FALSE,
        extensions = c("Scroller"),
        options = list(
            initComplete = initComplete(),
            dom = "rt",
            columnDefs = list(list(width = "200px", targets = "_all")),
            ordering = FALSE
        )
    )
})
