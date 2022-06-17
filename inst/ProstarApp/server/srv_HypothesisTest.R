

callModule(moduleProcess, "moduleProcess_HypothesisTest",
    isDone = reactive({
        rvModProcess$moduleHypothesisTestDone
    }),
    pages = reactive({
        rvModProcess$moduleHypothesisTest
    }),
    rstFunc = resetModuleHypothesisTest,
    forceReset = reactive({
        rvModProcess$moduleHypothesisTestForceReset
    })
)


rv.ht <- reactiveValues(
    n = NULL,
    swap.history = NULL
)

resetModuleHypothesisTest <- reactive({

    ## update widgets values (reactive values)
    resetModuleProcess("HypothesisTest")

    rv$widgets$hypothesisTest$design <- "None"
    rv$widgets$hypothesisTest$method <- "None"
    rv$widgets$hypothesisTest$ttest_options <- "Student"
    rv$widgets$hypothesisTest$th_logFC <- 0
    rv$widgets$hypothesisTest$listNomsComparaison <- NULL


    rv$res_AllPairwiseComparisons <- NULL
    rv$tempplot$logFCDistr <- NULL
    rvModProcess$moduleHypothesisTestDone <- rep(FALSE, 2)

    # rv.ht$swap.history <- NULL

    # Get back to previous dataset
    if (length(grep("HypothesisTest.", names(rv$dataset))) > 0) {
        i <- grep("HypothesisTest.", names(rv$dataset))
        rv$dataset <- rv$dataset[1:(i - 1)]
        updateSelectInput(session,
            "datasets",
            choices = names(rv$dataset),
            selected = names(rv$dataset)[i - 1]
        )
    }

    rv$current.obj <- rv$dataset[[input$datasets]]
})


callModule(
    module_Not_a_numeric, "test_seuillogFC",
    reactive({
        rv$widgets$hypothesisTest$th_logFC
    })
)


observeEvent(input$diffAnaMethod, {
    rv$widgets$hypothesisTest$method <- input$diffAnaMethod
})



observeEvent(input$PerformLogFCPlot, {
    isolate({
        rv$widgets$hypothesisTest$design <- input$anaDiff_Design
        rv$widgets$hypothesisTest$th_logFC <- as.numeric(input$seuilLogFC)
        rv$widgets$hypothesisTest$ttest_options <- input$ttest_options

        rv$res_AllPairwiseComparisons <- ComputeComparisons()
        rv.ht$n <- ncol(rv$res_AllPairwiseComparisons$logFC)
        rv.ht$swap.history <- rep(0, rv.ht$n)
    })
    rvModProcess$moduleHypothesisTestDone[1] <- TRUE
})



output$headerInputGroup <- renderUI({
    req(rv$res_AllPairwiseComparisons)
    uiOutput("showConds")
})

output$showConds <- renderUI({
    req(rv$res_AllPairwiseComparisons)
    .style <- "align: center;
  display:inline-block;
  vertical-align: middle;
  padding-right: 50px;
  padding-bottom: 50px;"


    input_list <- lapply(seq_len(rv.ht$n), function(i) {
        ll.conds <- unlist(
            strsplit(
                colnames(rv$res_AllPairwiseComparisons$logFC)[i],
                split = "_"
            )
        )

        div(
            div(
                style = .style,
                p(gsub("[()]", "", ll.conds[1]))
            ),
            div(
                style = .style,
                p(gsub("[()]", "", ll.conds[3]))
            ),
            div(
                style = .style,
                actionButton(paste0("compswap", i), "",
                    icon("sync", lib = "font-awesome"),
                    style = "border-width: 0px;
                        padding: 0px",
                    width = "30px",
                    height = "30px",
                    class = actionBtnClass
                )
            )
        )
    })
    do.call(tagList, input_list)
})


observeEvent(req(sum(GetSwapShinyValue()) > 0), {
    req(rv$res_AllPairwiseComparisons)
    swap <- GetSwapShinyValue()

    isolate({
        ind.swap <- which(swap != rv.ht$swap.history)
        rv.ht$swap.history <- swap
        if (length(ind.swap) > 0) {
            for (i in ind.swap) {
                current.comp <- colnames(rv$res_AllPairwiseComparisons$logFC)[i]

                # Swap comparisons names
                ll <- unlist(strsplit(current.comp, split = "_"))
                tmp.cond1 <- gsub("[( )]", "", ll[1])
                tmp.cond2 <- gsub("[( )]", "", ll[3])
                tmp.logFC <- paste0(
                    "(",
                    tmp.cond2,
                    ")_vs_(",
                    tmp.cond1,
                    ")_logFC"
                )
                tmp.pval <- paste0(
                    "(",
                    tmp.cond2,
                    ")_vs_(",
                    tmp.cond1,
                    ")_pval"
                )
                colnames(rv$res_AllPairwiseComparisons$logFC)[i] <- tmp.logFC
                colnames(rv$res_AllPairwiseComparisons$P_Value)[i] <- tmp.pval

                # Swap logFC values
                .logFC <- rv$res_AllPairwiseComparisons$logFC
                rv$res_AllPairwiseComparisons$logFC[, i] <- -.logFC[, i]
            }
        }
    })
})




GetSwapShinyValue <- reactive({
    req(rv.ht$n)
    unlist(lapply(
        seq_len(rv.ht$n),
        function(x) input[[paste0("compswap", x)]]
    ))
})


# First screen
output$screenHypoTest1 <- renderUI({
    if (!requireNamespace("shinyBS", quietly = TRUE)) {
        stop("Please install shinyBS: BiocManager::install('shinyBS')")
    }


    req(rv$current.obj)
    isolate({
        m <- match.metacell(DAPAR::GetMetacell(rv$current.obj),
            pattern = "missing",
            level = DAPAR::GetTypeofData(rv$current.obj)
        )
        NA.count <- length(which(m))


        if (NA.count > 0) {
            tags$p("Your dataset contains missing values. Before using the
      differential analysis, you must filter/impute them.")
        } else {
            .widgets <- rv$widgets$hypothesisTest
            shinyjs::useShinyjs()
            tagList(
                tags$div(
                    tags$div(
                        style = "display:inline-block; vertical-align: middle;
          padding-right: 20px;",
                        selectInput("anaDiff_Design", "Contrast",
                            choices = c(
                                "None" = "None", "One vs One" = "OnevsOne",
                                "One vs All" = "OnevsAll"
                            ),
                            selected = .widgets$design,
                            width = "150px"
                        )
                    ),
                    tags$div(
                        style = "display:inline-block; vertical-align: middle;
          padding-right: 20px;",
                        selectInput("diffAnaMethod", "Statistical test",
                            choices = anaDiffMethod_Choices,
                            selected = .widgets$method,
                            width = "150px"
                        )
                    ),
                    tags$div(
                        style = "display:inline-block; vertical-align: middle;
          padding-right: 20px;",
                        hidden(
                            radioButtons("ttest_options", "t-tests options",
                                choices = c("Student", "Welch"),
                                selected = .widgets$ttest_options,
                                width = "150px"
                            )
                        )
                    ),
                    tags$div(
                        style = "display:inline-block; vertical-align: middle;
          padding-right: 20px;",
                        textInput("seuilLogFC",
                            "log(FC) threshold",
                            value = .widgets$th_logFC,
                            width = "150px"
                        ),
                        module_Not_a_numericUI("test_seuillogFC")
                    ),
                    tags$div(
                        style = "display:inline-block; vertical-align: middle;
          padding-right: 20px;",
                        uiOutput("correspondingRatio")
                    ),
                    tags$div(
                        style = "display:inline-block; vertical-align: middle;
          padding-right: 20px;",
                        uiOutput("perform_btn")
                    )
                ),
                tags$hr(),
                shinyBS::bsCollapse(
                    id = "collapseExample",
                    open = "",
                    shinyBS::bsCollapsePanel(
                        title = "Swap conditions",
                        uiOutput("headerInputGroup"),
                        fluidRow(
                            column(width = 2, uiOutput("cond1_ui")),
                            column(width = 2, uiOutput("cond2_ui")),
                            column(width = 2, uiOutput("btns_ui"))
                        ),
                        style = "info"
                    )
                ),
                highchartOutput("FoldChangePlot", height = "100%")
            )
        }
    })
})


output$perform_btn <- renderUI({
    rvModProcess$moduleHypothesisTestDone[1]
    if (rvModProcess$moduleHypothesisTestDone[1]) {
        shinyjs::disabled(
            actionButton("PerformLogFCPlot",
                "Perform log FC plot",
                class = actionBtnClass
            )
        )
    } else {
        actionButton("PerformLogFCPlot",
            "Perform log FC plot",
            class = actionBtnClass
        )
    }
})

# Definition of screen 2
output$screenHypoTest2 <- renderUI({
    tagList(
        uiOutput("btn_valid")
    )
})


# Test if a hypothesis test has already been done.
observe({
    if (length(grep("HypothesisTest.", names(rv$dataset))) > 0) {
        rvModProcess$moduleHypothesisTestDone[seq_len(2)] <- TRUE
    }
})


output$correspondingRatio <- renderUI({
    ratio <- as.numeric(rv$widgets$hypothesisTest$th_logFC)
    p("(FC = ", 2^(ratio), ")")
})


output$btn_valid <- renderUI({
    req(rv$widgets$hypothesisTest$method != "None")
    req(rv$widgets$hypothesisTest$design != "None")

    actionButton("ValidTest", "Save significance test", class = actionBtnClass)
})


observeEvent(rv$widgets$hypothesisTest$method, {
    toggle(
        id = "ttest_options",
        condition = (rv$widgets$hypothesisTest$method == "ttests")
    )
})


# Highcharts plot
output$FoldChangePlot <- renderHighchart({
    req(rv$res_AllPairwiseComparisons)
    .params <- rv$current.obj@experimentData@other$Params$HypothesisTest.protein
    name <- .params$HypothesisTest$AllPairwiseCompNames$logFC
    l1 <- length(as.data.frame(fData(rv$current.obj)[, name]))
    l2 <- length(rv$res_AllPairwiseComparisons$logFC)
    req(l2 + l1 > 0)

    withProgress(message = "Computing plot...", detail = "", value = 0.5, {
        if (l1 > 0) {
            tmp.df <- as.data.frame(fData(rv$current.obj)[, name])

            th <- .params$HypothesisTest$th_logFC
            rv$tempplot$logFCDistr <- hc_logFC_DensityPlot(tmp.df, th)
        } else if (l2 > 0) {
            tmp.df <- rv$res_AllPairwiseComparisons$logFC
            th <- as.numeric(rv$widgets$hypothesisTest$th_logFC)
            rv$tempplot$logFCDistr <- hc_logFC_DensityPlot(tmp.df, th)
        }
        rv$tempplot$logFCDistr
    })
})



### Computation of comparisons selected in the variable
# 'rv$widgets$hypothesisTest$design'
ComputeComparisons <- reactive({
    req(rv$widgets$hypothesisTest$method != "None")
    req(rv$widgets$hypothesisTest$design != "None")
    rv$widgets$hypothesisTest$ttest_options


    m <- match.metacell(DAPAR::GetMetacell(rv$current.obj),
        pattern = "missing",
        level = DAPAR::GetTypeofData(rv$current.obj)
    )
    req(length(which(m)) == 0)

    df <- NULL

    switch(rv$widgets$hypothesisTest$method,
        Limma = {
            df <- DAPAR::limmaCompleteTest(
                exprs(rv$current.obj),
                pData(rv$current.obj),
                rv$widgets$hypothesisTest$design
            )
        },
        ttests = {
            df <- DAPAR::compute_t_tests(rv$current.obj,
                contrast = rv$widgets$hypothesisTest$design,
                type = rv$widgets$hypothesisTest$ttest_options
            )
        }
    )
    rv$widgets$hypothesisTest$listNomsComparaison <- colnames(df$logFC)
    #  browser()

    rvModProcess$moduleHypothesisTestDone[1] <- TRUE

    df
}) %>% bindCache(
    rv$current.obj,
    rv$widgets$hypothesisTest$method,
    rv$widgets$hypothesisTest$design,
    rv$widgets$hypothesisTest$ttest_options
)



observeEvent(input$ValidTest, {
    req(rv$res_AllPairwiseComparisons)

    rv$current.obj <- DAPAR::diffAnaSave(
        obj = rv$current.obj,
        allComp = rv$res_AllPairwiseComparisons
    )

    name <- paste("HypothesisTest.", rv$typeOfDataset, sep = "")
    rv$current.obj <- saveParameters(
        rv$current.obj,
        name,
        "HypothesisTest",
        build_ParamsList_HypothesisTest()
    )

    BuildNavbarPage()
    rvModProcess$moduleHypothesisTestDone[2] <- TRUE
    UpdateDatasetWidget(rv$current.obj, name)
    shinyjs::toggleState("ValidTest", FALSE)
})
