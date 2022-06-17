

observeEvent(rv$current.obj, {
    BuildNavbarPage()
})



observeEvent(req(input$datasets), ignoreInit = TRUE, {

    # isolate({

    if (rv$processSaved == TRUE) {
        rv$processSaved <- FALSE
    } else {
        rv$current.obj <- rv$dataset[[input$datasets]]
        if (!is.null(rv$current.obj)) {
            rv$typeOfDataset <- rv$current.obj@experimentData@other$typeOfData

            ## remettre les logFC s'ils existent
            rv$res_AllPairwiseComparisons <- Get_AllComparisons(rv$current.obj)
        }
        ClearCurrentNavPage(input$navPage)
    }

    # })
})


ClearCurrentNavPage <- function(page) {
    switch(page,
        FilteringTab = {
            resetModuleProcess("Filtering")
            .force <- rvModProcess$moduleFilteringForceReset
            rvModProcess$moduleFilteringForceReset <- .force
        },
        NormalizationTab = {
            resetModuleProcess("Normalization")
            .force <- rvModProcess$moduleNormalizationForceReset
            rvModProcess$moduleNormalizationForceReset <- .force
        },
        imputationProteinLevelTabs = {
            resetModuleProcess("ProtImputation")
            .force <- rvModProcess$moduleProtImputationForceReset
            rvModProcess$moduleProtImputationForceReset <- .force
        },
        imputationPeptideLevelTabs = {
            resetModuleProcess("PepImputation")
            .force <- rvModProcess$modulePepImputationForceReset
            rvModProcess$modulePepImputationForceReset <- .force
        },
        testTab = {
            resetModuleProcess("HypothesisTest")
            .force <- rvModProcess$moduleHypothesisTestForceReset
            rvModProcess$moduleHypothesisTestForceReset <- .force
        },
        AggregationTab = {
            resetModuleProcess("Aggregation")
            .force < rvModProcess$moduleAggregationForceReset
            rvModProcess$moduleAggregationForceReset <- .force
        },
        diffAnalysisTab = {
            resetModuleProcess("AnaDiff")
            .force <- rvModProcess$moduleAnaDiffForceReset
            rvModProcess$moduleAnaDiffForceReset <- .force
        },
        convertTab = {
            resetModuleProcess("Convert")
            .force <- rvModProcess$moduleConvertForceReset
            rvModProcess$moduleConvertForceReset <- .force
        },
        GoTab = {
            resetModuleProcess("GO")
            .force <- rvModProcess$moduleGOForceReset
            rvModProcess$moduleGOForceReset <- .force
        }
    )
}

## Change of page
observeEvent(input$navPage, {
    ClearCurrentNavPage(input$navPage)
})





ClearNavbarPage <- reactive({
    if ("dataProcessPeptTab" %in% rv$UI_TabsList) {
        removeTab(inputId = "navPage", target = "Data processing (peptide)")
        .ind <- which(rv$UI_TabsList == "dataProcessPeptTab")
        isolate({
            rv$UI_TabsList <- rv$UI_TabsList[-(.ind)]
        })
    }

    if ("dataProcessProtTab" %in% rv$UI_TabsList) {
        removeTab(inputId = "navPage", target = "Data processing (protein)")
        .ind <- which(rv$UI_TabsList == "dataProcessProtTab")
        isolate({
            rv$UI_TabsList <- rv$UI_TabsList[-(.ind)]
        })
    }

    if ("DataMiningTab" %in% rv$UI_TabsList) {
        removeTab(inputId = "navPage", target = "Data mining")
        .ind <- which(rv$UI_TabsList == "DataMiningTab")
        isolate({
            rv$UI_TabsList <- rv$UI_TabsList[-(.ind)]
        })
    }
})


#########################################################################
BuildNavbarPage <- reactive({
    rv$current.obj
    #   rv$typeOfDataset
    isolate({
        rv$UI_TabsList
    })


    ## if a dataset is in memory (ie rv$current.obj is not null
    ## remove menus to import new dataset
    removeTab(inputId = "navPage", target = "demoTab")
    removeTab(inputId = "navPage", target = "convertTab")
    removeTab(inputId = "navPage", target = "openMSnsetTab")

    if (!is.null(rv$typeOfDataset)) {
        switch(rv$typeOfDataset,
            protein = {
                if ("dataProcessPeptTab" %in% rv$UI_TabsList) {
                    removeTab(
                        inputId = "navPage",
                        target = "Data processing (peptide)"
                    )
                    .ind <- which(rv$UI_TabsList == "dataProcessPeptTab")
                    isolate({
                        rv$UI_TabsList <- rv$UI_TabsList[-(.ind)]
                    })
                }
                if (!("dataProcessProtTab" %in% rv$UI_TabsList)) {
                    insertTab(
                        inputId = "navPage",
                        navbarMenu(
                            "Data processing (protein)",
                            source(
                                file.path("ui", "ui_Filtering.R"),
                                local = TRUE
                            )$value,
                            source(
                                file.path("ui", "ui_Normalization.R"),
                                local = TRUE
                            )$value,
                            source(
                                file.path("ui", "ui_ImputationProteinLevel.R"),
                                local = TRUE
                            )$value,
                            source(
                                file.path("ui", "ui_HypothesisTest.R"),
                                local = TRUE
                            )$value
                        ),
                        target = "Data manager",
                        position = "after"
                    )
                    isolate({
                        rv$UI_TabsList <- c(
                            rv$UI_TabsList,
                            "dataProcessProtTab"
                        )
                    })
                }
            },
            peptide = {
                if ("dataProcessProtTab" %in% rv$UI_TabsList) {
                    removeTab(
                        inputId = "navPage",
                        target = "Data processing (protein)"
                    )
                    .ind <- which(rv$UI_TabsList == "dataProcessProtTab")
                    isolate({
                        rv$UI_TabsList <- rv$UI_TabsList[-(.ind)]
                    })
                }

                if (!("dataProcessPeptTab" %in% rv$UI_TabsList)) {
                    insertTab(
                        inputId = "navPage",
                        navbarMenu(
                            "Data processing (peptide)",
                            source(
                                file.path("ui", "ui_Filtering.R"),
                                local = TRUE
                            )$value,
                            source(
                                file.path("ui", "ui_Normalization.R"),
                                local = TRUE
                            )$value,
                            source(
                                file.path("ui", "ui_ImputationPeptideLevel.R"),
                                local = TRUE
                            )$value,
                            source(
                                file.path("ui", "ui_Aggregation.R"),
                                local = TRUE
                            )$value,
                            source(
                                file.path("ui", "ui_HypothesisTest.R"),
                                local = TRUE
                            )$value
                        ),
                        target = "Data manager",
                        position = "after"
                    )
                    isolate({
                        rv$UI_TabsList <- c(
                            rv$UI_TabsList,
                            "dataProcessPeptTab"
                        )
                    })
                }
            }
        )
    }


    if (("DataMiningTab" %in% rv$UI_TabsList)) {
        removeTab(inputId = "navPage", target = "Data mining")
    }

    dataset.name <- last(names(rv$dataset))
    .type <- rv$current.obj@experimentData@other$typeOfData
    prev.dataset.name <- paste0("prev.HypothesisTest.", .type)

    .params <- rv$current.obj@experimentData@other$Params
    .currdesign <- .params[[dataset.name]][["HypothesisTest"]]$design
    .prevdesign <- .params[[prev.dataset.name]][["HypothesisTest"]]$design
    if ((is.null(.currdesign) && is.null(.prevdesign)) ||
        (.currdesign == "None" && .prevdesign == "None")) {
        if (rv$typeOfDataset == "peptide") {
            insertTab(
                inputId = "navPage",
                navbarMenu(
                    "Data mining",
                    source(
                        file.path("ui", "ui_DescriptiveStatistics.R"),
                        local = TRUE
                    )$value,
                    mod_cc_ui("CC_Multi_Any"),
                    source(
                        file.path("ui", "ui_GO_Enrich.R"),
                        local = TRUE
                    )$value
                ),
                target = "Help",
                position = "before"
            )
        } else {
            insertTab(
                inputId = "navPage",
                navbarMenu(
                    "Data mining",
                    source(file.path("ui", "ui_DescriptiveStatistics.R"),
                        local = TRUE
                    )$value,
                    source(file.path("ui", "ui_GO_Enrich.R"),
                        local = TRUE
                    )$value
                ),
                target = "Help",
                position = "before"
            )
        }
        isolate({
            rv$UI_TabsList <- c(rv$UI_TabsList, "DataMiningTab")
        })
    } else {
        if (rv$typeOfDataset == "peptide") {
            insertTab(
                inputId = "navPage",
                navbarMenu(
                    "Data mining",
                    source(file.path("ui", "ui_DescriptiveStatistics.R"),
                        local = TRUE
                    )$value,
                    mod_cc_ui("CC_Multi_Any"),
                    source(
                        file.path("ui", "ui_GO_Enrich.R"),
                        local = TRUE
                    )$value,
                    source(
                        file.path("ui", "ui_AnaDiff.R"),
                        local = TRUE
                    )$value
                ),
                target = "Help",
                position = "before"
            )
        } else {
            insertTab(
                inputId = "navPage",
                navbarMenu(
                    "Data mining",
                    source(file.path("ui", "ui_DescriptiveStatistics.R"),
                        local = TRUE
                    )$value,
                    source(
                        file.path("ui", "ui_GO_Enrich.R"),
                        local = TRUE
                    )$value,
                    source(
                        file.path("ui", "ui_AnaDiff.R"),
                        local = TRUE
                    )$value
                ),
                target = "Help",
                position = "before"
            )
        }
        isolate({
            rv$UI_TabsList <- c(rv$UI_TabsList, "DataMiningTab")
        })
    }
})
