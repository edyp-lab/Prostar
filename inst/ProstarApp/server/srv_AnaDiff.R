source(file.path("server", "mod_popover_for_help.R"), local = TRUE)$value

#source(file.path("server", "mod_popover.R"), local = TRUE)$value
source(file.path("server", "mod_query_metacell.R"), local = TRUE)$value
source(file.path("server", "mod_filtering_example.R"), local = TRUE)$value
source(file.path("server", "mod_format_DT.R"), local = TRUE)$value
source(file.path("server", "mod_dl.R"), local = TRUE)$value


convertAnaDiff2DF <- reactive({
    req(rv$widgets$anaDiff)
    rv$widgets$anaDiff[sapply(rv$widgets$anaDiff, is.null)] <- NA
    df <- as.data.frame(tibble::enframe(rv$widgets$anaDiff))
    names(df) <- c("Parameter", "Value")
    rownames(df) <- NULL
    df
})


callModule(moduleVolcanoplot, "volcano_Step1",
    data = reactive({rv$resAnaDiff}),
    comp = reactive({as.character(rv$widgets$anaDiff$Comparison)}),
    tooltip = reactive({rv$widgets$anaDiff$tooltipInfo})
)

callModule(moduleVolcanoplot, "volcano_Step2",
    data = reactive({rv$resAnaDiff}),
    comp = reactive({as.character(rv$widgets$anaDiff$Comparison)}),
    tooltip = reactive({rv$widgets$anaDiff$tooltipInfo})
)

format_DT_server("params_AnaDiff",
    data = reactive({convertAnaDiff2DF()}),
    filename = "AnaDiffParams"
)


mod_Not_a_numeric_server("test_seuilPVal",
                         reactive({input$seuilPVal}))

callModule(moduleProcess, "moduleProcess_AnaDiff",
    isDone = reactive({rvModProcess$moduleAnaDiffDone}),
    pages = reactive({rvModProcess$moduleAnaDiff}),
    rstFunc = resetModuleAnaDiff,
    forceReset = reactive({rvModProcess$moduleAnaDiffForceReset})
)


######
resetModuleAnaDiff <- reactive({
    # if (rv$widgets$anaDiff$swapVolcano == TRUE){
    #   rv$resAnaDiff$logFC <- - rv$resAnaDiff$logFC
    #   rv$widgets$anaDiff$swapVolcano <- FALSE
    # }

    ## update widgets values (reactive values)
    resetModuleProcess("AnaDiff")

    rv$nbTotalAnaDiff <- NULL
    rv$nbSelectedAnaDiff <- NULL
    rv$nbSelectedTotal_Step3 <- NULL
    rv$nbSelected_Step3 <- NULL
    rv$conditions <- list(cond1 = NULL, cond2 = NULL)
    rv$calibrationRes <- NULL
    rv$errMsgcalibrationPlot <- NULL
    rv$errMsgcalibrationPlotALL <- NULL
    rv$pi0 <- NULL

    rv$widgets$anaDiff$Comparison <- "None"
    rv$widgets$anaDiff$Condition1 <- ""
    rv$widgets$anaDiff$Condition2 <- ""
    rv$widgets$anaDiff$val_vs_percent <- "Value"
    rv$widgets$anaDiff$ChooseFilters <- "None"
    rv$widgets$anaDiff$seuilNA_percent <- 0
    rv$widgets$anaDiff$seuilNA <- 0
    rv$widgets$anaDiff$filter_th_NA <- 0
    rv$widgets$anaDiff$calibMethod <- "None"
    rv$widgets$anaDiff$numValCalibMethod <- 0
    rv$widgets$anaDiff$th_pval <- 0
    rv$widgets$anaDiff$FDR <- 0
    rv$widgets$anaDiff$NbSelected <- 0
    rv$widgets$anaDiff$nBinsHistpval <- 80
    rv$widgets$anaDiff$downloadAnaDiff <- "All"

    .protId <- rv$current.obj@experimentData@other$proteinId
    rv$widgets$anaDiff$tooltipInfo <- .protId

    rv$widgets$anaDiff[sapply(rv$widgets$anaDiff, is.null)] <- NA
    rvModProcess$moduleAnaDiffDone <- rep(FALSE, 4)

    rv_anaDiff$filename <- NULL
    rv_anaDiff$local.reset <- rv_anaDiff$local.reset + 1
    UpdateCompList()
    ## update dataset to put the previous one
    # rv$current.obj <- rv$dataset[[last(names(rv$dataset))]]
    # rv$resAnaDiff <- NULL
})


rv_anaDiff <- reactiveValues(
    filename = NULL,
    local.reset = 0
)




###
### ------------------- SCREEN 1 ------------------------------
###


# Fill the variable 'rv$resAnaDiff' with informations relatives to
# the comparison choosen by the user.
# Concertely, it extracts data from the variable rv$res_AllPairwiseComparisons
# which contains all info (logFC and pValue) for all comparisons.
UpdateCompList <- reactive({
    rv$widgets$anaDiff$Comparison
    isolate({
        if (rv$widgets$anaDiff$Comparison == "None") {
            rv$resAnaDiff <- NULL
            return(NULL)
        } else {
            index <- which(
                paste(
                    as.character(rv$widgets$anaDiff$Comparison),
                    "_logFC",
                    sep = ""
                ) == colnames(rv$res_AllPairwiseComparisons$logFC)
            )

            # Update of the list rv$resAnaDiff
            rv$widgets$anaDiff$Condition1 <- strsplit(as.character(rv$widgets$anaDiff$Comparison), "_vs_")[[1]][1]
            rv$widgets$anaDiff$Condition2 <- strsplit(as.character(rv$widgets$anaDiff$Comparison), "_vs_")[[1]][2]

            # if (input$swapVolcano == FALSE)
            rv$resAnaDiff <- list(
                logFC = (rv$res_AllPairwiseComparisons$logFC)[, index],
                P_Value = (rv$res_AllPairwiseComparisons$P_Value)[, index],
                condition1 = rv$widgets$anaDiff$Condition1,
                condition2 = rv$widgets$anaDiff$Condition2
            )
            rvModProcess$moduleAnaDiffDone[1] <- TRUE
        }
    })
})


# By default, the tooltip for volcanoplot is set to the proteinId
observe({
  req(rv$current.obj)
    if (is.null(rv$widgets$anaDiff$tooltipInfo)) {
        .protId <- rv$current.obj@experimentData@other$proteinId
        rv$widgets$anaDiff$tooltipInfo <- .protId
    }
})


output$screenAnaDiff1 <- renderUI({
    isolate({
        tagList(
            tags$div(
                tags$div(
                    style = "display:inline-block; vertical-align: top; padding-right: 60px",
                    selectInput("selectComparison", "Select a comparison",
                        choices = c("None" = "None", GetPairwiseCompChoice()),
                        selected = rv$widgets$anaDiff$Comparison,
                        width = "300px"
                    )
                )
            ),
            uiOutput("pushpval_ui"),
            tags$hr(),
            tags$div(
                tags$div(
                    style = "display:inline-block; vertical-align: top; padding-right: 60px",
                    moduleVolcanoplotUI("volcano_Step1")
                ),
                tags$div(
                    style = "display:inline-block; vertical-align: top;",
                    tagList(
                        br(),
                        uiOutput("volcanoTooltip_UI")
                    )
                )
            )
        )
    })
})


output$pushpval_ui <- renderUI({
  req(rv$widgets$anaDiff$Comparison != "None")
  
    popover_for_help_server("modulePopover_pushPVal",
        title = h3("Push p-value"),
            content = "This functionality is useful in case of multiple pairwise comparisons 
              (more than 2 conditions): At the filtering step, a given analyte X
              (either peptide or protein) may have been kept because it contains
              very few missing values in a given condition (say Cond. A), even
              though it contains (too) many of them in all other conditions
              (say Cond B and C only contains 'MEC' type missing values).
              Thanks to the imputation step, these missing values are no
              longer an issue for the differential analysis, at least from
              the computational viewpoint. However, statistically speaking,
              when performing B vs C, the test will rely on too many imputed
              missing values to derive a meaningful p-value: It may be wiser
              to consider analyte X as non-differentially abundant, regardless
              the test result (and thus, to push its p-value to 1). This is just
              the role of the P-value push parameter. It makes it possible to
              introduce a new filtering step that only applies to each pairwise
              comparison, and which assigns a p-value of 1 to analytes that, for
              the considered comparison are assumed meaningless due to too many
              missing values (before imputation)."
          )

    wellPanel(
        popover_for_help_ui("modulePopover_pushPVal"),
        mod_query_metacell_ui("AnaDiff_query"))
       # shinyjs::disabled(actionButton("AnaDiff_performFilteringMV", "Push p-value", class = actionBtnClass))

})

#---------------------------

# Extract conditions of the current dataset to represent the
# selected comparison. It returns a subset of the current dataset that will 
# be used to filter the data within the 'Push p-value' feature
Get_Dataset_to_Analyze <- reactive({
    #req(rv$widgets$anaDiff$Comparison != "None")
    #req(rv$current.obj)

 
    datasetToAnalyze <- NULL
if (rv$widgets$anaDiff$Comparison == "None" || is.null(rv$current.obj))
  return(NULL)
  
    if (length(grep("all-", rv$widgets$anaDiff$Comparison)) == 1) {
        .conds <- Biobase::pData(rv$current.obj)$Condition
        condition1 <- strsplit(as.character(rv$widgets$anaDiff$Comparison), "_vs_")[[1]][1]
        ind_virtual_cond2 <- which(.conds != condition1)
        datasetToAnalyze <- rv$current.obj
        Biobase::pData(datasetToAnalyze)$Condition[ind_virtual_cond2] <- "virtual_cond_2"
    } else {
        condition1 <- strsplit(as.character(rv$widgets$anaDiff$Comparison), "_vs_")[[1]][1]
        condition2 <- strsplit(as.character(rv$widgets$anaDiff$Comparison), "_vs_")[[1]][2]

        if (substr(condition1, 1, 1) == "(" &&
            substr(condition1, nchar(condition1), nchar(condition1)) == ")") {
            condition1 <- sub("^.(.*).$", "\\1", condition1)
        }

        if (substr(condition2, 1, 1) == "(" &&
            substr(condition2, nchar(condition2), nchar(condition2)) == ")") {
            condition2 <- sub("^.(.*).$", "\\1", condition2)
        }



        ind <- c(
            which(Biobase::pData(rv$current.obj)$Condition == condition1),
            which(Biobase::pData(rv$current.obj)$Condition == condition2)
        )

        datasetToAnalyze <- rv$current.obj[, ind]
        datasetToAnalyze@experimentData@other$names_metacell <-
            rv$current.obj@experimentData@other$names_metacell[ind]
    }

    datasetToAnalyze
}) %>% bindCache(rv$current.obj, rv$widgets$anaDiff$Comparison)



GetPairwiseCompChoice <- reactive({
    req(rv$res_AllPairwiseComparisons$logFC)
    ll <- unlist(strsplit(colnames(rv$res_AllPairwiseComparisons$logFC), "_logFC"))
    ll
})

GetFiltersScope <- function()
  c("Whole Line" = "WholeLine",
    "Whole matrix" = "WholeMatrix",
    "For every condition" = "AllCond",
    "At least one condition" = "AtLeastOneCond"
  )




      observe({
          rv$AnaDiff_indices <- mod_query_metacell_server(id = "AnaDiff_query",
                                             obj = reactive({req(Get_Dataset_to_Analyze())}),
                                             reset = reactive({rv_anaDiff$local.reset}),
                                             op_names = reactive({c('Push p-value', 'Keep original p-value')})
                                             )

})


      observeEvent(req(rv$AnaDiff_indices()$indices),{
         # shinyjs::toggleState("AnaDiff_performFilteringMV",
         #                 condition = length(AnaDiff_indices()$indices > 0))
         #  
          UpdateCompList()
          .ind <- rv$AnaDiff_indices()
          params <- rv$AnaDiff_indices()$params
          .protId <- rv$current.obj@experimentData@other$proteinId
          rv$widgets$anaDiff$MetacellTag <- params$MetacellTag
          rv$widgets$anaDiff$KeepRemove <- params$KeepRemove
          rv$widgets$anaDiff$ChooseFilters <- params$MetacellFilters
          rv$widgets$anaDiff$seuilNA_percent <- params$metacell_percent_th
          rv$widgets$anaDiff$seuilNA <- params$metacell_value_th
          rv$widgets$anaDiff$val_vs_percent <- params$val_vs_percent
          rv$widgets$anaDiff$operator <- params$metacellFilter_operator
          rv$widgets$anaDiff$tooltipInfo <- .protId
          
          if (as.character(rv$widgets$anaDiff$ChooseFilters) == "None") {
              GetBackToCurrentResAnaDiff()
          }
          
          #--------------------------------

          if (!is.null(rv$AnaDiff_indices()$indices) && length(.ind$indices) < nrow(Get_Dataset_to_Analyze())) {
              
              if (rv$widgets$anaDiff$KeepRemove == 'delete')
                  indices_to_push <- .ind$indices
              else if (rv$widgets$anaDiff$KeepRemove == 'keep')
                  indices_to_push <- seq_len(nrow(Get_Dataset_to_Analyze()))[-(.ind$indices)]
              
              rv$resAnaDiff$P_Value[indices_to_push] <- 1
              n <- length(rv$resAnaDiff$P_Value)
              rv$resAnaDiff$pushed <- seq_len(n)[indices_to_push]
              
              #shinyjs::toggleState("div_AnaDiff_query", condition = FALSE)
          }
          #shinyjs::toggleState("div_AnaDiff_query", condition = TRUE)
})

########################################################
## Perform missing values filtering
########################################################
# observeEvent(input$AnaDiff_performFilteringMV, ignoreInit = TRUE, ignoreNULL = TRUE,{
#         UpdateCompList()
#         .ind <- AnaDiff_indices()
#         .protId <- rv$current.obj@experimentData@other$proteinId
#         rv$widgets$anaDiff$MetacellTag <- .ind$params$MetacellTag
#         rv$widgets$anaDiff$KeepRemove <- .ind$params$KeepRemove
#         rv$widgets$anaDiff$ChooseFilters <- .ind$params$MetacellFilters
#         rv$widgets$anaDiff$seuilNA_percent <- .ind$params$metacell_percent_th
#         rv$widgets$anaDiff$seuilNA <- .ind$params$metacell_value_th
#         rv$widgets$anaDiff$val_vs_percent <- .ind$params$val_vs_percent
#         rv$widgets$anaDiff$operator <- .ind$params$metacellFilter_operator
#         rv$widgets$anaDiff$tooltipInfo <- .protId
# 
#         if (as.character(rv$widgets$anaDiff$ChooseFilters) == "None") {
#             GetBackToCurrentResAnaDiff()
#         }
# 
#         #--------------------------------
#         if (!is.null(AnaDiff_indices()$indices) && length(.ind$indices) < nrow(Get_Dataset_to_Analyze())) {
#             rv$resAnaDiff$P_Value[-(.ind$indices)] <- 1
#             n <- length(rv$resAnaDiff$P_Value)
#             rv$resAnaDiff$pushed <- seq_len(n)[-(.ind$indices)]
#         }
#     }
# )

observeEvent(input$selectComparison, ignoreInit = TRUE, {
    rv$widgets$anaDiff$Comparison <- input$selectComparison
    .protId <- rv$current.obj@experimentData@other$proteinId
    rv$widgets$anaDiff$tooltipInfo <- .protId
    UpdateCompList()

    req(rv$widgets$anaDiff$Comparison)
    cond1 <- rv$widgets$anaDiff$Condition1
    cond2 <- rv$widgets$anaDiff$Condition2

    rv_anaDiff$filename <- paste0("anaDiff_", cond1, "_vs_", cond2, ".xlsx")
})





## --------------------------------------------------------
## ---------------------------------------------------------


output$volcanoTooltip_UI <- renderUI({
    req(rv$widgets$anaDiff$Comparison != "None")
    rv$widgets$anaDiff$tooltipInfo
    isolate({
        tagList(
            popover_for_help_ui("modulePopover_volcanoTooltip"),
            selectInput("tooltipInfo",
                label = NULL,
                choices = colnames(Biobase::fData(rv$current.obj)),
                selected = rv$widgets$anaDiff$tooltipInfo,
                multiple = TRUE,
                selectize = FALSE,
                width = "300px", size = 5
            ),
            actionButton("validTooltipInfo",  "Validate tooltip choice", class = actionBtnClass)
        )
    })
})


popover_for_help_server("modulePopover_volcanoTooltip",
    title = "Tooltip",
        content = "Infos to be displayed in the tooltip of volcanoplot"
    )


popover_for_help_server("modulePopover_keepLines",
    title = "n values",
        content = "Keep the lines which have at least n intensity values."
    )



GetBackToCurrentResAnaDiff <- reactive({
    req(rv$res_AllPairwiseComparisons)

    index <- which(
        paste(
            as.character(rv$widgets$anaDiff$Comparison), "_logFC",
            sep = ""
        ) ==
            colnames(rv$res_AllPairwiseComparisons$logFC)
    )
    rv$resAnaDiff <- list(
        logFC = (rv$res_AllPairwiseComparisons$logFC)[, index],
        P_Value = (rv$res_AllPairwiseComparisons$P_Value)[, index],
        condition1 = strsplit(
            as.character(rv$widgets$anaDiff$Comparison), "_vs_"
        )[[1]][1],
        condition2 = strsplit(
            as.character(rv$widgets$anaDiff$Comparison), "_vs_"
        )[[1]][2]
    )
    rv$resAnaDiff
})





not_a_numeric <- function(input) {
    if (is.na(as.numeric(input))) {
        "Please input a number"
    } else {
        NULL
    }
}


###
### ------------------- SCREEN 2 ------------------------------
###



output$screenAnaDiff2 <- renderUI({
    req(as.character(rv$widgets$anaDiff$Comparison))

    tagList(
        tags$div(
            tags$div(
                style = "display:inline-block; vertical-align: middle;
        padding-right: 40px;",
                selectInput("calibrationMethod", "Calibration method",
                    choices = c("None" = "None", calibMethod_Choices),
                    selected = rv$widgets$anaDiff$calibMethod,
                    width = "200px"
                )
            ),
            tags$div(
                style = "display:inline-block; vertical-align: middle;",
                uiOutput("numericalValForCalibrationPlot")
            ),
            tags$div(
                style = "display:inline-block; vertical-align: middle;
        padding-right: 40px;",
                uiOutput("nBins_ui")
            ),
            tags$div(
                style = "display:inline-block; vertical-align: middle;",
                p(tags$strong(paste0(
                    "value of pi0: ",
                    round(as.numeric(rv$pi0), digits = 2)
                )))
            )
        ),
        tags$hr(),
        fluidRow(
            column(width = 6, fluidRow(
                style = "height:800px;",
                imageOutput("calibrationPlotAll", height = "800px")
            )),
            column(
                width = 6, fluidRow(
                    style = "height:400px;",
                    imageOutput("calibrationPlot", height = "400px")
                ),
                fluidRow(style = "height:400px;", highchartOutput("histPValue"))
            )
        )
    )
})





observeEvent(input$nBinsHistpval, {
    rv$widgets$anaDiff$nBinsHistpval <- as.numeric(input$nBinsHistpval)
})


observeEvent(input$calibrationMethod, {
    rv$widgets$anaDiff$calibMethod <- input$calibrationMethod
    shinyjs::toggle("numericValCalibration",
        condition = rv$widgets$anaDiff$calibMethod == "numeric value"
    )
})


observeEvent(input$numericValCalibration, {
    rv$widgets$anaDiff$numValCalibMethod <- input$numericValCalibration
})


output$nBins_ui <- renderUI({
    req(rv$resAnaDiff)
    req(rv$pi0)
    req(rv$widgets$anaDiff$nBinsHistpval)

    selectInput("nBinsHistpval", "n bins of p-value histogram",
        choices = c(1, seq(from = 0, to = 100, by = 10)[-1]),
        selected = rv$widgets$anaDiff$nBinsHistpval, width = "80px"
    )
})


histPValue <- reactive({
    req(rv$resAnaDiff)
    req(rv$pi0)
    req(rv$widgets$anaDiff$nBinsHistpval)

    rv$widgets$hypothesisTest$th_logFC

    if (is.null(rv$widgets$hypothesisTest$th_logFC) ||
        is.na(rv$widgets$hypothesisTest$th_logFC) ||
        (length(rv$resAnaDiff$logFC) == 0)) {
        return()
    }
    m <- match.metacell(DAPAR::GetMetacell(rv$current.obj),
                        pattern = c("Missing", "Missing POV", "Missing MEC"),
                        level = "peptide"
                        )
    if (length(which(m)) > 0) {
        return()
    }

    # isolate({
    t <- NULL
    method <- NULL
    t <- rv$resAnaDiff$P_Value
    .thlogfc <- rv$widgets$hypothesisTest$th_logFC
    t <- t[which(abs(rv$resAnaDiff$logFC) >= .thlogfc)]
    toDelete <- which(t == 1)
    if (length(toDelete) > 0) {
        t <- t[-toDelete]
    }
    histPValue_HC(t,
        bins = as.numeric(rv$widgets$anaDiff$nBinsHistpval),
        pi0 = rv$pi0
    )
    # })
})

output$histPValue <- renderHighchart({
    histPValue()
})



output$numericalValForCalibrationPlot <- renderUI({
    rv$widgets$anaDiff$calibMethod
    if (rv$widgets$anaDiff$calibMethod == "numeric value") {
        numericInput("numericValCalibration",
            "Proportion of TRUE null hypohtesis",
            value = rv$widgets$anaDiff$numValCalibMethod,
            step = 0.05
        )
    }
})


output$calibrationResults <- renderUI({
    req(rv$calibrationRes)
    rv$widgets$hypothesisTest$th_logFC
    rv$current.obj


    txt <- paste("Non-DA protein proportion = ",
        round(100 * rv$calibrationRes$pi0, digits = 2), "%<br>",
        "DA protein concentration = ",
        round(100 * rv$calibrationRes$h1.concentration, digits = 2),
        "%<br>",
        "Uniformity underestimation = ",
        rv$calibrationRes$unif.under, "<br><br><hr>",
        sep = ""
    )
    HTML(txt)
})




calibrationPlot <- reactive({
    req(rv$widgets$anaDiff$calibMethod != "None")
    rv$widgets$hypothesisTest$th_logFC
    rv$resAnaDiff
    req(rv$current.obj)

    if (length(rv$resAnaDiff$logFC) == 0) {
        return()
    }

    m <- match.metacell(DAPAR::GetMetacell(rv$current.obj),
                        pattern = c("Missing", "Missing POV", "Missing MEC"),
                        level = "peptide")
    if (length(which(m)) > 0) {
        return()
    }

    cond <- c(rv$resAnaDiff$condition1, rv$resAnaDiff$condition2)


    t <- NULL
    method <- NULL
    t <- rv$resAnaDiff$P_Value
    .thlogfc <- rv$widgets$hypothesisTest$th_logFC
    t <- t[which(abs(rv$resAnaDiff$logFC) >= .thlogfc)]
    toDelete <- which(t == 1)
    if (length(toDelete) > 0) {
        t <- t[-toDelete]
    }

    l <- NULL
    ll <- NULL
    result <- tryCatch(
        {
            if ((rv$widgets$anaDiff$calibMethod == "numeric value") &&
                !is.null(rv$widgets$anaDiff$numValCalibMethod)) {
                ll <- catchToList(
                    wrapperCalibrationPlot(
                        t,
                        rv$widgets$anaDiff$numValCalibMethod
                    )
                )
                .warns <- ll$warnings[grep("Warning:", ll$warnings)]
                rv$errMsgCalibrationPlot <- .warns
            } else if (rv$widgets$anaDiff$calibMethod == "Benjamini-Hochberg") {
                ll <- catchToList(wrapperCalibrationPlot(t, 1))
                .warns <- ll$warnings[grep("Warning:", ll$warnings)]
                rv$errMsgCalibrationPlot <- .warns
            } else {
                ll <- catchToList(
                    wrapperCalibrationPlot(t, rv$widgets$anaDiff$calibMethod)
                )
                .warns <- ll$warnings[grep("Warning:", ll$warnings)]
                rv$errMsgCalibrationPlot <- .warns
            }
            rv$pi0 <- ll$value$pi0

            rvModProcess$moduleAnaDiffDone[2] <- !is.null(rv$pi0)
        },
        warning = function(w) {
            shinyjs::info(paste("Calibration plot", ":",
                conditionMessage(w),
                sep = " "
            ))
        },
        error = function(e) {
            shinyjs::info(paste("Calibration plot", ":",
                conditionMessage(e),
                sep = " "
            ))
        },
        finally = {
            # cleanup-code
        }
    )
})

output$calibrationPlot <- renderImage(
    {
        outfile <- tempfile(fileext = ".png")

        # Generate a png
        png(outfile, width = 600, height = 500)
        calibrationPlot()
        dev.off()

        # Return a list
        list(
            src = outfile,
            alt = "This is alternate text"
        )
    },
    deleteFile = TRUE
)




output$errMsgCalibrationPlot <- renderUI({
    req(rv$errMsgCalibrationPlot)
    rv$widgets$hypothesisTest$th_logFC
    req(rv$current.obj)

    txt <- NULL

    for (i in 1:length(rv$errMsgCalibrationPlot)) {
        txt <- paste(txt, "errMsgCalibrationPlot: ",
            rv$errMsgCalibrationPlot[i], "<br>",
            sep = ""
        )
    }

    div(HTML(txt), style = "color:red")
})


output$errMsgCalibrationPlotAll <- renderUI({
    rv$errMsgCalibrationPlotAll
    rv$widgets$hypothesisTest$th_logFC
    req(rv$current.obj)
    if (is.null(rv$errMsgCalibrationPlotAll)) {
        return()
    }

    txt <- NULL
    for (i in 1:length(rv$errMsgCalibrationPlotAll)) {
        txt <- paste(txt, "errMsgCalibrationPlotAll:",
            rv$errMsgCalibrationPlotAll[i], "<br>",
            sep = ""
        )
    }

    div(HTML(txt), style = "color:red")
})



calibrationPlotAll <- reactive({
    rv$resAnaDiff
    req(rv$current.obj)

    if (is.na(rv$widgets$hypothesisTest$th_logFC) ||
        (length(rv$resAnaDiff$logFC) == 0)) {
        return()
    }
    m <- match.metacell(DAPAR::GetMetacell(rv$current.obj),
                        pattern = c("Missing", "Missing POV", "Missing MEC"),
                        level = "peptide")
    if (length(which(m)) > 0) {
        return()
    }

    cond <- c(rv$resAnaDiff$condition1, rv$resAnaDiff$condition2)

    t <- NULL
    method <- NULL
    t <- rv$resAnaDiff$P_Value
    .thlogfc <- rv$widgets$hypothesisTest$th_logFC
    t <- t[which(abs(rv$resAnaDiff$logFC) >= .thlogfc)]
    toDelete <- which(t == 1)
    if (length(toDelete) > 0) {
        t <- t[-toDelete]
    }

    l <- NULL
    result <- tryCatch(
        {
            l <- catchToList(wrapperCalibrationPlot(t, "ALL"))
            .warns <- l$warnings[grep("Warning:", l$warnings)]
            rv$errMsgCalibrationPlotAll <- .warns
            rvModProcess$moduleAnaDiffDone[2] <- !is.null(rv$pi0)
        },
        warning = function(w) {
            shinyjs::info(paste("Calibration Plot All methods", ":",
                conditionMessage(w),
                sep = " "
            ))
        },
        error = function(e) {
            shinyjs::info(paste("Calibration Plot All methods", ":",
                conditionMessage(e),
                sep = " "
            ))
        },
        finally = {
            # cleanup-code
        }
    )
})



#--------------------------------------------------
output$calibrationPlotAll <- renderImage(
    {
        outfile <- tempfile(fileext = ".png")

        # Generate a png
        png(outfile, width = 600, height = 500)
        calibrationPlotAll()
        dev.off()

        # Return a list
        list(
            src = outfile,
            alt = "This is alternate text"
        )
    },
    deleteFile = TRUE
)



###
### ------------------- SCREEN 3 ------------------------------
###

output$screenAnaDiff3 <- renderUI({
    req(as.character(rv$widgets$anaDiff$Comparison) != "None")


    isolate({
        tagList(
            tags$div(
                tags$div(style = "display:inline-block; vertical-align: center; padding-right: 2px;",
                         popover_for_help_ui("modulePopover_pValThreshold"),
                        textInput("seuilPVal", NULL,
                                  value = rv$widgets$anaDiff$th_pval, width = "100px"),
                        radioButtons("type_pval", "Type of value",
                                 choices = c('raw pval' = 'raw pval',
                                             '-log10()' = '-log10()')),
                    
                        actionButton("valid_seuilPVal", "Validate value",
                                     class = actionBtnClass)
                ),
                tags$div(style = "display:inline-block; vertical-align: top;",
                    mod_Not_a_numeric_ui("test_seuilPVal")
                )
            ),
            tags$hr(),
            tagList(
                tags$div(
                    tags$div(style = "display:inline-block; vertical-align: top;",
                        htmlOutput("showFDR"),
                        withProgress(message = "", detail = "", value = 1, {
                            moduleVolcanoplotUI("volcano_Step2")
                        })
                    ),
                    tags$div(style = "display:inline-block; vertical-align: top;",
                        uiOutput("tooltipInfo"),
                        checkboxInput("showpvalTable", "Show p-value table",
                            value = FALSE
                        ),
                        shinyjs::hidden(checkboxInput('viewAdjPval', 'View adjusted p-value', value = FALSE)),
                        radioButtons("downloadAnaDiff", "Download as Excel file",
                            choices = c("All data" = "All",
                                        "only DA" = "onlyDA"),
                            selected = rv$widgets$anaDiff$downloadAnaDiff
                        ),
                        downloadButton("downloadSelectedItems", "Download",
                            class = actionBtnClass
                        )
                    )
                ),
                hidden(DT::DTOutput("anaDiff_selectedItems"))
            )
        )
    })
})



###
### ------------------- SCREEN 4 ------------------------------
###



output$screenAnaDiff4 <- renderUI({
    req(as.character(rv$widgets$anaDiff$Comparison) != "None")
    tagList(
        format_DT_ui("params_AnaDiff")
    )
})

output$diffAna_Summary <- renderUI({
    req(as.character(rv$widgets$anaDiff$Comparison) != "None")

    tagList(
        format_DT_ui("params_AnaDiff")
    )
})


#################################################################
###### Set code for widgets managment
################################################################


observeEvent(input$valid_seuilPVal, {
    req(input$seuilPVal)
    tmp <- gsub(",", ".", input$seuilPVal, fixed = TRUE)

    if (input$type_pval == 'raw pval')
        rv$widgets$anaDiff$th_pval <- -log10(as.numeric(tmp))
    else if (input$type_pval == '-log10()')
        rv$widgets$anaDiff$th_pval <- as.numeric(tmp)
})



observeEvent(input$showpvalTable, {
    #print("show : anaDiff_selectedItems")
    shinyjs::toggle(id = "anaDiff_selectedItems", condition = isTRUE(input$showpvalTable) )
    shinyjs::toggle(id = "viewAdjPval", condition = isTRUE(input$showpvalTable) )
})


observeEvent(input$validTooltipInfo, {

    .tmp <- c(
      rv$current.obj@experimentData@other$proteinId, 
      input$tooltipInfo
    )
    rv$widgets$anaDiff$tooltipInfo <- unique(.tmp)

})

observeEvent(input$downloadAnaDiff, {
    rv$widgets$anaDiff$downloadAnaDiff <- input$downloadAnaDiff
})

popover_for_help_server("modulePopover_pValThreshold",
    title = "p-val cutoff",
        content = "Define the -log10(p_value) threshold"
    )



output$anaDiff_selectedItems <- DT::renderDT({
    input$viewAdjPval
    df <- GetSelectedItems()
    
    if (input$viewAdjPval){
        df <- df[order(df$Adjusted_PValue, decreasing=FALSE), ]
        .coldefs <- list(list(width = "200px", targets = "_all"))
    } else {
        name <- paste0("Adjusted_PValue (",
                       as.character(rv$widgets$anaDiff$Comparison), ")")
        .coldefs <- list(
            list(width = "200px", targets = "_all"),
            list(targets = (which(colnames(df)==name) - 1), visible = FALSE))
    }
    
    
    DT::datatable(df,
        escape = FALSE,
        rownames = FALSE,
        options = list(initComplete = initComplete(),
                       dom = "frtip",
                       pageLength = 100,
                       scrollY = 500,
                       scroller = TRUE,
                       server = TRUE,
                       columnDefs = .coldefs,
                       ordering = !input$viewAdjPval
                       )
        ) %>%
        DT::formatStyle(
            paste0("isDifferential (",
                   as.character(rv$widgets$anaDiff$Comparison), ")"),
            target = "row",
            backgroundColor = DT::styleEqual(c(0, 1), c("white", orangeProstar))
        )
})



output$downloadSelectedItems <- downloadHandler(
    filename = reactive({rv_anaDiff$filename}),
    content = function(file) {
        DA_Style <- openxlsx::createStyle(fgFill = orangeProstar)
        hs1 <- openxlsx::createStyle(fgFill = "#DCE6F1",
                                     halign = "CENTER",
                                     textDecoration = "italic",
                                     border = "Bottom")
        
        wb <- openxlsx::createWorkbook() # Create wb in R
        openxlsx::addWorksheet(wb, sheetName = "DA result") # create sheet
        openxlsx::writeData(wb,
                            sheet = 1,
                            as.character(rv$widgets$anaDiff$Comparison),
                            colNames = TRUE,
                            headerStyle = hs1
                            )
        openxlsx::writeData(wb,
                            sheet = 1,
                            startRow = 3,
                            GetSelectedItems(),
                            )

        .txt <- paste0("isDifferential (",
                       as.character(rv$widgets$anaDiff$Comparison),
                       ")")

        ll.DA.row <- which(GetSelectedItems()[, .txt] == 1)
        ll.DA.col <- rep(which(colnames(GetSelectedItems()) == .txt),
            length(ll.DA.row) )

        openxlsx::addStyle(wb,
            sheet = 1, cols = ll.DA.col,
            rows = 3 + ll.DA.row, style = DA_Style
        )

        tempFile <- tempfile(fileext = ".xlsx")
        openxlsx::saveWorkbook(wb, file = tempFile, overwrite = TRUE)

        file.rename(tempFile, file)
    }
)


Get_adjusted_pvalues <- reactive({
    req(rv$current.obj)
    rv$widgets$anaDiff$numValCalibMethod
    rv$widgets$anaDiff$calibMethod
    req(rv$resAnaDiff)
    
    .calibMethod <- NULL
    if (rv$widgets$anaDiff$calibMethod == "Benjamini-Hochberg") {
        .calibMethod <- 1
    } else if (rv$widgets$anaDiff$calibMethod == "numeric value") {
        .calibMethod <- as.numeric(rv$widgets$anaDiff$numValCalibMethod)
    } else {
        .calibMethod <- rv$widgets$anaDiff$calibMethod
    }
    
    
    df <- data.frame(id = rownames(rv$current.obj),
                     logFC = rv$resAnaDiff[["logFC"]],
                     pval = rv$resAnaDiff[["P_Value"]])
    
    
    threshold_LogFC <- rv$widgets$hypothesisTest$th_logFC
    
    upItems <- which(abs(rv$resAnaDiff[["logFC"]]) >= threshold_LogFC)
    df <- df[upItems,]
    
    rv$widgets$anaDiff$adjusted_pvalues <- diffAnaComputeAdjustedPValues(
        df$pval,
        .calibMethod)
    
    rv$widgets$anaDiff$adjusted_pvalues
})

Get_FDR <- reactive({
    req(rv$current.obj)
    rv$widgets$anaDiff$th_pval
    Get_adjusted_pvalues()
    
    adj.pval <- Get_adjusted_pvalues()
    upitems <- which(-log10(adj.pval) >= rv$widgets$anaDiff$th_pval)
    
    fdr <- max(adj.pval[upitems])
    rvModProcess$moduleAnaDiffDone[3] <- TRUE
    as.numeric(fdr)
})


output$showFDR <- renderUI({
    req(rv$current.obj)
    #browser()
    nb <- length(
        which(
            GetSelectedItems()[paste0(
                "isDifferential (",
                as.character(rv$widgets$anaDiff$Comparison), ")"
            )] == 1
        )
    )
    th <- Get_FDR() * nb
    #print(th)


    tagList(
        if (!is.infinite(Get_FDR())) {
            tags$p(style = "font-size: 25px;", 
                   "FDR = ",
                round(100 * Get_FDR(), digits = 2), " % (p-value = ",
                signif(10^(-(rv$widgets$anaDiff$th_pval)), digits = 3), ")"
            )
        } else {
            tags$p(style = "font-size: 25px;", "FDR = NA")
        },
        if (th < 1) {
            tags$p(style = "color: red", paste0(
                "Warning: With such a dataset size (",
                nb, " selected discoveries), an FDR of ",
                round(100 * Get_FDR(), digits = 2),
                "% should be cautiously interpreted as strictly less than one
        discovery (", round(th, digits = 2), ") is expected to be false"
            ))
        }
    )
})





output$equivPVal <- renderUI({
    req(rv$widgets$anaDiff$th_pval)

    tags$p(paste0(
        "(p-value = ",
        signif(10^(-(rv$widgets$anaDiff$th_pval)), digits = 3), ")"
    ))
})


output$equivLog10 <- renderText({
    req(rv$widgets$anaDiff$th_pval)

    tags$p(paste0(
        "-log10 (p-value) = ",
        signif(-log10(rv$widgets$anaDiff$th_pval / 100), digits = 1)
    ))
})


GetSelectedItems <- reactive({
    req(rv$resAnaDiff)
    rv$widgets$anaDiff$downloadAnaDiff


    t <- NULL
    thpval <- rv$widgets$anaDiff$th_pval
    thlogfc <- rv$widgets$hypothesisTest$th_logFC
    upItems1 <- which(-log10(rv$resAnaDiff$P_Value) >= thpval)
    upItems2 <- which(abs(rv$resAnaDiff$logFC) >= thlogfc)

    if (rv$widgets$anaDiff$downloadAnaDiff == "All") {
        selectedItems <- 1:nrow(rv$current.obj)
        significant <- rep(0, nrow(rv$current.obj))
        significant[intersect(upItems1, upItems2)] <- 1
    } else {
        selectedItems <- intersect(upItems1, upItems2)
        significant <- rep(1, length(selectedItems))
    }
    

   # if (input$viewAdjPval){
        t <- data.frame(
        id = rownames(Biobase::exprs(rv$current.obj))[selectedItems],
        logFC = round(rv$resAnaDiff$logFC[selectedItems], digits = rv$settings_nDigits),
        P_Value = rv$resAnaDiff$P_Value[selectedItems],
        Adjusted_PValue = Get_adjusted_pvalues()[selectedItems],
        isDifferential = significant)
    
    # Sort data.frame wrt adjusted pvalues
    #t <- t[order(t$Adjusted_PValue, decreasing=FALSE), ]
    # 
    # } else {
    #     t <- data.frame(
    #         id = rownames(Biobase::exprs(rv$current.obj))[selectedItems],
    #         logFC = round(rv$resAnaDiff$logFC[selectedItems], digits = rv$settings_nDigits),
    #         P_Value = rv$resAnaDiff$P_Value[selectedItems],
    #         isDifferential = significant)
    # }
    # 
    tmp <- as.data.frame(
      Biobase::fData(rv$current.obj)[selectedItems, rv$widgets$anaDiff$tooltipInfo]
    )
    names(tmp) <- rv$widgets$anaDiff$tooltipInfo
    t <- cbind(t, tmp)
    
    colnames(t)[2:5] <- paste0(colnames(t)[2:5], " (", as.character(rv$widgets$anaDiff$Comparison), ")")
    
    t
})



isContainedIn <- function(strA, strB) {
    return(all(strA %in% strB))
}
