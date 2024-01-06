

popover_for_help_server("modulePopover_convertChooseDatafile",
    title = "Data file",
    content = "Select one (.txt, .csv, .tsv, .xls, .xlsx) file."
        )

popover_for_help_server("modulePopover_convertIdType",
    title = "ID definition",
    content = "If you choose the automatic ID, Prostar will build an index."
        )





popover_for_help_server("modulePopover_convertProteinID",
    title = "Select protein IDs",
        content = "Select the column containing the parent protein IDs."
    )


popover_for_help_server("modulePopover_convertDataQuanti",
    title = "Quantitative data",
            content = "Select the columns that are quantitation values
            by clicking in the field below."
        )

format_DT_server("overview_convertData",
    data = reactive({GetDatasetOverview()})
)




## --------------------------------------------------------------
## Gestion du slideshow
## --------------------------------------------------------------


output$checkConvertPanel <- renderUI({
    rv$tab1
    rv$pageConvert
    color <- rep("lightgrey", NUM_PAGES_CONVERT)

    txt <- c(
        "Select file", "Select ID", "Select quantitative data",
        "Build design", "Convert"
    )
    buildTable(txt, color)
})



########### STEP 1 ############
output$Convert_SelectFile <- renderUI({
    tagList(
        wellPanel(
            fluidRow(
            column(width = 3,
                   p('Data source'),
                   radioButtons("choose_software", "",
                                choices = setNames(nm = DAPAR::GetSoftAvailables()),
                                selected = DAPAR::GetSoftAvailables()[2]
                                )),
            column(width = 4,
                   uiOutput("choose_file_to_import"),
                   uiOutput("ManageXlsFiles"),
                   uiOutput("Manage_csv_Files")
                   ),
            column(width = 5,
                   checkboxInput('show_preview', 'show_preview', value = FALSE),
                   uiOutput("previewDataset_UI")
                   )
            )
            ),
        wellPanel(
            p('Options'),
            uiOutput("ConvertOptions")
            ),
        tags$div(style='"align: center;display:inline-block; vertical-align: top;',
                 uiOutput('loadFile_btn_UI')
                 ),
        tags$div(style='"align: center;display:inline-block; vertical-align: top;',
                 uiOutput('info_file_loaded_UI')
        )
    )
})



output$info_file_loaded_UI <- renderUI({
    req(rv$tab1)
    p('The file has been loaded correctly')
})

output$loadFile_btn_UI <- renderUI({
    req(input$file1$name)
    .ext <- GetExtension(input$file1$name)
    cond.csv <-.ext %in% c("txt", "csv", 'tsv')
    cond.xl <- .ext %in% c("xls", "xlsx") && !is.null(input$XLSsheets)
    req(cond.csv || cond.xl)
    actionButton("loadFile", "Load file", class = actionBtnClass)
})

output$choose_file_to_import <- renderUI({
    #req(input$choose_software)
    tagList(
        popover_for_help_ui("modulePopover_convertChooseDatafile"),
        fileInput("file1", "",
              width = '300px',
              multiple = FALSE,
              accept = c(".txt", ".tsv", ".csv", ".xls", ".xlsx")
            )
    )
})


fileExt.ok <- reactive({
    req(input$file1$name)
    authorizedExts <- c("txt", "csv", "tsv", "xls", "xlsx")
    ext <- GetExtension(input$file1$name)
    !is.na(match(ext, authorizedExts))
})

output$ConvertOptions <- renderUI({
    #req(input$choose_software)
    #req(input$file1)
    #req(fileExt.ok())

    tagList(
        fluidRow(
            column(width = 2,
                   radioButtons("typeOfData", "Type of dataset",
            choices = c( "peptide dataset" = "peptide",
                         "protein dataset" = "protein"),
            selected = rv$widgets$Convert$typeOfDataset)),
        column(width = 4,
               radioButtons("checkDataLogged", "Data already log-transformed ?",
                            choices = c("yes (they stay unchanged)" = "yes",
                                        "no (they wil be automatically transformed)" = "no"),
                            selected = rv$widgets$Convert$checkDataLogged)),
        column(width = 2,
               checkboxInput("replaceAllZeros", "Replace all 0 and NaN by NA",
                             value = TRUE)
        ))
    )
})

observeEvent(input$typeOfData, ignoreInit = TRUE, {rv$widgets$Convert$typeOfDataset <- input$typeOfData})
observeEvent(input$checkDataLogged, ignoreInit = TRUE, {rv$widgets$Convert$checkDataLogged <- input$checkDataLogged})






ReadDatasetFile <- reactive({
    req(input$file1)
    
    tab1 <- NULL
    #browser()
    authorizedExts <- c("txt", "csv", "tsv", "xls", "xlsx")
    if (!fileExt.ok()) {
        shinyjs::info("Warning : this file is not a text nor an Excel file !
     Please choose another one.")
    } else {
        tryCatch({
            
            ext <- GetExtension(input$file1$name)
            shinyjs::disable("file1")
            
            if (ext %in% c('txt', 'csv', 'tsv')){
                req(input$csv_separator)
                
                .sep <- input$csv_separator
                if (.sep == 'tab')
                    .sep <- '\t'
                
                tab1 <- read.csv(input$file1$datapath, 
                                 header = TRUE, 
                                 sep = .sep, 
                                 as.is = T)
            }
            else if (ext %in% c('xls', 'xlsx')){
                req(input$XLSsheets)
                tab1 <- DAPAR::readExcel(input$file1$datapath, 
                                         sheet = input$XLSsheets)
            }
            
            colnames(tab1) <- gsub(".", "_", colnames(tab1), fixed = TRUE)
            colnames(tab1) <- gsub(" ", "_", colnames(tab1), fixed = TRUE)
        },
        warning = function(w) {
            #shinyjs::info(conditionMessage(w))
            tab1 <- NULL
        },
        error = function(e) {
            #shinyjs::info(conditionMessage(e))
            tab1 <- NULL
        },
        finally = {
            # cleanup-code
        })
    }
    
    tab1
})



output$previewDataset_UI <- renderUI({
    req(ReadDatasetFile())
    req(input$show_preview)
    
    tagList(
        p(style = "color: black;", "Preview"),
        DT::DTOutput("previewDataset", width = '100%')
    )
})

output$previewDataset <- DT::renderDT(server = TRUE, {
    req(ReadDatasetFile())
    DT::datatable(as.data.frame(ReadDatasetFile()[1:3,]),
                  rownames = FALSE,
                  plugins = "ellipsis",
                  extensions = c("Scroller", "FixedColumns"),
                  options = list(
                      dom = "Brt",
                      scrollX = TRUE,
                      ordering = FALSE
                  )
                  )
})



observeEvent(input$show_preview, {
    print("preload")
    #browser()
    preview.dataset <- ReadDatasetFile()
    
    print(colnames(preview.dataset))
})


############ Read text file to be imported ######################
observeEvent(input$loadFile, {
rv$tab1 <- ReadDatasetFile()
})



output$ManageXlsFiles <- renderUI({
    req(input$choose_software)
    req(input$file1)

    req(GetExtension(input$file1$name) %in% c("xls", "xlsx"))
     
    tryCatch({   
        sheets <- listSheets(input$file1$datapath)
        selectInput("XLSsheets", "sheets", choices = as.list(sheets), width = "200px")
    },
    warning = function(w) {
        shinyjs::info(conditionMessage(w))
        return(NULL)
    },
    error = function(e) {
        shinyjs::info(conditionMessage(e))
        return(NULL)
    },
    finally = {
        # cleanup-code
    }
    )
})





output$Manage_csv_Files <- renderUI({
    req(input$file1)
    
    req(GetExtension(input$file1$name) %in% c("csv"))
    
    selectInput('csv_separator', 'Separator', 
                choices = c(';' = ';',
                            'tab' = 'tab',
                            '.' = '.'),
                width = '100px')
})


################## STEP 2 ###############################

output$Convert_DataId <- renderUI({
    tagList(
        br(), br(),
        tags$div(
            tags$div(
                style = "display:inline-block; vertical-align: top;
        padding-right: 100px;",
                uiOutput("id"),
                uiOutput("warningNonUniqueID")
            ),
            tags$div(
                style = "display:inline-block; vertical-align: top;",
                uiOutput("convertChooseProteinID_UI"),
                uiOutput("previewProteinID_UI")
            )
        )
    )
})


output$id <- renderUI({
    req(rv$tab1)

    .choices <- c("AutoID", colnames(rv$tab1))
    names(.choices) <- c("Auto ID", colnames(rv$tab1))

    tagList(
        popover_for_help_ui("modulePopover_convertIdType"),
        selectInput("colnameForID", label = "", choices = .choices)
    )
})


output$warningNonUniqueID <- renderUI({
    req(input$colnameForID != "AutoID")
    req(rv$tab1)

    t <- (length(as.data.frame(rv$tab1)[, input$colnameForID])
    == length(unique(as.data.frame(rv$tab1)[, input$colnameForID])))

    if (!t) {
        text <- "<img src=\"images/Problem.png\" height=\"24\"></img>
    <font color=\"red\">
        Warning ! Your ID contains duplicate data.
        Please choose another one."
    } else {
        text <- "<img src=\"images/Ok.png\" height=\"24\"></img>"
    }
    HTML(text)
})


output$convertChooseProteinID_UI <- renderUI({
    req(rv$tab1)
    req(rv$widgets$Convert$typeOfDataset != "protein")

    .choices <- c("", colnames(rv$tab1))
    names(.choices) <- c("", colnames(rv$tab1))
    tagList(
        popover_for_help_ui("modulePopover_convertProteinID"),
        selectInput("convert_proteinId",
            "",
            choices = .choices, selected = character(0)
        )
    )
})




output$helpTextDataID <- renderUI({
    rv$widgets$Convert$typeOfDataset
    if (is.null(rv$widgets$Convert$typeOfDataset)) {
        return(NULL)
    }
    t <- ""
    switch(rv$widgets$Convert$typeOfDataset,
        protein = {
            t <- "proteins"
        },
        peptide = {
            t <- "peptides"
        }
    )
    txt <- paste("Please select among the columns of your data the one that
                corresponds to a unique ID of the ", t, ".", sep = " ")
    helpText(txt)
})



datasetID_Ok <- reactive({
    req(input$colnameForID)
    req(rv$tab1)
    if (input$colnameForID == "AutoID") {
        t <- TRUE
    } else {
        t <- (length(as.data.frame(rv$tab1)[, input$colnameForID])
        == length(unique(as.data.frame(rv$tab1)[, input$colnameForID])))
    }
    t
})



output$previewProteinID_UI <- renderUI({
    req(input$convert_proteinId != "")

    tagList(
        p(style = "color: black;", "Preview"),
        tableOutput("previewProtID")
    )
})




output$previewProtID <- renderTable(
    # req(input$convert_proteinId),
    head(rv$tab1[, input$convert_proteinId]),
    colnames = FALSE
)






output$Convert_ExpFeatData <- renderUI({
    tagList(
        shinyjs::useShinyjs(),
        fluidRow(
            column(
                width = 4,
                radioButtons("selectIdent", "Provide identification method",
                    choices = list(
                        "No (default values will be computed)" = FALSE,
                        "Yes" = TRUE
                    ),
                    selected = FALSE
                )
            ),
            column(width = 4, uiOutput("checkIdentificationTab")),
            column(width = 4, shinyjs::hidden(
                div(
                    id = "warning_neg_values",
                    p(
                        "Warning : Your original dataset may contain
                      negative values",
                        "so that they cannot be logged. Please check
                      back the dataset or",
                        "the log option in the first tab."
                    )
                )
            ))
        ),
        fluidRow(
            column(width = 4, uiOutput("eData", width = "400px")),
            column(width = 8, shinyjs::hidden(
                uiOutput("inputGroup", width = "600px")
            ))
        )
    )
})


output$inputGroup <- renderUI({
    # if (is.null(input$choose_quantitative_columns) || is.null(rv$tab1))
    #  return(NULL)

    n <- length(input$choose_quantitative_columns)

    input_list <- lapply(seq_len(n), function(i) {
        inputName <- paste("colForOriginValue_", i, sep = "")
        div(
            div(
                style = "align: center;display:inline-block; vertical-align:
          middle;padding-right: 10px;",
                p(tags$strong(paste0(
                    "Identification col. for ",
                    input$choose_quantitative_columns[i]
                )))
            ),
            div(
                style = "align: center;display:inline-block;
              vertical-align: middle;",
                selectInput(inputName, "",
                    choices = c("None", colnames(rv$tab1))
                )
            )
        )
    })
    do.call(tagList, input_list)
})


observeEvent(input[["colForOriginValue_1"]], ignoreInit = T, ignoreNULL = F, {
    n <- length(input$choose_quantitative_columns)
    lapply(seq(2, n), function(i) {
        inputName <- paste("colForOriginValue_", i, sep = "")
        start <- which(colnames(rv$tab1) == input[["colForOriginValue_1"]])

        if (input[["colForOriginValue_1"]] == "None") {
            .select <- "None"
        } else {
            .select <- colnames(rv$tab1)[(i - 1) + start]
        }
        updateSelectInput(session, inputName, selected = .select)
    })
})

observe({
    shinyjs::toggle("warning_neg_values",
        condition = !is.null(input$choose_quantitative_columns) &&
            length(which(rv$tab1[, input$choose_quantitative_columns] < 0)) > 0
    )
    shinyjs::toggle("selectIdent",
        condition = !is.null(input$choose_quantitative_columns)
    )
    shinyjs::toggle("inputGroup",
        condition = as.logical(input$selectIdent) == TRUE
    )
})

output$eData <- renderUI({
    input$file1
    req(rv$tab1)

    choices <- colnames(rv$tab1)
    names(choices) <- colnames(rv$tab1)

    tagList(
        popover_for_help_ui("modulePopover_convertDataQuanti"),
        selectInput("choose_quantitative_columns",
            label = "",
            choices = choices,
            multiple = TRUE, width = "200px",
            size = 20,
            selectize = FALSE
        )
    )
})



output$checkIdentificationTab <- renderUI({
    req(as.logical(input$selectIdent) == TRUE)

    shinyValue("colForOriginValue_", length(input$choose_quantitative_columns))
    temp <- shinyValue(
        "colForOriginValue_",
        length(input$choose_quantitative_columns)
    )

    # if ((length(which(temp == "None")) == length(temp)))
    # {
    #   img <- "images/Ok.png"
    #   txt <- "Correct"
    # }  else {

    if (length(which(temp == "None")) > 0) {
        img <- "images/Problem.png"
        txt <- "The identification method is not appropriately defined for
      each sample."
    } else {
        if (length(temp) != length(unique(temp))) {
            img <- "images/Problem.png"
            txt <- "There are duplicates in identification columns."
        } else {
            img <- "images/Ok.png"
            txt <- "Correct"
        }
    }
    # }
    tags$div(
        tags$div(
            tags$div(
                style = "display:inline-block;",
                tags$img(
                    src = img,
                    height = 25
                )
            ),
            tags$div(style = "display:inline-block;", tags$p(txt))
        )
    )
})






checkIdentificationMethod_Ok <- reactive({
    res <- TRUE
    tmp <- NULL
    if (isTRUE(as.logical(input$selectIdent))) {
        tmp <- shinyValue("colForOriginValue_", nrow(quantiDataTable()))
        if ((length(grep("None", tmp)) > 0) || (sum(is.na(tmp)) > 0)) {
            res <- FALSE
        }
    }
    res
})





############# STEP 4 ######################

output$Convert_BuildDesign <- renderUI({
    req(input$file1)
    tagList(
        tags$p(
            "If you do not know how to fill the experimental design, you can
            click on the '?' next to each design in the list that appear
            once the conditions are checked or got to the ",
            actionLink("linkToFaq1", "FAQ",
                style = "background-color: white"
            ),
            " page."
        ),
        fluidRow(
            column(width = 6,
            tags$b("1 - Fill the \"Condition\" column to identify the conditions to compare.")
            ),
            column(width = 6,
                   uiOutput("UI_checkConditions")
            )
        ),
        fluidRow(
            column(width = 6, uiOutput("UI_hierarchicalExp")),
            column(width = 6, uiOutput("checkDesign"))
        ),
        hr(),
        selectInput("convert_reorder", "Order by conditions ?",
            choices = c("No" = "No", "Yes" = "Yes"),
            width = "100px"
        ),
        tags$div(
            tags$div(style = "display:inline-block; vertical-align: top;",
                uiOutput("viewDesign", width = "100%")
            ),
            tags$div(style = "display:inline-block; vertical-align: top;",
                shinyjs::hidden(div(
                    id = "showExamples",
                    uiOutput("designExamples")
                ))
            )
        )
    )
})



############# STEP 5 ########################


output$Convert_Convert <- renderUI({
    tagList(
        br(), br(),
        uiOutput("convertFinalStep"),
        format_DT_ui("overview_convertData"),
        uiOutput("conversionDone"),
        p("Once the 'Load' button (above) clicked, you will be automatically
    redirected to Prostar home page. The dataset will be accessible within
    Prostar
    interface and processing menus will be enabled. However, all importing
    functions ('Open MSnset', 'Demo data' and 'Convert data') will be disabled
    (because successive dataset loading can make Prostar unstable). To work
    on another dataset, use first the 'Reload Prostar' functionality from
    the 'Dataset manager' menu: it will make Prostar restart with a fresh R
      session where import functions are enabled.")
    )
})



output$convertFinalStep <- renderUI({
    req(rv$designChecked)
    if (!(rv$designChecked$valid)) {
        return(NULL)
    }
    tagList(
        uiOutput("checkAll_convert", width = "50"),
        htmlOutput("msgAlertCreateMSnset"),
        hr(),
        textInput("filenameToCreate", "Enter the name of the study"),
        actionButton("createMSnsetButton", "Convert data",
            class = actionBtnClass
        ),
        uiOutput("warningCreateMSnset")
    )
})


output$conversionDone <- renderUI({
    req(rv$current.obj)

    h4("The conversion is done. Your dataset has been automatically loaded
       in memory. Now, you can switch to the Descriptive statistics panel to
       vizualize your data.")
})



output$warningCreateMSnset <- renderUI({
    if (isTRUE(as.logical(input$selectIdent))) {
        n <- length(input$choose_quantitative_columns)

        colNamesForMetacell <- unlist(lapply(seq_len(n), function(x) {
            input[[paste0("colForOriginValue_", x)]]
        }))

        if (length(which(colNamesForMetacell == "None")) > 0) {
            text <- "<font color=\"red\"> Warning: The MSnset cannot be created
      because the identification
            method are not fully filled.  <br>"
            HTML(text)
        }
    }
})



#######################################
observeEvent(input$createMSnsetButton, ignoreInit = TRUE, {
    
    colNamesForMetacell <- NULL
    if (isTRUE(as.logical(input$selectIdent))) {
        n <- length(input$choose_quantitative_columns)

        colNamesForMetacell <- unlist(lapply(seq_len(n), function(x) {
            input[[paste0("colForOriginValue_", x)]]}))
        if (length(which(colNamesForMetacell == "None")) > 0) {
            return(NULL)
        }
        if (!is.null(rv$newOrder)) {
            colNamesForMetacell <- colNamesForMetacell[rv$newOrder]
        }
    }

    isolate({
        result <- try({
                ext <- GetExtension(input$file1$name)
                txtTab <- paste("tab1 <- read.csv(\"", input$file1$name,
                    "\",header=TRUE, sep=\"\t\", as.is=T)",
                    sep = ""
                )
                txtXls <- paste("tab1 <- read.xlsx(", input$file1$name,
                    ",sheet=", input$XLSsheets, ")",
                    sep = ""
                )
                switch(ext,
                    txt = writeToCommandLogFile(txtTab),
                    csv = writeToCommandLogFile(txtTab),
                    tsv = writeToCommandLogFile(txtTab),
                    xls = writeToCommandLogFile(txtXls),
                    xlsx = writeToCommandLogFile(txtXls)
                )

                metadata <- hot_to_r(input$hot)
                #logData <- (rv$widgets$Convert$checkDataLogged== "no")
                
                
                input$filenameToCreate
                rv$tab1
                
               # browser()
                indexForEData <- match(rv$hot[, 'Sample.name'], colnames(rv$tab1))
                # .chooseCols <- input$choose_quantitative_columns
                # indexForEData <- match(.chooseCols,colnames(rv$tab1))
                # if (!is.null(rv$newOrder)) {
                #     indexForEData <- rv$newOrder
                # }

                indexForFData <- seq(1, ncol(rv$tab1))[-indexForEData]



                
                indexForMetacell <- NULL
                if (!is.null(colNamesForMetacell) &&
                    (length(grep("None", colNamesForMetacell)) == 0) &&
                    (sum(is.na(colNamesForMetacell)) == 0)) {
                    indexForMetacell <- match(
                        colNamesForMetacell,
                        colnames(rv$tab1)
                    )
                }

                options(digits = 15)

                protId <- NULL
                if (rv$widgets$Convert$typeOfDataset == "protein") {
                    protId <- input$colnameForID
                } else if (rv$widgets$Convert$typeOfDataset == "peptide") {
                    protId <- input$convert_proteinId
                }

                
                #browser()
                tmp <- DAPAR::createMSnset(
                    file = rv$tab1,
                    metadata = metadata,
                    indExpData = indexForEData,
                    colnameForID = input$colnameForID,
                    indexForMetacell = indexForMetacell,
                    logData = (rv$widgets$Convert$checkDataLogged== "no"),
                    replaceZeros = input$replaceAllZeros,
                    pep_prot_data = rv$widgets$Convert$typeOfDataset,
                    proteinId = gsub(".", "_", protId, fixed = TRUE),
                    software = input$choose_software
                )
                ClearUI()
                ClearMemory()
                rv$current.obj <- tmp

                rv$current.obj.name <- input$filenameToCreate
                rv$indexNA <- which(is.na(Biobase::exprs(rv$current.obj)))

                l.params <- list(filename = input$filenameToCreate)

                loadObjectInMemoryFromConverter()

                updateTabsetPanel(session, "tabImport", selected = "Convert")
                
                rv$current.obj
            })
        
        if(inherits(result, "try-error")) {
             mod_SweetAlert_server('sweetAlert_convert',
                                  text = result[[1]],
                                  showClipBtn = TRUE,
                                  type = 'error')
        } else {
          # sendSweetAlert(
          #   session = session,
          #   title = "Success",
          #   type = "success"
          # )
        }
        

    })
    
    
    observe({
        req(Check_Dataset_Validity(rv$current.obj))
        mod_SweetAlert_server('sweetAlert_Check_Dataset_Validity',
                              text = Check_Dataset_Validity(rv$current.obj),
                              showClipBtn = FALSE,
                              type = 'warning')
        
    })
})
