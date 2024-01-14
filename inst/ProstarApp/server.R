library(shiny)
library(shinyjs)
library(shinyBS)
library(shinyWidgets)
rm(list = ls())

options(
    shiny.maxRequestSize = 300 * 1024^2,
    encoding = "UTF-8",
    shiny.fullstacktrace = TRUE
    )

require(compiler)
enableJIT(3)

source(system.file("ProstarApp/ui", "ui_Configure.R", package = 'Prostar'), local = TRUE)$value
source(system.file("ProstarApp/server", "mod_errorModal.R", package = 'Prostar'), local = TRUE)$value
source(system.file("ProstarApp/server", "mod_metacell_tree.R", package = 'Prostar'), local = TRUE)$value
source(system.file("ProstarApp/server", "mod_insert_md.R", package = 'Prostar'), local = TRUE)$value
source(system.file("ProstarApp/server", "mod_popover_for_help.R", package = 'Prostar'), local = TRUE)$value
source(system.file("ProstarApp/server", "mod_SweetAlert.R", package = 'Prostar'), local = TRUE)$value

# Initialize data with colnames
df <- data.frame(matrix(c("0", "0"), 1, 2))
colnames(df) <- c("Input1", "Input2")

onStart <- function() {
    cat("Doing application setup\n")

    onStop(function() {
        cat("Doing application cleanup\n")
        graphics.off()
        unlink(sessionID, recursive = TRUE)
        unlink(paste(tempdir(), sessionID, commandLogFile, sep = "/"), recursive = TRUE)
        unlink(paste(tempdir(), sep = "/"), recursive = TRUE)
        unlink(paste(tempdir(), "*", sep = "/"), recursive = TRUE)
        unlink(paste(tempdir(), "*html", sep = "/"))
        unlink(paste(tempdir(), "*log", sep = "/"))
        unlink("www/*pdf")
    })
}



shinyServer( 
    function(input, output, session) {

    # Sys.setlocale("LC_ALL","English")
    # Sys.setlocale("LC_ALL", 'en_GB.UTF-8')
    # Sys.setlocale("LC_ALL", 'fr_FR.UTF-8')
    # Sys.setenv(LANG = "fr")
     sink()
     sink()
    #  logfilename <- tempfile(fileext=".log")
    #  print(logfilename)
      showErrLog <- TRUE
    #showErrLog <- FALSE
    
    # Redirect both message() and error outputs to a file
      #if (showErrLog){
      #    con <- file(logfilename, open="wt")
      #    sink(con, append=TRUE, type="message")
          #sink(con, append=TRUE, type="output")
      #}
    
    #message(paste0('message : tempdir() = ', tempdir()))
    #warning("A warning from warning()")


     tryCatch({
         loadLibraries()
         
         Sys.setenv("R_ZIPCMD" = Sys.which("zip"))
         sessionID <- Sys.getpid()
    
    
     # Hide the loading message when the rest of the server function has executed
    
         env <- environment()
          source(system.file("ProstarApp/server", "mod_format_DT.R", package = 'Prostar'), local = TRUE)$value
         
         source(system.file("ProstarApp/server", "mod_dl.R", package = 'Prostar'), local = TRUE)$value
         source(system.file("ProstarApp/server", "mod_insert_md.R", package = 'Prostar'), local = TRUE)$value
         source(system.file("ProstarApp/server", "mod_popover_for_help.R", package = 'Prostar'), local = TRUE)$value
         source(system.file("ProstarApp/server", "mod_download_btns.R", package = 'Prostar'), local = TRUE)$value
         source(system.file("ProstarApp/modules/Plots", "mod_MSnSetExplorer.R", package = 'Prostar'), local = TRUE)$value
         source(system.file("ProstarApp/server", "mod_LegendColoredExprs.R", package = 'Prostar'), local = TRUE)$value
    
         source(system.file("ProstarApp/server", "srv_NavbarPage.R", package = 'Prostar'), local = TRUE)$value
         source(system.file("ProstarApp/server", "srv_ModulesSrv.R", package = 'Prostar'), local = TRUE)$value
         source(system.file("ProstarApp/server", "srv_ModuleProcess.R", package = 'Prostar'), local = TRUE)$value
         source(system.file("ProstarApp/server", "srv_General.R", package = 'Prostar'), local = TRUE)$value
         source(system.file("ProstarApp/server", "srv_DefineRVmoduleProcess.R", package = 'Prostar'), local = TRUE)$value
         source(system.file("ProstarApp/server", "srv_Home.R", package = 'Prostar'), local = TRUE)$value
         source(system.file("ProstarApp/server", "srv_Settings.R", package = 'Prostar'), local = TRUE)$value
         source(system.file("ProstarApp/server", "srv_ParamsManager.R", package = 'Prostar'), local = TRUE)$value
    
         # source(file.path(".", "modules/Plots/modulePlots.R"),  local = TRUE)$value
         source(system.file("ProstarApp/modules/Plots/moduleCC.R", package = 'Prostar'), local = TRUE)$value
         #stop('simulated error')
         
         #loadSourceFiles()
         },
     error = function(e) {
         #if(showErrLog)
         #     shinyjs::info(conditionMessage(e))
         # return(NULL)
         #    mod_errorModal_server("test_error",
         #        reactive({readLines(logfilename)})
         #)
         print(e)
         return(NULL)
     })
      
      observeEvent(input$distance, {
      rv$PlotParams$heatmap.distance <- input$distance
      })
      
      
     observeEvent(input$linkage, {rv$PlotParams$heatmap.linkage <- input$linkage})

     output$datasetAbsPanel <- renderUI({
         req(rv$current.obj.name)
         
         if (is.null(rv$current.obj.name))
             .title <- "No dataset"
         else
             .title <- rv$current.obj.name

         popover_for_help_server("modulePopover_dataset",
                                 title = .title,
                                 content = "Before each processing step, a backup of the current
                                 dataset is stored. It is possible to reload one of them at any time."
         )
         
         div(
             div(style = "display:inline-block; vertical-align: middle;",
                 popover_for_help_ui("modulePopover_dataset")
             ),
             div(style = "display:inline-block; vertical-align: middle; z-index: 1000;",
                 selectInput("datasets", "",
                             choices = list("None" = "None"),
                             width = "200px"
                 )
             )
         )
         
         
     })

     observe({
        req(input$navPage)
         cond <- !(input$navPage %in% c("graphTab", "bugReportTab", "checkForUpdatesTab", "faqTab"))
        shinyjs::toggle("tete", condition = cond)
    
        tryCatch({
        switch(input$navPage,
            DescriptiveStatisticsTab = {
                source(file.path("server", "mod_plots_metacell_histo.R"), local = TRUE)$value
                source(file.path("server", "srv_DescriptiveStats.R"), local = TRUE)$value
            },
            openMSnsetTab = {
                source(file.path("server", "srv_OpenMSnset.R"),local = TRUE)$value
            },
            demoTab =
                source(file.path("server", "srv_DemoMode.R"),local = TRUE)$value,
            convertTab = {
                source(file.path("server", "srv_ConvertData.R"),local = TRUE)$value
                source(file.path("server", "srv_BuildDesign.R"),local = TRUE)$value
            },
            ExportTab = {
                source(file.path("server", "srv_Export.R"),local = TRUE)$value
                source(file.path("server", "srv_SaveGraphics.R"),local = TRUE)$value
            },
            ReloadTab = {
                source(file.path("server", "srv_ReloadProstar.R"),local = TRUE)$value
            },
            FilteringTab = {
                source(file.path("server", "mod_plots_metacell_histo.R"),local = TRUE)$value
                source(file.path("server", "mod_filtering_example.R"),local = TRUE)$value
                source(file.path("server", "srv_Filtering.R"),local = TRUE)$value
                source(file.path("server", "mod_filtering_example.R"),local = TRUE)$value
            },
            NormalizationTab = {
                source(file.path("server", "mod_plots_tracking.R"), local = TRUE)$value
                source(file.path("server", "mod_plots_intensity.R"), local = TRUE)$value
                source(file.path("server", "srv_Normalization.R"), local = TRUE)$value
            },
            imputationProteinLevelTabs = {
                source(file.path("server", "srv_Imputation_ProteinLevel.R"), local = TRUE)$value
            },
            imputationPeptideLevelTabs = {
                source(file.path("server", "srv_Imputation_PeptideLevel.R"), local = TRUE)$value
            },
            AggregationTab =
                source(file.path("server", "srv_Aggregation.R"), local = TRUE)$value,
            diffAnalysisTab = {
                source(file.path("server", "srv_AnaDiff.R"), local = TRUE)$value
            },
            graphTab = {
                mod_cc_server("CC_Multi_Any",
                    obj = reactive({rv$current.obj}),
                    cc = reactive({GetCC(rv$current.obj)$allPep})
                )
            },
            GoTab =
                source(file.path("server", "srv_GO_enrichment.R"), local = TRUE)$value,

            # updateDesignTab =
            #   source(file.path("server", "srv_UpdateDesign.R"),
            #   local = TRUE)$value,
            #
            faqTab = source(file.path("server", "srv_FAQ.R"), local = TRUE)$value,
            checkForUpdatesTab = source(file.path("server", "srv_CheckForUpdates.R"), local = TRUE)$value,
            usefulLinksTab = source(file.path("server", "srv_UsefulLinks.R"), local = TRUE)$value,
            ReleaseNotesTab = source(file.path("server", "srv_ReleaseNotes.R"), local = TRUE)$value,
            bugReportTab = source(file.path("server", "srv_BugReport.R"), local = TRUE)$value,
            testTab = source(file.path("server", "srv_HypothesisTest.R"), local = TRUE)$value
        )
        },
            error = function(e) {
                # shinyjs::info(conditionMessage(e))
                # return(NULL)
                # if(showErrLog)
                #     mod_errorModal_server("err2",
                #         reactive({readLines(logfilename)})
                #     )
                print(e)
                return(NULL)
            })

    })
    

    shinyjs::hide(id = "loading_page", anim = FALSE)
    shinyjs::show("main_content", anim = TRUE, animType = "fade")

})
