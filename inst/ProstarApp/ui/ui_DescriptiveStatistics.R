

tabPanel("Descriptive statistics",
         value = "DescriptiveStatisticsTab",
         tagList(
             h3('test')
             #view_dataset_ui('view_dataset')
         )
)
         # tabsetPanel(
         #     id = "DS_tabSetPanel",
         #     tabPanel("Overview",
         #              value = "DS_tabGeneral",
         #              tagList(
         #                  br(),
         #                  format_DT_ui("overview_DS"),
         #                  uiOutput("versionsUI")
         #              )
         #     ),
         #     tabPanel("Quantification type",
         #         value = "DS_tabOverviewMV",
         #         #mod_plotsMetacellHistos_ui("MVPlots_DS")
         #         uiOutput("plotsMVHistograms")
         #     ),
         #     tabPanel(
         #         title = "Data explorer",
         #         value = "DS_DataExplorer",
         #         mod_MSnSetExplorer_ui(id = "test")
         #     ),
         #     tabPanel("Corr. matrix",
         #              value = "DS_tabCorrMatrix",
         #              checkboxInput("showDataLabels", "Show labels", value = FALSE),
         #              uiOutput("plotsCorM")
         #     ),
         #     tabPanel("Heatmap",
         #              value = "DS_tabHeatmap",
         #              uiOutput("plotsHeatmap")
         #     ),
         #     tabPanel("PCA",
         #              value = "DS_PCA",
         #              uiOutput("plotsPCA")
         #     ),
         #     tabPanel("Intensity distr.",
         #              value = "DS_tabDensityplot",
         #              uiOutput("IntensityStatsPlots")
         #     ),
         #     tabPanel("CV distr.",
         #              value = "DS_tabDistVar",
         #              uiOutput("plotsDistCV")
         #     )
         # )
)