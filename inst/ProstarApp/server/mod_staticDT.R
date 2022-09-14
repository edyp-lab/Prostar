


mod_staticDT_ui <- function(id) {
    ns <- NS(id)
    tagList(
        useShinyjs(),
        shinyjs::hidden(
            div(
                id = ns("dl_div"),
                mod_download_btns_ui(ns("DL_btns"))
            )
        ),
        fluidRow(
            column(
                align = "center",
                width = 12,
                DT::dataTableOutput(ns("StaticDataTable"))
            )
        )
    )
}




mod_staticDT_server <- function(id,
                                data,
                                withDLBtns = TRUE,
                                showRownames = FALSE,
                                dom = "t",
                                filename = "Prostar_export") {
    moduleServer(
        id,
        function(input, output, session) {
            proxy <- DT::dataTableProxy(session$ns("StaticDataTable"), session)


            observe({
                DT::replaceData(proxy, data(), resetPaging = FALSE)
            })


            observe({
                shinyjs::toggle("dl_div", condition = isTRUE(withDLBtns))
            })


            mod_download_btns_server(
                id = "DL_btns",
                df.data = reactive({
                    data()
                }),
                name = reactive({
                    filename
                }),
                colors = reactive({
                    NULL
                }),
                df.tags = reactive({
                    NULL
                })
            )

            output$StaticDataTable <- DT::renderDataTable(server = TRUE, {
                req(length(data) > 0)
                .jscode <- JS("$.fn.dataTable.render.ellipsis( 30 )")
                DT::datatable(data(),
                    escape = FALSE,
                    rownames = FALSE,
                    plugins = "ellipsis",
                    options = list(
                        # initComplete = initComplete(),
                        dom = dom,
                        #    server = FALSE,
                        #    autoWidth=TRUE,
                        columnDefs = list(
                            list(
                                className = "dt-center",
                                # width='150px',
                                targets = "_all",
                                render = .jscode
                            )
                        )
                        # ordering = FALSE
                    )
                )
            })
        }
    )
}
