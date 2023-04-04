#' @title   format_DT_ui and format_DT_server
#' 
#' @description  A shiny Module.
#' 
#' @name format_DT
#'
#' @keywords internal
#' @example examples/test_format_DT.R
#' 
NULL



#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname format_DT
#' @export
#'  
#' @importFrom shiny NS tagList 
#' @importFrom DT dataTableOutput
#' 
format_DT_ui <- function(id){
    ns <- NS(id)
    tagList(
        useShinyjs(),
        shinyjs::hidden(
            div(id = ns("dl_div"),
                dl_ui(ns("DL_btns"))
            )
        ),
        fluidRow(
            column(
                # align = "center",
                width = 12,
                DT::dataTableOutput(ns("StaticDataTable"))
            )
        )
    )
}

# Module Server

#' @rdname format_DT
#' 
#' @param input internal
#' @param output internal
#' @param session internal
#' @param withBtns xxx
#' @param showRownames xxxx
#' @param dom xxx
#' @param style A list of four items:
#' * cols: a vector of colnames of columns to show,
#' * vals: a vector of colnames of columns that contain values,
#' * unique: unique(conds),
#' * pal: RColorBrewer::brewer.pal(3, "Dark2")[seq_len(2)]
#' @param filename xxx
#' 
#' @export
#' 
#' @keywords internal
#' 
#' @import DT
#' @importFrom htmlwidgets JS    
#' 
format_DT_server <- function(id,
                             data,
                             withDLBtns = FALSE,
                             showRownames = FALSE,
                             dom = 'Bt',
                             hc_style = reactive({NULL}),
                             full_style = reactive({NULL}),
                             filename = "Prostar_export",
                             hideCols = reactive({NULL})
){
    
    
    moduleServer(id, function(input, output, session){
        ns <- session$ns
        
        proxy = DT::dataTableProxy(session$ns('StaticDataTable'), session)
        
        observe({
            req(data())
            DT::replaceData(proxy, data(), resetPaging = FALSE)
        })
        
        observe({
            shinyjs::toggle("dl_div", condition = isTRUE(withDLBtns))
        })
        
        dl_server(
            id = "DL_btns",
            dataIn = reactive({data()[,-hideCols()]}),
            name = reactive({filename}),
            excel.style = reactive({full_style()})
        )
        
        
        output$StaticDataTable <- DT::renderDataTable(server=TRUE,{
            
            req(length(data()) > 0)
            .jscode <- JS("$.fn.dataTable.render.ellipsis( 30 )")
            #isolate({
            dt <- DT::datatable(
                data(), 
                escape = FALSE,
                rownames= showRownames,
                plugins = "ellipsis",
                options = list(
                    #initComplete = initComplete(),
                    dom = dom,
                    autoWidth = TRUE,
                    columnDefs = list(
                        list(targets = '_all', className = "dt-center", render = .jscode)
                        ,list(targets = hideCols()-1, visible = FALSE)
                    )
                    #ordering = FALSE
                )
            )
            
            if (!is.null(hc_style())){
                dt <- dt %>%
                    DT::formatStyle(
                        columns = hc_style()$cols,
                        valueColumns = hc_style()$vals,
                        backgroundColor = DT::styleEqual(hc_style()$unique, hc_style()$pal)
                    )
            }
            #})
            
            dt
            
        })
        
        initComplete <- function(){
            return (htmlwidgets::JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': 'darkgrey', 'color': 'black'});",
                "}"))
        }
        
        
    })
    
}