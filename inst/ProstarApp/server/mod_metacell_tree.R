css <- "
* {
  margin: 0;
  padding: 0;
  box-sizing: border-box;
}

body {
  padding: 50px;
  font-family: helvetica, arial, sans-serif;
}

ul {
  margin-left: 20px;
  margin-bottom: 30px;
}

.wtree li {
  list-style-type: none;
  margin: 10px 0 5px 10px;
  position: relative;
}
.wtree li:before {
  content: '';
  position: absolute;
  top: -15px;
  left: -25px;
  border-left: 1px solid #ddd;
  border-bottom: 1px solid #ddd;
  width: 20px;
  height: 30px;
}
.wtree li:after {
  position: absolute;
  content: '';
  top: 15px;
  left: -25px;
  border-left: 1px solid #ddd;
  border-top: 1px solid #ddd;
  width: 20px;
  height: 100%;
}
.wtree li:last-child:after {
  display: none;
}
.wtree li span {
  display: inline-block;
  border: 0px solid #ddd;
  border-radius: 10px;
  text-align: center;
  vertical-align: middle;
  padding: 5px;
  color: #888;
  text-decoration: none;
  width: 150px;
}
"


mod_metacell_tree_ui <- function(id) {
    ns <- NS(id)
    
    tagList(
        shinyjs::inlineCSS(css),
        checkboxInput(ns('multiple'), 'Multiple selection', value = FALSE),
        uiOutput(ns('tree'))
    )
    
}

mod_metacell_tree_server <- function(id, level = NULL) {
    
    if(is.null(level))
        stop('level is empty')
    
    
    moduleServer(id,
                 function(input, output, session) {
                     ns <- session$ns
                     
                     convertWidgetName <- function(name){
                         # This function implements the transformations used to
                         # create the names of the checkboxes
                         ll <- lapply(name, function(x){
                             tmp <- gsub('.', '', x, fixed = TRUE)
                             tmp <- gsub(' ', '', tmp, fixed = TRUE)
                             tmp <- tolower(tmp)
                             tmp <- paste0(tmp, '_cb')
                         })
                         
                         return(unlist(ll))
                     }
                     
                     BuildMapping <- function(){
                         mapping <- c()
                         meta <- DAPAR::metacell.def(level)
                         ll <- meta$node
                         .ind <- which(ll == 'Any')
                         ll <- ll[-.ind]
                         colors <- setNames(meta$color[-.ind], nm = convertWidgetName(ll)) 
                         widgets.names <- setNames(ll, nm = convertWidgetName(ll))
                         return(list(names = widgets.names,
                                     colors = colors)
                         )
                     }
                     
                     reverse.mapping <- function(x){
                         req(rv$mapping)
                         
                         return(names(rv$mapping)[which(rv$mapping == x)])
                     }
                     
                     rv <- reactiveValues(
                         tags = NULL,
                         mapping = BuildMapping()$names,
                         bg_colors = BuildMapping()$colors
                     )
                     
                     
                     
                     output$tree <- renderUI({
                         uiOutput(ns(paste0('metacell_tree_', level)))
                     })
                     
                     .style <- 'vertical-align: top; 
                     background: #bg-color#; 
                     color: white; 
                     padding: 5px;'
                     
                     # Define tree for protein dataset
                     output$metacell_tree_protein <- renderUI({
                         
                         
                         div(class='wtree',
                             h1(class="title", 'Cell metadata tags'),
                             tags$ul(
                                 tags$li(
                                     checkboxInput(ns('quantified_cb'), 
                                                   tags$span(style = Get_bg_color('quantified_cb'), 'Quantified')
                                     ),
                                     tags$ul(
                                         tags$li(
                                             checkboxInput(ns('quantbydirectid_cb'), 
                                                           tags$span(style = Get_bg_color('quantbydirectid_cb'), 'Quant. by direct id')
                                             )
                                         ),
                                         tags$li(
                                             checkboxInput(ns('quantbyrecovery_cb'), 
                                                           tags$span(style = Get_bg_color('quantbyrecovery_cb'), 'Quant. by recovery')
                                             )
                                         )
                                     )
                                 ),
                                 
                                 
                                 tags$li(
                                     checkboxInput(ns('missing_cb'), 
                                                   tags$span(style = Get_bg_color('missing_cb'), 'Missing')
                                     ),
                                     tags$ul(
                                         tags$li(
                                             checkboxInput(ns('missingpov_cb'), 
                                                           tags$span(style = Get_bg_color('missingpov_cb'), 'Missing POV')
                                             )
                                         ),
                                         tags$li(
                                             checkboxInput(ns('missingmec_cb'), 
                                                           tags$span(style = Get_bg_color('missingmec_cb'), 'Missing MEC')
                                             )
                                         )
                                     )
                                 ),
                                 
                                 
                                 tags$li(
                                     checkboxInput(ns('imputed_cb'), 
                                                   tags$span(style = Get_bg_color('imputed_cb'), 'Imputed')
                                     ),
                                     tags$ul(
                                         tags$li(
                                             checkboxInput(ns('imputedpov_cb'), 
                                                           tags$span(style = Get_bg_color('imputedpov_cb'), 'Imputed POV')
                                             )
                                         ),
                                         tags$li(
                                             checkboxInput(ns('imputedmec_cb'), 
                                                           tags$span(style = Get_bg_color('imputedmec_cb'), 'Imputed MEC')
                                             )
                                         )
                                     )
                                 ),
                                 
                                 tags$li(
                                     checkboxInput(ns('combinedtags_cb'), 
                                                   tags$span(style = Get_bg_color('combinedtags_cb'), 'Combined tags')
                                     )
                                     # tags$ul(
                                     #     tags$li(
                                     #         checkboxInput(ns('partiallyquantified_cb'), 
                                     #                       tags$span(style = .style, 'Partially quantified')
                                     #         )
                                     #     )
                                     # )
                                 )
                                 
                             )
                         )
                     })
                     
                  
                     output$metacell_tree_peptide <- renderUI({
                         div(class='wtree',
                             h1(class="title", 'Cell metadata tags'),
                             tags$ul(
                                 tags$li(
                                     checkboxInput(ns('quantified_cb'), 
                                                   tags$span(style = Get_bg_color('quantified_cb'), 'Quantified')
                                     ),
                                     tags$ul(
                                         tags$li(
                                             checkboxInput(ns('quantbydirectid_cb'), 
                                                           tags$span(style = Get_bg_color('quantbydirectid_cb'), 'Quant. by direct id')
                                             )
                                         ),
                                         tags$li(
                                             checkboxInput(ns('quantbyrecovery_cb'), 
                                                           tags$span(style = Get_bg_color('quantbyrecovery_cb'), 'Quant. by recovery')
                                             )
                                         )
                                     )
                                 ),
                                 
                                 
                                 tags$li(
                                     checkboxInput(ns('missing_cb'), 
                                                   tags$span(style = Get_bg_color('missing_cb'), 'Missing')
                                     ),
                                     tags$ul(
                                         tags$li(
                                             checkboxInput(ns('missingpov_cb'), 
                                                           tags$span(style = Get_bg_color('missingpov_cb'), 'Missing POV')
                                             )
                                         ),
                                         tags$li(
                                             checkboxInput(ns('missingmec_cb'), 
                                                           tags$span(style = Get_bg_color('missingmec_cb'), 'Missing MEC')
                                             )
                                         )
                                     )
                                 ),
                                 
                                 
                                 tags$li(
                                     checkboxInput(ns('imputed_cb'), 
                                                   tags$span(style = Get_bg_color('imputed_cb'), 'Imputed')
                                     ),
                                     tags$ul(
                                         tags$li(
                                             checkboxInput(ns('imputedpov_cb'), 
                                                           tags$span(style = Get_bg_color('imputedpov_cb'), 'Imputed POV')
                                             )
                                         ),
                                         tags$li(
                                             checkboxInput(ns('imputedmec_cb'), 
                                                           tags$span(style = Get_bg_color('imputedmec_cb'), 'Imputed MEC')
                                             )
                                         )
                                     )
                                 )
                                 
                             )
                         )
                     })
                     
                     
                     Get_bg_color <- function(name){
                         gsub("#bg-color#", rv$bg_colors[name], .style)
                     }
                     
                     update_CB <- function(nametokeep=NULL){
                         if(!is.null(nametokeep))
                            widgets_to_disable <- names(input)[-match(reverse.mapping(nametokeep) , names(input))]
                         
                         lapply(widgets_to_disable,
                                function(x){
                                    updateCheckboxInput(session, x, value = FALSE)
                                    rv$tags[rv$mapping[x]] <- FALSE
                                }
                         )
                         
                     } 
                     
                     
                     observeEvent(input$multiple, {
                         update_CB <- function(nametokeep=NULL){
                             if(!is.null(nametokeep))
                                 widgets_to_disable <- GetTreeCBInputs()[-match(reverse.mapping(nametokeep) , GetTreeCBInputs())]
                             else
                                 widgets_to_disable <- GetTreeCBInputs()
                             
                             lapply(widgets_to_disable,
                                    function(x){
                                        updateCheckboxInput(session, x, value = FALSE)
                                        rv$tags[rv$mapping[x]] <- FALSE
                                    }
                             )
                             
                         }
                         if (!input$multiple)
                             update_CB()
                     })
                     
                     GetTreeCBInputs <- reactive({
                         names(input)[grepl('_cb', names(input))]
                     })
                     
                     observeEvent(lapply(GetTreeCBInputs(), function(x) input[[x]]), ignoreInit = TRUE, {
                         req(!is.null(input$multiple))
                         req(length(GetTreeCBInputs()) > 0)
                         update_CB <- function(nametokeep=NULL){
                             if(!is.null(nametokeep))
                                 widgets_to_disable <- GetTreeCBInputs()[-match(reverse.mapping(nametokeep) , GetTreeCBInputs())]
                             else
                                 widgets_to_disable <- GetTreeCBInputs()
                             
                             lapply(widgets_to_disable,
                                    function(x){
                                        updateCheckboxInput(session, x, value = FALSE)
                                        rv$tags[rv$mapping[x]] <- FALSE
                                    }
                             )
                             
                         }
                         
                         events <- unlist(lapply(GetTreeCBInputs(), function(x) input[[x]]))
                        #browser()
                         if (is.null(rv$tags)){
                             tree_cb_inputs <- GetTreeCBInputs()
                             rv$tags <- setNames(events, nm = gsub('_cb', '', unname(rv$mapping[GetTreeCBInputs()])))
                             newSelection <- names(rv$tags)[which(rv$tags)]
                             return(NULL)
                         }

                         #else {
                         #browser()
                             compare <- rv$tags == events
                             
                             # If nothing has change, quit
                             if (length(which(compare==FALSE))==0){
                                 return(NULL)
                             }
                             newSelection <- names(rv$tags)[which(compare==FALSE)]
                             rv$tags[newSelection] <- input[[reverse.mapping(newSelection)]]

                         #}

                             # Check if the new selection is a node or a leaf
                             # If it is not a node, then it is a leaf as the hierarchy only
                             # contains two levels

                         #browser()
                        
                             is.leaf <- length(DAPAR::Children(level, newSelection)) == 0
                         if (is.leaf){
                             # If the new selection is a leaf if(!multiple)
                             if(!input$multiple)
                                 update_CB(newSelection)
                         } else {
                             # If the new selection is a node:
                             # by default, all its children must be also selected
                             children.names <- DAPAR::Children(level, newSelection)
                             
                             if(input$multiple){
                                 lapply(children.names, function(x){
                                     updateCheckboxInput(session, reverse.mapping(x), value = TRUE)
                                     rv$tags[x] <- TRUE
                                 }
                                     )
                                 } else{
                                     update_CB(newSelection)
                                 }
                         }

                 
                     })
                     
                     
                     
                     
                     return(reactive({names(rv$tags)[which(rv$tags == TRUE)]}))
                     
                 }
    )
    
    
}



# Example

# ui <- mod_metacell_tree_ui('tree')
# 
# server <- function(input, output) {
#     res <- mod_metacell_tree_server('tree', level = 'protein')
#     
#     observe({
#         print(res())
#     })
# }
# 
# shinyApp(ui = ui, server = server)
# 


