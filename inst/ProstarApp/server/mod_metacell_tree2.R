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
        uiOutput(ns('metacell_tree'))
    )
    
}

mod_metacell_tree_server <- function(id, 
                                     level = NULL, 
                                     multiple = reactive({FALSE})
                                         ) {
    
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
                         ll <- metacell.def(level)$node
                         ll <- ll[-which(ll == 'Any')]
                         widgets.names <- setNames(ll, nm = convertWidgetName(ll))
                         return(widgets.names)
                     }
                     
                     reverse.mapping <- function(x){
                         req(rv$mapping)
                         
                         return(names(rv$mapping)[which(rv$mapping == x)])
                     }
                     
                     rv <- reactiveValues(
                         tags = NULL,
                         mapping = BuildMapping()
                     )
                     
                     output$metacell_tree <- renderUI({
                         div(class='wtree',
                             h1(class="title", 'Cell metadata tags'),
                             tags$ul(
                                 tags$li(
                                     checkboxInput(ns('quantified_cb'), 
                                                   tags$span(
                                                       style = 'width: 100px; padding: 0px; vertical-align: top; background: #0A31D0; color: white; padding: 5px;',
                                                       'Quantified')
                                     ),
                                     tags$ul(
                                         tags$li(
                                             checkboxInput(ns('quantbydirectid_cb'), 
                                                           tags$span(
                                                               style = 'vertical-align: top; background: #6178D9; color: white; padding: 5px;',
                                                               'Quant. by direct id')
                                             )
                                         ),
                                         tags$li(
                                             checkboxInput(ns('quantbyrecovery_cb'), 
                                                           tags$span(
                                                               style = 'vertical-align: top; background: #B9C4F2; color: white; padding: 5px;',
                                                               'Quant. by recovery')
                                             )
                                         )
                                     )
                                 ),
                                 
                                 
                                 tags$li(
                                     checkboxInput(ns('missing_cb'), 
                                                   tags$span(
                                                       style = 'vertical-align: top; background: #CF8205; color: white; padding: 5px;',
                                                       'Missing')
                                     ),
                                     tags$ul(
                                         tags$li(
                                             checkboxInput(ns('missingpov_cb'), 
                                                           tags$span(
                                                               style = 'vertical-align: top; background: #E5A947; color: white; padding: 5px;',
                                                               'Missing POV')
                                             )
                                         ),
                                         tags$li(
                                             checkboxInput(ns('missingmec_cb'), 
                                                           tags$span(
                                                               style = 'vertical-align: top; background: #F1CA8A; color: white; padding: 5px;',
                                                               'Missing MEC')
                                             )
                                         )
                                     )
                                 ),
                                 
                                 
                                 tags$li(
                                     checkboxInput(ns('imputed_cb'), 
                                                   tags$span(
                                                       style = 'vertical-align: top; background: #A40C0C; color: white; padding: 5px;',
                                                       'Imputed')
                                     ),
                                     tags$ul(
                                         tags$li(
                                             checkboxInput(ns('imputedpov_cb'), 
                                                           tags$span(
                                                               style = 'vertical-align: top; background: #E34343; color: white; padding: 5px;',
                                                               'Imputed POV')
                                             )
                                         ),
                                         tags$li(
                                             checkboxInput(ns('imputedmec_cb'), 
                                                           tags$span(
                                                               style = 'vertical-align: top; background: #F59898; color: white; padding: 5px;',
                                                               'Imputed MEC')
                                             )
                                         )
                                     )
                                 ),
                                 
                                 
                                 tags$li(
                                     checkboxInput(ns('combinedtags_cb'), 
                                                   tags$span(
                                                       style = 'vertical-align: top; background: #1E8E05; color: white; padding: 5px;',
                                                       'Combined tags')
                                     )
                                     # tags$ul(
                                     #     tags$li(
                                     #         checkboxInput(ns('partiallyquantified_cb'), 
                                     #                       tags$span(
                                     #                           style = 'vertical-align: top; background: #38CB17; color: white; padding: 5px;',
                                     #                           'Partially quantified')
                                     #         )
                                     #     )
                                     # )
                                 )
                             )
                         )
                     })
                     
                     
                     
                     
                     
                     
                     observeEvent(lapply(names(input), function(x) input[[x]]), ignoreInit = TRUE, {
                         
                         update_CB <- function(name){
                             widgets_to_disable <- names(input)[-match(reverse.mapping(name) , names(input))]
                             lapply(widgets_to_disable,
                                    function(x){
                                        updateCheckboxInput(session, x, value = FALSE)
                                        rv$tags[rv$mapping[x]] <- FALSE
                                    }
                             )
                             
                         } 
                         
                         events <- unlist(lapply(names(input), function(x) input[[x]]))
                        #browser()
                         if (is.null(rv$tags)){
                             rv$tags <- setNames(events, nm = gsub('_cb', '', unname(rv$mapping[names(input)])))
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
                        
                             is.leaf <- length(Children('peptide', newSelection)) == 0
                         if (is.leaf){
                             # If the new selection is a leaf if(!multiple)
                             if(!multiple())
                                 update_CB(newSelection)
                         } else {
                             # If the new selection is a node:
                             # by default, all its children must be also selected
                             children.names <- Children('peptide', newSelection)
                             print(children.names)
                             
                             if(multiple()){
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

ui <- mod_metacell_tree_ui('tree')

server <- function(input, output) {
    res <- mod_metacell_tree_server('tree', 
                                    level = 'protein', 
                                    multiple = reactive({FALSE})
    )
    
    observe({
        print(res())
    })
}

shinyApp(ui = ui, server = server)



