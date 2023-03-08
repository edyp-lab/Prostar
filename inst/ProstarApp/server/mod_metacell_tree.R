# This css is adapted from: https://codepen.io/willpower/pen/pJKdej
css <- "
* {
  margin: 0;
  padding: 0;
  box-sizing: border-box;
}

body {
  padding: 50px;
  font-family: helvetica, arial, sans-serif;
  font-size: 10px;
}

ul {
  margin: -10px 0px 30px 20px;
}

.wtree li {
  list-style-type: none;
  margin: 0px 0 -10px 10px;
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
  padding: 0px 0px 10px 0px;
}
.wtree li span {
  display: inline-block;
  border: 0px solid #ddd;
  border-radius: 10px;
  text-align: center;
  vertical-align: middle;
  padding: 0px 5px 0px 0px;
  color: #888;
  text-decoration: none;
  width: 100px;
}"


mod_metacell_tree_ui <- function(id) {
    ns <- NS(id)
    
    tagList(
        shinyjs::inlineCSS(css),
        h4('Cells metadata tags'),
        actionLink(ns("show_metacell_tree"),
                   tags$img(src = "images/metacelltree.png", height = "50px"))
        )

}

mod_metacell_tree_server <- function(id, level = NULL) {
    
    if(is.null(level))
        stop('level is empty')
    
    
    moduleServer(id,
                 function(input, output, session) {
                     ns <- session$ns
                     
                     # Show modal when button is clicked.
                     observeEvent(req(input$show_metacell_tree), ignoreNULL = TRUE, {
                         print('------------------observeEvent(input$show_metacell_tree-------------------------')
                         showModal(
                             div(
                                 #shinyjqui::jqui_draggable(
                             id = 'treeModal',
                             tags$style("#treeModal .modal-dialog{width: 400px;}"),
                             modalDialog(
                                 h3(modulePopoverUI(ns("metacellTag_help"))),
                                 div(style='display: inline-block;',
                                     radioButtons(ns('checkbox_mode'), 
                                                    p(style='font-size: 16px;','Multiple selection'), 
                                                    choices = c('Single selection' = 'single',
                                                                'Complete subtree' = 'subtree',
                                                                'Multiple selection' = 'multiple')
                                                    ),
                                 actionButton(ns('cleartree'), 'Clear selection', class=actionBtnClass)
                                 ),
                                 uiOutput(ns('tree')),
                                 footer = tagList(
                                     modalButton("Cancel"),
                                     actionButton(ns("ok"), "OK", class = actionBtnClass)
                                 )
                             )
                             )
                         )
                     })
                     
                     
                     # When OK button is pressed, attempt to load the data set. If successful,
                     # remove the modal. If not show another modal, but this time with a failure
                     # message.
                     observeEvent(input$ok, ignoreInit = TRUE, ignoreNULL = TRUE, {
                         print('################### observeEvent(input$ok) ##############################')
                         removeModal()
                         rv$dataOut <- names(rv$tags)[which(rv$tags == TRUE)]
                         
                     })
                     
                     
                     convertWidgetName <- function(name){
                         print('convertWidgetName()')
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
                         print('BuildMapping()')
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
                         print('reverse.mapping()')
                         return(names(rv$mapping)[which(rv$mapping == x)])
                     }
                     
                     
                     GetTreeCBInputs <- reactive({
                         print('GetTreeCBInputs()')
                         names(rv$mapping)
                         #    names(input)[grepl('_cb', names(input))]
                         #})
                     })
                     
                     
                     rv <- reactiveValues(
                         tags = NULL,
                         mapping = BuildMapping()$names,
                         bg_colors = BuildMapping()$colors,
                         dataOut = NULL
                     )
                     
                     observe({
                         print('observe rv$tags <- setNames(r')
                         tmp <- unname(rv$mapping[GetTreeCBInputs()])
                         rv$tags <- setNames(rep(FALSE, length(tmp)), 
                                             nm = gsub('_cb', '', tmp)
                         )
                         #browser()
                         
                     })
                     
                     observeEvent(c(input$checkbox_mode, input$cleartree), {
                         print('observeEvent(c(input$checkbox_mode, input$cleartree)')
                         
                         update_CB()
                         for (l in GetTreeCBInputs()) {
                             if(length(DAPAR::Children(level, rv$mapping[l]))==0)
                                 shinyjs::toggleState(l, input$checkbox_mode != 'subtree')
                             
                         }
                     })

                    output$tree <- renderUI({
                         print('output$tree <- renderUI')
                        div(style = "overflow-y: auto;",
                         uiOutput(ns(paste0('metacell_tree_', level)))
                         )
                     })
                     
                     .style <- 'vertical-align: top;
                                background: #bg-color#; 
                                color: white; 
                                padding: 5px;'

                     
                     # Define tree for protein dataset
                     output$metacell_tree_protein <- renderUI({
                         
                         
                         div(class='wtree',
                                shinyjs::useShinyjs(),
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
                         print('update_CB')
                         #browser()
                         if(!is.null(nametokeep))
                            widgets_to_disable <- GetTreeCBInputs()[-match(reverse.mapping(nametokeep), GetTreeCBInputs())]
                         else
                             widgets_to_disable <- GetTreeCBInputs()
                         
                         lapply(widgets_to_disable,
                                function(x){
                                    updateCheckboxInput(session, x, value = FALSE)
                                    rv$tags[rv$mapping[x]] <- FALSE
                                }
                         )
                         
                     } 
                     
                     
                     update_CB_childrens <- function(children.names){
                         print('update_CB_childrens()')
                         #browser()
                         if (is.null(children.names) || length(children.names)==0)
                             return(NULL)
                         
                         # rv$tags[children.names] <- TRUE
                         # for (i in children.names)
                         #     updateCheckboxInput(session, reverse.mapping(i), value = TRUE)
                         lapply(children.names, function(x){
                             updateCheckboxInput(session, reverse.mapping(x), value = TRUE)
                             rv$tags[x] <- TRUE
                         }
                         )
                     }
                     
                     
                     
                     somethingChanged <- reactive({
                         events <- unlist(lapply(GetTreeCBInputs(), function(x) input[[x]]))
                         compare <- rv$tags == events
                         length(which(compare==FALSE))==0
                     })
                     
                     
                     
                     # Catch a change in the selection of a node
                     observeEvent(somethingChanged(), ignoreInit = TRUE, {
                         req(length(GetTreeCBInputs()) > 0)
                         #print('observeEvent(unlist(lapply(GetTreeCBInputs()()')
                         #browser()
                        # Get the values of widgets corresponding to nodes in the tree
                         print(paste0('in observeEvent xxxxx :', input$show_metacell_tree))
                         events <- unlist(lapply(GetTreeCBInputs(), function(x) input[[x]]))
                         #print(events)
                         
                       # print(length(rv$tags))
                       # print(length(events))
                        compare <- rv$tags == events
                        
                         #browser()
                         # Deduce the new selected node
                         newSelection <- names(rv$tags)[which(compare==FALSE)]
                         # Update rv$tags vector with this new selection
                         if (length(newSelection) > 0) {
                             for (i in newSelection)
                           rv$tags[i] <- input[[reverse.mapping(i)]]
                         
                         
                             switch(input$checkbox_mode, 
                                single = {update_CB(newSelection)},
                                subtree = {
                                    # As the leaves are disabled, this selection is a node
                                    # by default, all its children must be also selected
                                    #browser()
                                    childrens <- DAPAR::Children(level, newSelection)
                                    print('childrens = ')
                                    print(childrens)
                                    update_CB_childrens(childrens)
                                },
                                multiple = {}
                                )

}
                 
                     })
                     
                     
                     
                     
        return(reactive({rv$dataOut}))
                     
                 }
    )
    
    
}



# Example
# 
ui <- fluidPage(
    tagList(
        mod_metacell_tree_ui('tree'),
      uiOutput('res')
    )
)

server <- function(input, output) {

    rv <- reactiveValues(
        tags = NULL
    )

    rv$tags <- mod_metacell_tree_server('tree', 
                                        level = 'protein')
    output$res <- renderUI({
      p(paste0(rv$tags(), collapse=','))
    })
}

shinyApp(ui = ui, server = server)



