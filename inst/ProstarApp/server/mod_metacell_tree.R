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

mod_metacell_tree_server <- function(id, multiple = FALSE) {
    moduleServer(id,
        function(input, output, session) {
            ns <- session$ns
            
            rv <- reactiveValues(
                tags = setNames(rep(FALSE, length(metacell.def('peptide')$node)), 
                nm = metacell.def('peptide')$node)
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
                            ),
                            tags$ul(
                                tags$li(
                                    checkboxInput(ns('partiallyquantified_cb'), 
                                                  tags$span(
                                                      style = 'vertical-align: top; background: #38CB17; color: white; padding: 5px;',
                                                      'Partially quantified')
                                    )
                                )
                            )
                        )
                    )
                )
            })
            
            
            
            update_CB <- function(name){
                lapply(names(input)[-match(name , names(input))],
                       function(x)
                           updateCheckboxInput(session, x, value = FALSE)
                )
            }
            
            observeEvent(req(input$quantbydirectid_cb), {
                if(!multiple)
                    update_CB('quantbydirectid_cb')
            })
            
            observeEvent(req(input$quantbyrecovery_cb), {
                if(!multiple)
                    update_CB('quantbyrecovery_cb')
            })
            
            
            observeEvent(req(input$missingpov_cb), {
                if(!multiple)
                    update_CB('missingpov_cb')
            })
            
            observeEvent(req(input$missingmec_cb), {
                if(!multiple)
                    update_CB('missingmec_cb')
            })
            
            observeEvent(req(input$imputedpov_cb), {
                if(!multiple)
                    update_CB('imputedpov_cb')
            })
            
            observeEvent(req(input$missingmec_cb), {
                if(!multiple)
                    update_CB('missingmec_cb')
            })
            observeEvent(req(input$partiallyquantified_cb), {
                if(!multiple)
                    update_CB('partiallyquantified_cb')
            })
  
  
            observeEvent(req(input$quantified_cb), {
                if(multiple){
                    updateCheckboxInput(session, 'quantbydirectid_cb', value = TRUE)
                    updateCheckboxInput(session, 'quantbyrecovery_cb', value = TRUE)
                    
                } else
                    update_CB('quantified_cb')
            })
            
            observeEvent(req(input$missing_cb), {
                if(multiple){
                    updateCheckboxInput(session, 'missingpov_cb', value = TRUE)
                    updateCheckboxInput(session, 'missingmec_cb', value = TRUE)
                } else
                    update_CB('missing_cb')
            })
            
            observeEvent(req(input$imputed_cb), {
                if(multiple){
                    updateCheckboxInput(session, 'imputedpov_cb', value = TRUE)
                    updateCheckboxInput(session, 'imputedmec_cb', value = TRUE)
                } else
                    update_CB('imputed_cb')
            })
            
            observeEvent(req(input$combinedtags_cb), {
                if (multiple)
                    updateCheckboxInput(session, 'partiallyquantified_cb', value = TRUE)
                else
                    update_CB('combinedtags_cb')
            })
            
        observe({
            req(c(input$quantified_cb, 
                  input$quantbydirectid_cb,
                  input$quantbyrecovery_cb,
                  input$missing_cb,
                  input$missingpov_cb,
                  input$missingmec_cb,
                  input$imputed_cb,
                  input$imputedpov_cb,
                  input$imputedmec_cb,
                  input$combinedtags_cb,
                  input$partiallyquantified_cb))
            
            rv$tags['Quantified'] <- input$quantified_cb
            rv$tags['Quant. by direct id'] <- input$quantbydirectid_cb
            rv$tags['Quant. by recovery'] <- input$quantbyrecovery_cb
            
            rv$tags['Missing'] <- input$missing_cb
            rv$tags['Missing POV'] <- input$missingpov_cb
            rv$tags['Missing MEC'] <- input$missingmec_cb
            
            rv$tags['Imputed'] <- input$imputed_cb
            rv$tags['Imputed POV'] <- input$imputedpov_cb
            rv$tags['Imputed MEC'] <- input$imputedmec_cb
            
            rv$tags['Combined tags'] <- input$combinedtags_cb
            rv$tags['Partially quantified'] <- input$partiallyquantified_cb


        })
        
        return(reactive({names(which(rv$tags))}))
        
        }
    )
    
    
}



# Example

ui <- mod_metacell_tree_ui('tree')

server <- function(input, output) {
    res <- mod_metacell_tree_server('tree', multiple = TRUE)
    
    observe({
        print(res())
    })
}

shinyApp(ui = ui, server = server)



