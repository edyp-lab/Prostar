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
}

.wtree li {
  list-style-type: none;
  margin: 10px 0 10px 10px;
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
   
            div(class='wtree',
                h1(class="title", 'Cell metadata tags'),
            tags$ul(
                tags$li(
                    checkboxInput(ns('quantified_cb'), 
                                  tags$span(
                                      style = 'vertical-align: top; background: #0A31D0; color: white; padding: 5px;',
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
)

    
    
}

mod_metacell_tree_server <- function(id) {
    moduleServer(id,
        function(input, output, session) {
            
            observeEvent(req(input$quantified_cb==TRUE), {
                updateCheckboxInput(session, 'quantbydirectid_cb', value = TRUE)
                updateCheckboxInput(session, 'quantbyrecovery_cb', value = TRUE)
            })
            
            observeEvent(req(input$missing_cb==TRUE), {
                updateCheckboxInput(session, 'missingpov_cb', value = TRUE)
                updateCheckboxInput(session, 'missingmec_cb', value = TRUE)
            })
            
            observeEvent(req(input$imputed_cb==TRUE), {
                updateCheckboxInput(session, 'imputedpov_cb', value = TRUE)
                updateCheckboxInput(session, 'imputedmec_cb', value = TRUE)
            })
            
            observeEvent(req(input$combinedtags_cb==TRUE), {
                updateCheckboxInput(session, 'partiallyquantified_cb', value = TRUE)
            })
            
        
        
        return(
            reactive({
                list(
                    quantified = input$quantified_cb,
                    quantbydirectid = input$quantbydirectid_cb,
                    quantbyrecovery = input$quantbyrecovery_cb,
                    missing = input$missing_cb,
                    missingpov = input$missingpov_cb,
                    missingmec = input$missingmec_cb,
                    imputed = input$imputed_cb,
                    imputedpov = input$imputedpov_cb,
                    imputedmec = input$imputedmec_cb,
                    combinedtags = input$combinedtags_cb,
                    partiallyquantified = input$partiallyquantified_cb
                )
                
            })
        )
        }
    )
    
    
}



# Example

ui <- mod_metacell_tree_ui('tree')

server <- function(input, output) {
    res <- mod_metacell_tree_server('tree')
    
    observe({
        print(res())
    })
}

shinyApp(ui = ui, server = server)



