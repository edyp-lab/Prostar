clipboard<-function(x, sep="\t", row.names=FALSE, col.names=F){
    con <- pipe("xclip -selection clipboard -i  -display :1", open="w") # note the 1 here
    #writeChar(x, con)  # for strings
    write.table(x, con, sep=sep, row.names=row.names, col.names=col.names) # for table
    close(con)
}



# Ouptut in alert ----

library(shiny)
library(shinyWidgets)

ui <- fluidPage(
    rclipboardSetup(),
    
    tags$h1("Click the button to open the alert"),
    uiOutput('BugReport_output'),
    actionButton(
        inputId = "sw_html",
        label = "Sweet alert with plot"
    ),
    uiOutput('toto'),
    #uiOutput('mailform'),
    use_mailtoR()
)

server <- function(input, output, session) {
    
     observeEvent(input$sw_html, {
         txt <- 'titi'
         
         show_alert(
            title = "Error",
            text = tags$div(
                style = "display:inline-block; vertical-align: top;",
                p(txt),
                rclipButton(inputId = "clipbtn",
                            label = "",
                            clipText = txt, 
                            icon = icon("copy"),
                            class = actionBtnClass
                            )
                ),
            html = TRUE,
            width = "60%"
        )
        
        observeEvent(input$clipbtn, clipboard(txt)
                     #clipr::write_clip(input$copytext)
        )
        
        output$toto <- renderUI({
            mailtoR(email = "wieczorek.sam@gmail.com",
                    text = "click here to send an email",
                    subject = "[URGENT]Bug Prostar]",
                    cc = '',
                    body = "Hi Michaels")
        })
        output$BugReport_output <- renderUI({
            
            mail <- unlist(strsplit(maintainer("Prostar"), "<"))[2]
            mail <- unlist(strsplit(mail, ">"))[1]
            
            tagList(
                a(actionButton(
                    inputId = "email1", label = "Contact maintainer",
                    icon = icon("envelope", lib = "font-awesome")
                ),
                href = paste0("mailto:", mail, "?subject=[Prostar bug report]&body=")
                )
            )
        })
        
    })
    
    
}


if (interactive())
    shinyApp(ui, server)