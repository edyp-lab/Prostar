heightSidebarPanel <- "600px"
test <- "Prostar"

widthLeftPanel <- "300px"
widthRightPanel <- "70%"
widthWellPanel <- "80%"
heightWellPanel <- "200px"

plotWidth <- "800px"
plotHeight <- "600px"

sidebarCustom <- function() {

}





sidebarPanelWidth <- function() {
    tags$head(
        tags$style(type = "text/css", "#fileopened {
                   font-weight:bold;
                   font-size:100%;
                   color:black; }")
    )
}


appCSS <- "
#loading-content {
position: absolute;
background: #000000;
opacity: 0.3;
z-index: 100;
left: 0;
right: 0;
height: 100%;
text-align: center;
color: #FFFFFF;
}
"
