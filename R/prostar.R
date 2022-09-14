#' @title Prostar
#' @return A new window in the default internet browser
#' @author Samuel Wieczorek
#' @examples
#' if (interactive()) {
#'     Prostar()
#' }
#'
#' @export
#'
#' @import shinycssloaders
#' @import shinythemes
# #' @import DT
#' @import highcharter
#' @import shinyBS
#' @import shinyAce
# #' @import shinyWidgets
# #' @import colourpicker
#' @import data.table
# #' @import MSnbase
#' @import promises
#' @import DAPAR
# #' @import R.utils
#' @import rhandsontable
#' @import future
#' @import DAPARdata
#' @import shinyjs
#' @import htmlwidgets
#' @import webshot
# #' @import XML
#' @import later
#' @import shinyjqui
# #' @import sass
#' @import tibble
# #' @import MSnbase
# #' @import RColorBrewer
#' @import ggplot2
#' @import gplots
#' @import vioplot
# #' @import gtools
Prostar <- function() {
    options(
        shiny.maxRequestSize = 1024^3,
        port = 3838,
        host = "0.0.0.0"
    )
    shiny::runApp(system.file("ProstarApp", package = "Prostar"),
        launch.browser = TRUE
    )
}
