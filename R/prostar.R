#' Prostar : a GUI for DAPAR
#' 
#' @title Prostar
#' @return A new window in the default internet browser
#' @author Samuel Wieczorek
#' @examples
#' if(interactive()) {Prostar()}
#' 
#' @export
#' 
Prostar <- function(){
  options(shiny.maxRequestSize=1024^3,
          port = 3838,
          host = '0.0.0.0'
          )
  shiny::runApp(system.file("ProstarApp", package="Prostar"),
                launch.browser = TRUE
                )
}
