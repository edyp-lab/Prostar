#' @title
#' 
#' @description
#' From: https://stackoverflow.com/questions/56942384/error-in-clipboard-on-x11-requires-that-the-display-envvar-be-configured
#' 
#' @export
#' 
clipboard <- function(x, sep="\t", row.names=FALSE, col.names=F){
    con <- pipe("xclip -selection clipboard -i  -display :1", open="w") # note the 1 here
    #writeChar(x, con)  # for strings
    write.table(x, con, sep=sep, row.names=row.names, col.names=col.names) # for table
    close(con)
}
