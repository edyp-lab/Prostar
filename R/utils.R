#' @title
#' xxxx
#'
#' @description
#' xxxx
#'
#' @param obj xx
#'
#' @export
BuildColorStyles <- function(obj) {
    styles <- list(
        tags = NULL,
        colors = NULL
    )
    mc <- metacell.def(GetTypeofData(obj))
    
    styles$tags <- mc$node
    styles$colors <- mc$color
    styles
}



#' @title
#' xxxx
#'
#' @description
#' xxxx
#'
#' @param obj xx
#' @param digits xxx
#'
#' @export
#'
getDataForExprs <- function(obj, digits = NULL) {
    if (is.null(digits)) {
        digits <- 2
    }
    
    test.table <- as.data.frame(round(Biobase::exprs(obj)))
    if (!is.null(obj@experimentData@other$names_metacell)) { # agregated dataset
        test.table <- cbind(
            round(Biobase::exprs(obj), digits = digits),
            DAPAR::GetMetacell(obj)
        )
    } else {
        test.table <- cbind(
            test.table,
            as.data.frame(
                matrix(rep(NA, ncol(test.table) * nrow(test.table)),
                       nrow = nrow(test.table)
                )
            )
        )
    }
    return(test.table)
}
