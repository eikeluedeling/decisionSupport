##############################################################################################
# as.data.frame.mcSimulation(x, row.names, optional, ..., stringsAsFactors)
##############################################################################################
#' Coerce Monte Carlo simulation results to a data frame.
#' 
#' Coerces Monte Carlo simulation results to a data frame.
#' @param x An object of class \code{mcSimulation}.
#' @inheritParams base::as.data.frame
#' @seealso \code{\link{as.data.frame}}
#' @export
as.data.frame.mcSimulation <- function(x, row.names = NULL, optional = FALSE, ..., 
                                       stringsAsFactors = NA){
  
  if(is.na(stringsAsFactors)) {
    stringsAsFactors <-
      if(getRversion() < "4.1.0") default.stringsAsFactors()
    else FALSE}
  
  as.data.frame(list(y=x$y,x=x$x), row.names = row.names, optional = optional, ..., 
                stringsAsFactors = stringsAsFactors)
}
