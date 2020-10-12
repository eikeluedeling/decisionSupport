##############################################################################################
# print.mcSimulation(x, ...)
##############################################################################################
#' Print Basic Results from Monte Carlo Simulation.
#' 
#' This function prints basic results from Monte Carlo simulation  and returns it invisible.
#' @param x An object of class \code{mcSimulation}.
#' @param ... Further arguments to be passed to \code{\link{print.data.frame}}.
#' @seealso \code{\link{mcSimulation}}, \code{\link{print.data.frame}}
#' @export
print.mcSimulation <- function(x, ...){
  #ToDo: Review
  cat("Call:\n")
  print(x$call)
  cat("\nMonte Carlo simulation results:\n")
  print.data.frame(as.data.frame(x),...)
}
