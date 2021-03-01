##############################################################################################
# print.summary.mcSimulation(x, ...)
##############################################################################################
#' Print the summary of a Monte Carlo simulation.
#' 
#' This function prints the summary of of \code{mcSimulation} obtained by \code{\link{summary.mcSimulation}}.
#' @param x An object of class \code{mcSimulation}.
#' @param ... Further arguments to be passed to \code{\link{print.data.frame}}.
#' @seealso \code{\link{mcSimulation}}, \code{\link{summary.mcSimulation}}, 
#'   \code{\link{print.data.frame}}
#' @export
print.summary.mcSimulation <- function(x, ...){
  cat("Call:\n")
  print(x$call)
  cat("\nSummary of Monte Carlo simulation:\n")
  print(x$summary,...)
}
