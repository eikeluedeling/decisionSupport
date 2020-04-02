##############################################################################################
# hist.mcSimulation(x, ...)
##############################################################################################
#' Plot Histogram of results of a Monte Carlo Simulation
#' 
#' This function plots the histograms of the results of
#' \code{\link{mcSimulation}}.
#' @param x An object of class \code{mcSimulation}.
#' @param xlab \code{character}: x label of the histogram. If it is not
#'   provided, i.e. equals \code{NULL} the name of the chosen variable by
#'   argument \code{resultName} is used.
#' @param main \code{character}: main title of the histogram.
#' @inheritParams graphics::hist
#' @param ... Further arguments to be passed to \code{\link[graphics]{hist}}.
#' @param colorQuantile \code{character vector}: encoding the colors of the 
#'   quantiles defined in argument \code{colorProbability}.
#' @param colorProbability \code{numeric vector}: defines the quantiles that 
#'   shall be distinguished by the colors chosen in argument 
#'   \code{colorQuantile}. Must be of the same length as \code{colorQuantile}.
#' @param resultName \code{character}: indicating the name of the component of
#'   the simulation function (\code{model_function}) which results histogram
#'   shall be generated. If \code{model_function} is single valued, no name
#'   needs to be supplied. Otherwise, one valid name has to be specified.
#'   Defaults to \code{NULL}.
#' @return an object of class "\code{histogram}". For details see 
#'   \code{\link[graphics]{hist}}.
#' @seealso \code{\link{mcSimulation}}, \code{\link{hist}}. For a list of colors
#'   available in R see \code{\link[grDevices]{colors}}.
#' @export
hist.mcSimulation <- function(x, breaks=100, col=NULL, xlab=NULL, main=paste("Histogram of " , xlab), ...,
                              colorQuantile   =c("GREY", "YELLOW", "ORANGE", "DARK GREEN", "ORANGE", "YELLOW", "GREY"), 
                              colorProbability=c(1.00,    0.95,     0.75,     0.55,         0.45,     0.25,     0.05),
                              resultName=NULL){
  # ToDo: review!!!
  if( is.list(x$y) ){
    if( !is.null(resultName) ){
      result<-x$y[[resultName]]
      if( is.null(xlab) )
        xlab<-resultName
    } else {
      if(length(names(x$y))==1){
        result<-unlist(x$y)
        if( is.null(xlab) )
          xlab<-names(x$y)[[1]]
      }
      else 
        stop("No component of the model function chosen!")
    }
    if( main==paste("Histogram of " , xlab))
      main<-paste("Histogram of " , xlab, " Monte Carlo Simulation")
  } else { 
    result<-x$y
  }
  if(!isTRUE(is.null(colorQuantile))){
    resultNames<-NULL
    if( length(colorQuantile) != length(colorProbability) )
      stop("length(colorQuantile) != length(colorProbability)")
    histPrepare<-hist(result, breaks=breaks, plot=FALSE)
    probability<-cumsum(histPrepare$density * diff(histPrepare$breaks))
    color<-c()
    for( i in seq(along=probability) ){
      for( j in seq(along=colorQuantile) )
        if(probability[i] < colorProbability[j]) color[i]<-colorQuantile[j]
    }	
  } else
    color=col
  hist(result, breaks=breaks, col=color, xlab=xlab, main=main,...)
}
