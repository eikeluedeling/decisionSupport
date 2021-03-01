##############################################################################################
# summary.mcSimulation(object, ...)
##############################################################################################
#' Summarize results from Monte Carlo simulation.
#' 
#' A summary of the results of a Monte Carlo simulation obtained by the function 
#' \code{\link{mcSimulation}} is produced.
#' @param object An object of class \code{mcSimulation}.
#' @param ... Further arguments passed to \code{\link{summary.data.frame}} (\code{classicView=TRUE})
#'   or \code{\link{format}} (\code{classicView=FALSE}).
#' @param digits how many significant digits are to be used for numeric and complex x.
#' The default, NULL, uses \code{getOption("digits")}. This is a suggestion: enough decimal places
#' will be used so that the smallest (in magnitude) number has this many significant digits,
#' and also to satisfy nsmall. (For the interpretation for complex numbers see \code{\link[base:Round]{signif}}.)
#' @param variables.y \code{character} or \code{character vector}: Names of the components of the
#'   simulation function (\code{model_function}), whose results shall be displayed. Defaults to all
#'   components.
#' @param variables.x \code{character} or \code{character vector}: Names of the components of the
#'   input variables to the simulation function, i.e. the names of the variables in the input
#'   \code{estimate}, whose random sampling results shall be displayed. Defaults to all components.
#' @param classicView \code{logical}: if \code{TRUE} the results are summarized using
#'   \code{\link{summary.data.frame}}, if \code{FALSE} further output is produced and the quantile
#'   information can be chosen. Cf. section Value and argument \code{probs}. Default is
#'   \code{FALSE}.
#' @param probs \code{numeric vector}: quantiles that shall be displayed if 
#'   \code{classicView=FALSE}.
#' @return An object of class \code{summary.mcSimulation}.
#' @seealso \code{\link{mcSimulation}}, \code{\link{print.summary.mcSimulation}}, \code{\link{summary.data.frame}}
#' @export
summary.mcSimulation <- function(object,
                                 ...,
                                 digits = max(3, getOption("digits")-3),
                                 variables.y=names(object$y),
                                 variables.x=if(classicView) names(object$x),
                                 classicView=FALSE,
                                 probs=c(0, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 1)
){
  #ToDo: Review
  data<-as.data.frame(list(y=(object$y)[variables.y],x=(object$x)[variables.x]))
  if( classicView ){
    res<-list(summary=summary.data.frame(object=as.data.frame(data),...,digits=digits),
              call=object$call)
  } else{
    chance_loss<-function(x){
      length(x[x<0])/length(x)
    }
    chance_zero<-function(x){
      length(x[x==0])/length(x)
    }
    chance_gain<-function(x){
      length(x[x>0])/length(x)
    }
    
    #		data<-mcResult$y[variables]
    
    summaryDf<-as.data.frame(t(apply(X=data, MARGIN=2, FUN=quantile, probs=probs)))
    summaryDf<-cbind(summaryDf,
                     mean=colMeans(data),
                     deparse.level=1)
    summaryDf<-cbind(summaryDf,
                     chance_loss=apply(X=data, MARGIN=2, FUN=chance_loss),
                     deparse.level=1)
    summaryDf<-cbind(summaryDf,
                     chance_zero=apply(X=data, MARGIN=2, FUN=chance_zero),
                     deparse.level=1)
    summaryDf<-cbind(summaryDf,
                     chance_gain=apply(X=data, MARGIN=2, FUN=chance_gain),
                     deparse.level=1)
    
    summaryDf<-format(x=summaryDf, digits=digits, ...)
    res<-list(summary=summaryDf,
              call=object$call)
  }
  
  class(res)<-"summary.mcSimulation"
  res
}
