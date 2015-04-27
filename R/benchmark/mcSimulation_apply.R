#
# file: mcSimulation_apply.R
#
# R package: decisionSupport
# 
# Authors (ToDo order?): 
#   Lutz Göhring <lutz.goehring@gmx.de>
#   Eike Luedeling (ICRAF) <eike@eikeluedeling.com>
#
# Affiliation: ToDo
# 
# License: ToDo
#
##############################################################################################
##############################################################################################
# mcSimulation_apply(estimate, model_function, numberOfModelRuns, ...)
##############################################################################################
# Perform a Monte Carlo Simulation using apply(...) for benchmarking.
#
mcSimulation_apply <- function(estimate, model_function, ..., numberOfModelRuns, 
                               randomMethod="exact", functionSyntax=NULL,
                               xAs="data.frame", applyMethod="complex"){
  if ( !is.null(functionSyntax) )
    stop("functionSyntax  not implemented, yet!")
  #ToDo: (i) review code and (ii) test
  x<-random(rho=estimate, n=numberOfModelRuns, method=randomMethod)
  if (applyMethod=="complex"){
    if ( xAs=="data.frame" ){
      y<-apply(t(sapply(apply(X=as.data.frame(x), 1, FUN=model_function, ...),c)),2,as.numeric)
    } else if ( xAs=="matrix" ){
      y<-apply(t(sapply(apply(X=as.matrix(x), 1, FUN=model_function, ...),c)),2,as.numeric) 
    } else
      stop("xAs=", xAs, "is not defined!") 
  }
  else if (applyMethod=="simple"){
    if ( xAs=="data.frame" ){
      y<-apply(X=as.data.frame(x), 1, FUN=model_function, ...)
    } else if ( xAs=="matrix" ){
      y<-apply(X=as.matrix(x), 1, FUN=model_function, ...)
    } else
      stop("xAs=", xAs, "is not defined!") 
  } else
    stop("applyMethod=", applyMethod, "is not defined!") 
  # Remove names in y if any.
  names(y)<-NULL
  returnObject<-data.frame(y=y, x=x)
  class(returnObject)<-cbind("mcSimulation", class(returnObject))
  
  return(returnObject)
}
