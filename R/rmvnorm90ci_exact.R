#
# file: rmvnorm90ci_exact.R
#
# R package: decisionSupport
# 
# Authors (ToDo order?): 
#   Lutz Göhring <lutz.goehring@gmx.de>
#   Eike Luedeling (ICRAF) <E.Luedeling@cgiar.org>
#
# Affiliation: ToDo
# 
# License: ToDo
#
##############################################################################################
##############################################################################################
# rmvnorm90ci_exact( n, lower, upper, correlationMatrix)
##############################################################################################
#' Generate normal distributed multivariate random numbers based on the 90\%-confidence interval.
#' 
#' This function generates normal distributed multivariate random numbers 
#' based on the 90\%-confidence interval.
#' @param n Number of generated observations.
#' @param lower \code{numeric} vector; lower bound of the 90\% confidence intervall.
#' @param upper \code{numeric} vector; upper bound of the 90\% confidence intervall.
#' @param correlationMatrix \code{numeric} symmetric matrix; correlation matrix; 
#'   In particular, all diagonal elements must be equal to 1. 
#' @export
rmvnorm90ci_exact <- function(n, lower, upper, correlationMatrix){
  correlationMatrix<-as.matrix(correlationMatrix)
  lower<-data.matrix(lower)
  upper<-data.matrix(upper)
  colnames(lower)<-NULL
  colnames(upper)<-NULL
  # Constants:
  # 95%-critical value of standard normal distribution (c_0.95=1.645):
  c_0.95=qnorm(0.95)
  # Check preconditions
  if ( !is.numeric(lower) || !is.numeric(upper) )
    stop("lower and upper value of the 90%-confidence intervall must be given.")
  if( !identical(length(lower), length(upper)) )
    stop("lower and upper vectors must be of the same length.")
  if( !identical( correlationMatrix, t(correlationMatrix) ) ) 
    stop("correlationMatrix must be a symmetric matrix.")
  if( !identical( as.vector(diag(correlationMatrix)), rep(1, nrow(correlationMatrix)) ) )
  		stop("All diagonal elements of correlationMatrix must be equal to 1.")
  if( !identical(length(lower), nrow(correlationMatrix)) )
    stop("confidence interval vectors and correlationMatrix must the same number of rows.")
  # Calculate the mean vector from the 90-% confidence interval:
  mean<-rowMeans(cbind(lower,upper))
  # Calculate the standard deviation from the 90-% confidence interval: 
  sd<-((mean - lower)/c_0.95)[,1]
  # Calculate the covariance matrix:
  sigma<-(t(sd*correlationMatrix))*sd
  # Generate the random numbers 
  x<-rmvnorm(n=n,
             mean=mean,
             sigma=sigma)
  x
}
