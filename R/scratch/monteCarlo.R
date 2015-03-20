#
# file: monteCarlo.R
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

#' Perform a Monte Carlo Simulation.
#' 
#' This method solves the following problem. Given a multivariate random variable 
#' \eqn{x = (x_1,\ldots,x_k)} with joint probability distribution \eqn{P}, i.e.
#' \deqn{x ~ P.} Then the continuous function 
#'  \deqn{f:R^k --> R^l, y = f(x)}
#' defines another random variable with distribution 
#'  \deqn{y ~ f(P).} 
#' Given a probability density \eqn{\rho} of x that defines \eqn{P} the problem is the determination of the
#' probability density \eqn{\phi} that defines \eqn{f(P)}. This method samples the probability density \eqn{\phi} 
#' of \eqn{y} by Monte Carlo simulation.
#' 
#' @param rho a multivariate probability distribution
#' @param f a single or multivariate numeric function.
#' @param ... optional arguments of f 
#' @param iterations Number of Monte Carlo simulations.
#' @param sampleMethod
#' @return An object of class \code{monteCarlo}.
#' 
#' \tabular{ll}{
#'  \code{phi} \tab an l-variate probability distribution\cr
#'  \code{x}   \tab a dataframe containing the sampled \eqn{x -} values\cr
#'  \code{y}   \tab a dataframe containing the simulated \eqn{y -} values
#' }
#' @export
#'
monteCarlo <- function(rho, f, ..., iterations, sampleMethod){
  x<-sample(rho,iterations,method=sampleMethod)
  y<-apply(X=x, MARGIN=1, FUN=f, ...)
#  y<-f(x,...)
  phi$mean<-mean(y)
  phi$sigma<-cov(y)
  returnObject$x <- x
  returnObject$y <- y
  returnObject$phi <- phi
  class(returnObject)<-"monteCarlo"
  
  return(returnObject)
}
###########################################################################################################
#monteCarlo <- function(x, f, ... ) UseMethod("monteCarlo")
#ToDo: monteCarlo <- function(x, f, yNames, ... ) UseMethod("monteCarlo") ?

# monteCarlo.default <- function(x, f,  ...){
#   #ToDo: Error handling
# }

monteCarlo.data.frame <- function(x, f, ...){
  #ToDo: check preconditions:
  
  # Accomplisch the transformation f
  y<-apply(X=x, MARGIN=1, FUN=f, ...)
#  y <- f(x,...)
#  y <- sapply(x,f,...) NB: this is not necessary!
  # Return value
  y
}
monteCarlo.matrix <- function(x, f, ...){
  #ToDo: check preconditions:
  y<- f(as.matrix(x),...)
  # Return value
  y
}
# monteCarlo.mvdist <- function(x, f, ...){
#   #ToDo
# }
# 


# Output ------------------------------------------------------------------

#' Print information on \code{\link{monteCarlo}} object
#'
#' 
print.monteCarlo <- function(x, ...){
  #ToDo
}

#' Summarize a \code{\link{monteCarlo}} object
#'
#' 
#' 
summary.monteCarlo <- function(x, ...){
  #ToDo
}

#' Print the summary on a \code{\link{monteCarlo}} object
#'
#' 
print.summary.monteCarlo <- function(x, ...){
  #ToDo
}

#' Plot Monte Carlo Simulation results
#' 
#' 
plot.monteCarlo <- function(x,...){
  #ToDo
}

#' Summarize distributions
#'
#' 
summarizeDistributions <- function (x, ...){
  #ToDo
}
