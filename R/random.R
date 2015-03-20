# 
# file: random.R
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
# generic: random(rho,n,method,...)
##############################################################################################
#' Generate random numbers for a certain probability distribution.
#' 
#' This function generates multivariate random numbers for general multivariate 
#' distributions. 
#' @param rho Distribution to be randomly sampled.
#' @param n Number of generated observations.
#' @param method Particular method to be used for random number generation.
#' @param ... Optional arguments to be passed to the particular random number
#'  generating function.
#'  @export
random <- function(rho,n,method, ...) UseMethod("random")
##############################################################################################
# random.default(rho,n,method,...)
##############################################################################################
#' Generate random numbers based on the first two moments of a certain probability distribution.
#' 
#' This function generates random numbers for general multivariate 
#' distributions that can be characterized by the joint first two moments, viz. 
#' the mean and covariance. 
#' @param rho list; Distribution to be randomly sampled. 
#' @param n Number of generated observations
#' @param method Particular method to be used for random number generation.
#' @param ... Optional arguments to be passed to the particular random number
#'  generating function.
#' @export
random.default <- function(rho=list(distribution_type,mean,sd),n,method, ...){
  #ToDo: implement
  stop("function random.default() not implemented, yet!")
}


