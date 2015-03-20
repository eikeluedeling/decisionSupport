#
# file: via.R
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
# via(decision_model_function, estimate, certainVariables, decisionPrinciple,...)
##############################################################################################
#' Value of Information Analysis (VIA)
#' 
#' This function performs a Value of Information Analysis of a binary decision problem. For two 
#' alternative projects which generate different values the Expected Value of Perfect Information
#' for individual variables or clusters of variables is calculated. 
#' @param decision_model_function either a list of the two project functions: \code{list(decision_0, decision_1)}
#'    with either component being scalar or only one function if its value is the NPV of the project and the decision 
#'    alternative is the status quo.
#' @param estimate An object of class \code{estimate}; The status quo information on the variables, viz. the status
#'  quo estimate.
#' @param certainVariables A list of named vectors. Each list element represents a cluster of variables for which the EVPI
#'  is calculated. Each component of the vector gives the value of the variable that is assumed to be known with 
#'  certainty.
#' @param decisionPrinciple A character, equal to either \code{"riskMinimization"} or \code{"valueMaximization"},
#'   which defines the decision principle to use, viz. either risk minimization or maximization of expected net present value.   
#'  @param ... ToDo
#' @return An object of class \code{via} which contains the clustered EVPI for each cluster ordered by the value (greatest first).
#' @references Hubbard, Douglas W., ch. 7: Quantifying the Value of Information, in:
#'    How to Measure Anything? - Finding the Value of "Intangibles" in Business, 
#'    John Wiley & Sons, Hoboken, New Jersey, 2014, 3rd Ed.\cr
#'    Jeffrey, Scott R. and Pannell, David J. (2013),  Economics of Prioritising Environmental Research: 
#'    An Expected Value of Partial Perfect Information (EVPPI) Framework, Working Paper, School of Agricultural and Resource Economics, 
#'    University of Western Australia, \url{http://purl.umn.edu/144944}.
via <- function(decision_model_function, estimate, certainVariables, decisionPrinciple, ...){
  #ToDo
}
##############################################################################################
# print.via(x, ...)
##############################################################################################
#' Print Basic Results of the Value of Information Analysis.
#' 
#' This function prints basic results of the Value of Information Analysis and returns it invisible.
#' @param x An object of class \code{via}.
#' @param ... Further arguments #ToDo
#' @export
print.via <- function(x, ...){
  #ToDo
}
##############################################################################################
# summary.via(object, ...)
##############################################################################################
#' Summarize Results of the Value of Information Analysis.
#' 
#' summary.via produces result summaries of the results of of the Value of Information Analysis obtained by the function \code{\link{via}}.
#' @param object An object of class \code{via}.
#' @param ... Further arguments #ToDo
#' @return An object of class \code{summary.via}.
#' @export
summary.via <- function(object, ...){
  #ToDo
}
##############################################################################################
# print.summary.via(x, ...)
##############################################################################################
#' Print the Summary of a Value of Information Analysis.
#' 
#' This function prints the summary of of \code{via} obtained by \code{\link{summary.via}}.
#' @param x An object of class \code{via}.
#' @param ... Further arguments #ToDo
#' @export
print.summary.via <- function(x, ...){
  #ToDo
}
##############################################################################################
# plot.via(x, ...)
##############################################################################################
#' Plot results of a Value of Information Analysis.
#' 
#' This function plots results of \code{via}.
#' @param x An object of class \code{via}.
#' @param ... Further arguments #ToDo
#' @export
plot.via <- function(x, ...){
  #ToDo
}