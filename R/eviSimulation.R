#
# file: eviSimulation.R
#
# This file is part of the R-package decisionSupport
# 
# Authors: 
#   Lutz Göhring <lutz.goehring@gmx.de>
#   Eike Luedeling (ICRAF) <E.Luedeling@cgiar.org>
#
# Copyright (C) 2015 World Agroforestry Centre (ICRAF) 
#	http://www.worldagroforestry.org
# 
# The R-package decisionSupport is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# The R-package decisionSupport is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with the R-package decisionSupport.  If not, see <http://www.gnu.org/licenses/>.
#
##############################################################################################
##############################################################################################
# eviSimulation(model, currentEstimate, prospectiveEstimate, numberOfSimulations, functionSyntax)
##############################################################################################
#' Expected Value of Information (EVI) Simulation
#' 
#' The Expected Value of Information (EVI) is calculated based on a Monte Carlo simulation
#' of the values of two different decision alternatives.
#' @param model either a function or a list with two functions: \code{list(p1,p2)}. In the first case the function is the 
#' net benefit of project approval vs. the status quo. In the second case the element \code{p1} is the function valuing 
#' the first project and the element \code{p2} valueing the second project.
#' @param currentEstimate \code{\link{estimate}} object describing the distribution of the input variables as currently estmated.
#' @param prospectiveEstmate \code{\link{estimate}} object describing the prospective distribution of the input variables 
#' 		which could hypothetically achieved by collecting more information, viz. improving the measurement.
#' @param numberOfSimulations integer; number of simulations to be used in the underlying Monte Carlo analysis
#' @param functionSyntax function character; function syntax used in the model function(s).
#' @return An object of class \code{eviSimulation} with the following elements:
#'  \tabular{ll}{
#' 			\code{current} \tab \code{\link{welfareDecisionAnalysis}} object for \code{currentEstimate}\cr
#' 			\code{prospective} \tab \code{\link{welfareDecisionAnalysis}} object  for \code{prospectiveEstimate}\cr
#'  		\code{evi}   \tab  Expected Value of Information (EVI) of gained by the prospective estimate w.r.t. 
#'  								the current estimate
#' }
#' @details This principle is along the line described in Hubbard (2014). The Expected Value of Information is the decrease in the EOL
#'  for an information improvement from the current estimate (I_current) to a better prospective (or hypothetical) information (I_prospective):
#'   EVI := EOL(I_current) - EOL(I_prospective). Thus, the EVI depends on the model for valueing a decision, 
#'   the current information, i.e. the current estimate, and the specification of a hypothetical improvement in information, i.e. a prospective
#'    estimate. 
#' @examples 
#' #############################################################
#' # Example 1 Only one prospective estimate:
#' #############################################################
#' numberOfSimulations=10000
#' # Create the estimate object:
#' variable=c("revenue","costs")
#' distribution=c("posnorm","posnorm")
#' lower=c(10000,  5000)
#' upper=c(100000, 50000)
#' currentEstimate<-estimate(variable, distribution, lower, upper)
#' prospectiveEstimate<-currentEstimate
#' revenueConst<-mean(c(currentEstimate$base["revenue","lower"],currentEstimate$base["revenue","upper"]))
#' prospectiveEstimate$base["revenue",]<-data.frame(distribution="const",
#' 																								 lower=revenueConst, 
#' 																								 upper=revenueConst, 
#' 																								 row.names="revenue",
#' 																								 stringsAsFactors=FALSE)
#' # (a) Define the model function without name for the return value:
#' profit<-function(x){
#' 	x$revenue-x$costs
#' }
#' 
#' # Calculate the Expected Value of Information:
#' eviSimulationResult<-eviSimulation(model=profit,
#' 																	 currentEstimate=currentEstimate,
#' 																	 prospectiveEstimate=prospectiveEstimate,
#' 																	 numberOfSimulations=numberOfSimulations,
#' 																	 functionSyntax="data.frameNames")
#' # Show the simulation results:
#' print(summary(eviSimulationResult))
#' #############################################################
#' # (b) Define the model function with a name for the return value:
#' profit<-function(x){
#' 	list(Profit=x$revenue-x$costs)
#' }
#' # Calculate the Expected Value of Information:
#' eviSimulationResult<-eviSimulation(model=profit,
#'																	 currentEstimate=currentEstimate,
#' 																	 prospectiveEstimate=prospectiveEstimate,
#' 																	 numberOfSimulations=numberOfSimulations,
#' 																	 functionSyntax="data.frameNames")
#' # Show the simulation results:
#' print(summary((eviSimulationResult)))
#' #############################################################
#' # (c) Two decision variables:
#' decisionModel<-function(x){
#' 	list(Profit=x$revenue-x$costs,
#' 			 Costs=-x$costs)
#' }
#' # Calculate the Expected Value of Information:
#' eviSimulationResult<-eviSimulation(model=decisionModel,
#' 																	 currentEstimate=currentEstimate,
#' 																	 prospectiveEstimate=prospectiveEstimate,
#' 																	 numberOfSimulations=numberOfSimulations,
#'																	 functionSyntax="data.frameNames")
#' # Show the simulation results:
#' print(summary((eviSimulationResult)))
#' #############################################################
#' # Example 2 A list of prospective estimates:
#' #############################################################
#' numberOfSimulations=10000
#' #  Define the model function with a name for the return value:
#' profit<-function(x){
#' 	list(Profit=x$revenue-x$costs)
#' }
#' # Create the estimate object:
#' variable=c("revenue","costs")
#' distribution=c("posnorm","posnorm")
#' lower=c(10000,  5000)
#' upper=c(100000, 50000)
#' currentEstimate<-estimate(variable, distribution, lower, upper)
#' perfectInformationRevenue<-currentEstimate
#' revenueConst<-mean(c(currentEstimate$base["revenue","lower"],currentEstimate$base["revenue","upper"]))
#' perfectInformationRevenue$base["revenue",]<-data.frame(distribution="const",
#' 																											 lower=revenueConst, 
#' 																											 upper=revenueConst, 
#' 																											 row.names="revenue",
#' 																											 stringsAsFactors=FALSE)
#' # (a) A list with one element
#' prospectiveEstimate<-list(perfectInformationRevenue=perfectInformationRevenue)
#' # Calculate the Expected Value of Information:
#' eviSimulationResult<-eviSimulation(model=profit,
#' 																	 currentEstimate=currentEstimate,
#' 																	 prospectiveEstimate=prospectiveEstimate,
#' 																	 numberOfSimulations=numberOfSimulations,
#' 																	 functionSyntax="data.frameNames")
#' # Show the simulation results:
#' print(summary(eviSimulationResult))
#' #############################################################
#' # (b) A list with two elements
#' perfectInformationCosts<-currentEstimate
#' costsConst<-mean(c(currentEstimate$base["costs","lower"],currentEstimate$base["costs","upper"]))
#' perfectInformationCosts$base["costs",]<-data.frame(distribution="const",
#' 																									 lower=costsConst, 
#' 																									 upper=costsConst, 
#' 																									 row.names="costs",
#' 																									 stringsAsFactors=FALSE)
#' prospectiveEstimate<-list(perfectInformationRevenue=perfectInformationRevenue,
#' 													perfectInformationCosts=perfectInformationCosts)
#' # Calculate the Expected Value of Information:
#' eviSimulationResult<-eviSimulation(model=profit,
#' 																	 currentEstimate=currentEstimate,
#' 																	 prospectiveEstimate=prospectiveEstimate,
#' 																	 numberOfSimulations=numberOfSimulations,
#' 																	 functionSyntax="data.frameNames")
#' # Show the simulation results:
#' print(summary(eviSimulationResult))
#' #############################################################
#' # Example 3 A list of prospective estimates and two decision variables:
#' #############################################################
#' numberOfSimulations=10000
#' # Create the current estimate object:
#' variable=c("revenue","costs")
#' distribution=c("posnorm","posnorm")
#' lower=c(10000,  5000)
#' upper=c(100000, 50000)
#' currentEstimate<-estimate(variable, distribution, lower, upper)
#' # Create a list of two prospective estimates:
#' perfectInformationRevenue<-currentEstimate
#' revenueConst<-mean(c(currentEstimate$base["revenue","lower"],currentEstimate$base["revenue","upper"]))
#' perfectInformationRevenue$base["revenue",]<-data.frame(distribution="const",
#' 																											 lower=revenueConst, 
#' 																											 upper=revenueConst, 
#' 																											 row.names="revenue",
#' 																											 stringsAsFactors=FALSE)
#' perfectInformationCosts<-currentEstimate
#' costsConst<-mean(c(currentEstimate$base["costs","lower"],currentEstimate$base["costs","upper"]))
#' perfectInformationCosts$base["costs",]<-data.frame(distribution="const",
#' 																									 lower=costsConst, 
#' 																									 upper=costsConst, 
#' 																									 row.names="costs",
#' 																									 stringsAsFactors=FALSE)
#' prospectiveEstimate<-list(perfectInformationRevenue=perfectInformationRevenue,
#' 													perfectInformationCosts=perfectInformationCosts)
#' # Define the model function with two decision variables:
#' decisionModel<-function(x){
#' 	list(Profit=x$revenue-x$costs,
#' 			 Costs=-x$costs)
#' }
#' # Calculate the Expected Value of Information:
#' eviSimulationResult<-eviSimulation(model=decisionModel,
#' 																	 currentEstimate=currentEstimate,
#' 																	 prospectiveEstimate=prospectiveEstimate,
#' 																	 numberOfSimulations=numberOfSimulations,
#' 																	 functionSyntax="data.frameNames")
#' # Show the simulation results:
#' print(sort(summary(eviSimulationResult)),decreasing=TRUE,along="Profit")
 #' @seealso \code{\link{welfareDecisionAnalysis}}, \code{\link{mcSimulation}}, \code{\link{estimate}}
#' @export
eviSimulation<-function(model, currentEstimate, prospectiveEstimate, numberOfSimulations, functionSyntax="data.frameNames"){
	# Return object:
	thisAnalysis<-NULL
	# Perform the current decision analysis:
	analysisCurrent<-welfareDecisionAnalysis( estimate=currentEstimate,
																		 model=model,
																		 numberOfSimulations=numberOfSimulations,
																		 functionSyntax=functionSyntax)
	
	# Perform the prospective decision analysis:
	if( class(prospectiveEstimate) == "estimate"){
		# Perform the decision analysis:
		analysisProspective<-welfareDecisionAnalysis( estimate=prospectiveEstimate,
																					 model=model,
																					 numberOfSimulations=numberOfSimulations,
																					 functionSyntax=functionSyntax)
		evi<-analysisCurrent$eol - analysisProspective$eol
	} else if ( is.list(prospectiveEstimate) ){
		analysisProspective<-lapply(X=prospectiveEstimate, 
																FUN=function(estimate) welfareDecisionAnalysis(estimate=estimate,
																																				model=model,
																																				numberOfSimulations=numberOfSimulations,
																																				functionSyntax=functionSyntax)
		)
		evi<-lapply(X=analysisProspective, 
								FUN=function(x) analysisCurrent$eol - x$eol)
	} else {
		stop("prospectiveEstimate must be either an estimate or a list of estimates.")
	}	
	
	# Fill return object:
	thisAnalysis$call<-match.call()
	thisAnalysis$current<-analysisCurrent
	thisAnalysis$prospective<-analysisProspective
	thisAnalysis$evi<-as.data.frame(evi)
	class(thisAnalysis) <- "eviSimulation"
	return(thisAnalysis)
}
##############################################################################################
# summary.eviSimulation(object, ...)
##############################################################################################
#' Summarize EVI Simulation Results..
#' 
#' summary.eviSimulation produces result summaries of the results of Expected Value of 
#'  Information (EVI) simulation obtained by the function \code{\link{eviSimulation}}.
#' @param object An object of class \code{eviSimulation}.
#' @param ... Further arguments #ToDo
#' @return An object of class \code{summary.eviSimulation}.
#' @seealso \code{\link{eviSimulation}}, \code{\link{print.summary.eviSimulation}}
#' @export
summary.eviSimulation <- function(object,
																	...,
																	digits = max(3, getOption("digits")-3)){	
	summaryList<-list(evi=format(x=object$evi, digits=digits),
										current=summary(object$current, ..., digits=digits),
										prospective=if( class(object$prospective)=="welfareDecisionAnalysis" ){
											summary(object$prospective, ..., digits=digits)
										}else{
											lapply(X=object$prospective, 
														 FUN=function(x) summary(x, ..., digits=digits)
											)
										}
	)
	#	summaryList<-format(x=summaryList, digits=digits, ...)
	res<-list(summary=summaryList,
						call=object$call)
	
	class(res)<-"summary.eviSimulation"
	res
}
##############################################################################################
# sort.summary.eviSimulation(x, decreasing, ..., along)
##############################################################################################
#' Sort Summarized EVI Simulation Results..
#' 
#' Sort summarized EVI simulation results according to their EVI.
#' @param x An object of class \code{summary.eviSimulation}.
#' @param decreasing logical; if the evi should be sorted in decreasing order.
#' @param ... Further arguments #ToDo
#' @param along character; the name of the valuation variable along which evi 
#'  should be sorted.
#' @return An object of class \code{summary.eviSimulation}.
#' @seealso \code{\link{eviSimulation}}, \code{\link{summary.eviSimulation}}, \code{\link[base]{sort}}
#' @export
sort.summary.eviSimulation <- function(x, decreasing=TRUE, ..., along=row.names(x$summary$evi)[[1]]){
	eviRanking<-order(x=as.numeric(x$summary$evi[along,]), decreasing=decreasing)
	eviRankingNames<-names(x$summary$evi)[eviRanking]
	x$summary$evi<-x$summary$evi[eviRankingNames]
	x$summary$prospective<-x$summary$prospective[eviRankingNames]
	x
}
##############################################################################################
# print.summary.eviSimulation(x, ...)
##############################################################################################
#' Print the Summarized EVI Simulation Results.
#' 
#' This function prints the summary of of \code{eviSimulation} obtained by \code{\link{summary.eviSimulation}}.
#' @param x An object of class \code{summary.eviSimulation}.
#' @param ... Further arguments #ToDo
#' @seealso \code{\link{eviSimulation}}
#' @export
print.summary.eviSimulation <- function(x, ...){
	cat("Call:\n")
	print(x$call)
	cat("\nExpected Value of Information (EVI):\n")
	print(x$summary$evi,...)
	cat("\nUnderlying welfare decision analysis:\n")
	cat("Based on the current estimate:\n")
	print(x$summary$current, ...)
	cat("\nBased on the prospective estimate(s):\n")
	print(x$summary$prospective, ...)
}
