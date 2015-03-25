#
# file: welfareDecisionAnalysis.R
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
#' @include mcSimulation.R
NULL
#############################################################
# welfareDecisionAnalysis(estimate, model, numberOfSimulations, functionSyntax)
#############################################################
#' Analysis of the Underlying Welfare Based Decision Problem
#' 
#' The optimal choice between two different opportunities is calculated. This decision is based on minimizing 
#' the Expected Net Loss (ENL).
#' @param estimate \code{\link{estimate}} object describing the distribution of the input variables.
#' @param model either a function or a list with two functions: \code{list(p1,p2)}. In the first case the function is the 
#' net benefit of project approval vs. the status quo. In the second case the element \code{p1} is the function valuing 
#' the first project and the element \code{p2} valueing the second project.
#' @param numberOfSimulations integer; number of simulations to be used in the underlying Monte Carlo analysis
#' @param functionSyntax function character; function syntax used in the model function(s).
#' @return An object of class \code{welfareDecisionAnalysis} with the following elements:
#'  \tabular{ll}{
#' 			\code{enbPa} \tab Expected Net Loss (ENL) in case of project approval (PA)\cr
#' 			\code{enbSq} \tab Expected Net Loss (ENL) in case of status quo (SQ)\cr
#'  		\code{eol}   \tab  Expected Oportunity Loss (EOL)\cr
#'  		\code{optimalChoice} \tab The optimal choice, i.e. either 
#'  															project approval (PA) or the status quo (SQ)
#' }
#' @details This principle is along the line described in Hubbard (2014). The Expected Opportunity Loss (EOL) is defined as the 
#' Expected Net Loss (ENL) for the best decision. The best decision minimises the ENL. The EOL is always conditional on the available 
#' information (I): EOL=EOL(I). Here, the available information is the supplied estimate. One can show that in the case of two 
#' alternatives, minimization of EOL is equivalent to maximization of the Expected Net Benefit.
#' @examples
#' #############################################################
#' # Example 1 (Creating the estimate from the command line):
#' #############################################################
#' # Create the estimate object:
#' variable=c("revenue","costs")
#' distribution=c("posnorm","posnorm")
#' lower=c(10000,  5000)
#' upper=c(100000, 50000)
#' costBenefitEstimate<-estimate(variable, distribution, lower, upper)
#' # (a) Define the model function without name for the return value:
#' profit<-function(x){
#'  x$revenue-x$costs
#' }
#' # Perform the decision analysis:
#' myAnalysis<-welfareDecisionAnalysis(estimate=costBenefitEstimate, 
#'                                     model=profit, 
#'                                     numberOfSimulations=100000,
#'                                     functionSyntax="data.frameNames")
#' # Show the analysis results:
#' print(summary((myAnalysis)))
#' #############################################################
#' # (b) Define the model function with a name for the return value:
#' profit<-function(x){
#'  list(Profit=x$revenue-x$costs)
#' }
#' # Perform the decision analysis:
#' myAnalysis<-welfareDecisionAnalysis(estimate=costBenefitEstimate, 
#'                                     model=profit, 
#'                                     numberOfSimulations=100000,
#'                                     functionSyntax="data.frameNames")
#' # Show the analysis results:
#' print(summary((myAnalysis)))
#' #############################################################
#' # (c) Two decsion variables:
#' decisionModel<-function(x){
#'  list(Profit=x$revenue-x$costs,
#'    Costs=-x$costs)
#' }
#' # Perform the decision analysis:
#' myAnalysis<-welfareDecisionAnalysis(estimate=costBenefitEstimate, 
#'                                     model=decisionModel, 
#'                                     numberOfSimulations=100000,
#'                                     functionSyntax="data.frameNames")
#' # Show the analysis results:
#' print(summary((myAnalysis)))
#' @seealso \code{\link{mcSimulation}}, \code{\link{estimate}}, \code{\link{summary.welfareDecisionAnalysis}}
#' @export
welfareDecisionAnalysis <- function(estimate, model, numberOfSimulations, functionSyntax="data.frameNames"){
	# Auxiliary functions (ToDo: check!):
	# Expected net loss of project approval
	enlPa <- function(netBenefitSample){
		- mean( netBenefitSample*(netBenefitSample<0) )
	}

	# Expected net loss of status quo
	enlSq <- function(netBenefitSample){
		mean( netBenefitSample*(netBenefitSample>0) )
	}
	# Expected opportunity loss
	eol <- function(netBenefitSample){
		enlPa_ <- enlPa(netBenefitSample)
		enlSq_ <- enlSq(netBenefitSample)
		min(enlPa_,enlSq_)
	}
	# Return object:
	thisAnalysis<-NULL
	if ( is.function(model) ) {
		# Perform the Monte Carlo simulation:
		mcResult<-mcSimulation( estimate=estimate, 
														model_function=model, 
														numberOfSimulations=numberOfSimulations,
														functionSyntax=functionSyntax)
		# Expected net benefit:
		enb_<-colMeans(mcResult$y)
		# Expected net loss for project aproval:
		enlPa_<-apply(X=mcResult$y, MARGIN=2, FUN=enlPa)
		# Expected net loss for status quo:
		enlSq_<-apply(X=mcResult$y, MARGIN=2, FUN=enlSq)
		# Expected oportunity loss:
		eol_ <-pmin(enlPa_,enlSq_)
		# The optimal choice (either project aproval (PA) or the status quo (SQ)):
		optimalChoice_<-ifelse( eol_==enlPa_, "PA", "SQ")
		# Fill return object:
		thisAnalysis$call<-match.call()
		thisAnalysis$mcResult
		thisAnalysis$enb<-enb_
		thisAnalysis$enlPa<-enlPa_
		thisAnalysis$enlSq<-enlSq_
		thisAnalysis$eol<-eol_
		thisAnalysis$optimalChoice<-optimalChoice_
	} else if ( is.list(model) ){
		stop("The general case of two valuation model functions for project approval and status quo, 
				 respectively is not implemented, yet!")
	} else {
		stop("model must be either a function or a list of two functions.")
	}
	class(thisAnalysis) <- "welfareDecisionAnalysis"
	return(thisAnalysis)
}
##############################################################################################
# summary.welfareDecisionAnalysis(object, ...)
##############################################################################################
#' Summarize Decsion Analysis Results.
#' 
#' summary.welfareDecisionAnalysis produces result summaries of the results of decision analysis
#'  simulation obtained by the function \code{\link{welfareDecisionAnalysis}}.
#' @param object An object of class \code{welfareDecisionAnalysis}.
#' @param ... Further arguments passed to \code{\link{format}}.
#' @inheritParams base::format
#' @return An object of class \code{summary.welfareDecisionAnalysis}.
#' @seealso \code{\link{welfareDecisionAnalysis}}, \code{\link{print.summary.welfareDecisionAnalysis}}, \code{\link{format}}
#' @export
summary.welfareDecisionAnalysis <- function(object,
																		 ...,
																		 digits = max(3, getOption("digits")-3)){	
	summaryDf<-data.frame(enb=object$enb, 
												enlPa=object$enlPa, 
												enlSq=object$enlSq, 
												eol=object$eol, 
												optimalChoice=object$optimalChoice)	
	summaryDf<-format(x=summaryDf, digits=digits, ...)
	res<-list(summary=summaryDf,
						call=object$call)
	
	class(res)<-"summary.welfareDecisionAnalysis"
	res
}
##############################################################################################
# print.summary.welfareDecisionAnalysis(x, ...)
##############################################################################################
#' Print the Summarized Decsion Analysis Results..
#' 
#' This function prints the summary of of \code{welfareDecisionAnalysis} obtained by \code{\link{summary.welfareDecisionAnalysis}}.
#' @param x An object of class \code{summary.welfareDecisionAnalysis}.
#' @param ... Further arguments #ToDo
#' @seealso \code{\link{welfareDecisionAnalysis}}
#' @export
print.summary.welfareDecisionAnalysis <- function(x, ...){
	cat("Call:\n")
	print(x$call)
	cat("\nSummary of decision analysis:\n")
	print(x$summary,...)
}
