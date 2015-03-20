#
# file: individualEvpiSimulation.R
#
# R package: decisionSupport
# 
# Authors (ToDo order?): 
#   Lutz Göhring <lutz.goehring@gmx.de>
#   Eike Luedeling (ICRAF) <E.Luedeling@cgiar.org>
#
# Affiliation: World Agroforestry Centre (ICRAF)
# 
# License: ToDo
#
##############################################################################################
##############################################################################################
# individualEvpiSimulation(model, currentEstimate, perfectProspectiveNames,perfectProspectiveValues,
#                           numberOfSimulations, functionSyntax)
##############################################################################################
#' Individual Expected Value of Perfect Information Simulation
#' 
#' The Individual Expected Value of Perfect Information (Individual EVPI) is calculated based on a Monte Carlo simulation
#' of the values of two different decision alternatives.
#' @param model either a function or a list with two functions: \code{list(p1,p2)}. In the first case the function is the 
#' net benefit of project approval vs. the status quo. In the second case the element \code{p1} is the function valuing 
#' the first project and the element \code{p2} valueing the second project.
#' @param currentEstimate \code{\link{estimate}} object describing the distribution of the input variables as currently estmated.
#' @param perfectProspectiveNames character vector; input variable names that are assumed to be known perfectly with 
#' 				prospective information.
#' @param perfectProspectiveValues numeric vector of the same length as \code{perfectProspectiveNames} with the corresponding
#' 				values assumed to be known perfectly.
#' @param numberOfSimulations integer; number of simulations to be used in the underlying Monte Carlo analysis
#' @param functionSyntax function character; function syntax used in the model function(s).
#' @return An object of class \code{eviSimulation} with the following elements:
#'  \tabular{ll}{
#' 			\code{current} \tab \code{\link{decisionAnalysis}} object for \code{currentEstimate}\cr
#' 			\code{prospective} \tab \code{\link{decisionAnalysis}} object  for \code{prospectiveEstimate}\cr
#'  		\code{evi}   \tab  Expected Value of Information (EVI) of gained by the prospective estimate w.r.t. 
#'  								the current estimate
#' }
#' @details This principle is along the line described in Hubbard (2014). The Expected Value of Information is the decrease in the EOL
#'  for an information improvement from the current estimate (I_current) to a better prospective (or hypothetical) information (I_prospective):
#'   EVI := EOL(I_current) - EOL(I_prospective). If one variables under I_prospective is assumed to be known with certainty the EVI
#'    is called the Individual Expected Value of Perfect Information (Individual EVPI). More precisely, if one assumes under 
#'    I_prospective to perfectly know (x_1, ..., x_k) to equal (a_1, ..., a_k) then one can specify the notation as  Individual EVPI[x_i = a_i]. 
#'   Summarizing, the Individual EVPI depends on the model for valueing a decision, the current information, i.e. the current estimate, 
#'   and the specification of the variable that is assumed to be known with certainty, viz. the improvement in information, i.e. a prospective
#'    estimate. 
#' @examples
#' # Number of simulations:
#' n=100000
#' # Create the current estimate from text:
#' estimateText<-"variable,  distribution, lower, upper
#' 								revenue1,  posnorm,      100,   1000
#' 								revenue2,	 posnorm,      50,    2000
#' 								costs1,    posnorm,      50,    2000
#'                costs2,    posnorm,      100,   1000"
#' currentEstimate<-estimate(read.csv(header=TRUE,text=estimateText, strip.white=TRUE, stringsAsFactors=FALSE))
#' # The model function:
#' profitModel <- function(x){
#' 	list(Profit=x$revenue1 + x$revenue2 - x$costs1 - x$costs2)
#' }
#' # Calculate the Individual EVPI:
#' individualEvpiResult<-individualEvpiSimulation(model=profitModel,
#' 																							 currentEstimate=currentEstimate,
#' 																							 numberOfSimulations=n,
#' 																							 functionSyntax="data.frameNames")
#' # Show the simulation results:
#' print(sort(summary(individualEvpiResult)),decreasing=TRUE,along="Profit")
#' @seealso \code{\link{eviSimulation}}, \code{\link{decisionAnalysis}}, \code{\link{mcSimulation}}, \code{\link{estimate}}
#' @export
individualEvpiSimulation <- function(model, currentEstimate, 
																		 perfectProspectiveNames=row.names(currentEstimate),
																		 perfectProspectiveValues=colMeans(random(rho=currentEstimate, n=numberOfSimulations)[,perfectProspectiveNames]),
																		 numberOfSimulations,
																		 functionSyntax="data.frameNames"){
	prospectiveEstimate<-c()
	for( i in perfectProspectiveNames){
		prospectiveEstimate[[i]]<-currentEstimate
		prospectiveEstimate[[i]]$base[i,"distribution"]<-"const"
		prospectiveEstimate[[i]]$base[i,"lower"]<-perfectProspectiveValues[[i]]
		prospectiveEstimate[[i]]$base[i,"upper"]<-perfectProspectiveValues[[i]]
	}
	
	# Calculate the Expected Value of Perfect Information:
	evpiResult<-eviSimulation(model=model,
														currentEstimate=currentEstimate,
														prospectiveEstimate=prospectiveEstimate,
														numberOfSimulations=numberOfSimulations,
														functionSyntax=functionSyntax)
	#	class(evpiResult)<-c("individualEvpiSimulation", class(evpiResult))
	evpiResult
}


