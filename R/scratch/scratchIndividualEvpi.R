# 
# file: scratchIndividualEvpi.R
###################################
# Number of simulations:
n=100000
# Create the current estimate from text:
estimateText<-" variable,  distribution, lower, upper
								revenue1,  posnorm,      100,   1000
								revenue2,	 posnorm,      50,    2000
								costs1,    posnorm,      50,    2000
                costs2,    posnorm,      100,   1000"
currentEstimate<-estimate(read.csv(header=TRUE,text=estimateText, strip.white=TRUE, stringsAsFactors=FALSE))
# The model function:
profitModel <- function(x){
	list(Profit=x$revenue1 + x$revenue2 - x$costs1 - x$costs2)
}

perfectProspectiveNames<-c("revenue1", "costs1", "costs2")

perfectProspectiveValues<-colMeans(random(rho=currentEstimate, n=n)[,perfectProspectiveNames])

prospectiveEstimate<-c()
for( i in perfectProspectiveNames){
	prospectiveEstimate[[i]]<-currentEstimate
	prospectiveEstimate[[i]]$base[i,"distribution"]<-"const"
	prospectiveEstimate[[i]]$base[i,"lower"]<-perfectProspectiveValues[[i]]
	prospectiveEstimate[[i]]$base[i,"upper"]<-perfectProspectiveValues[[i]]
}

# Calculate the Expected Value of Information:
individualEvpiResult<-eviSimulation(model=profitModel,
																		currentEstimate=currentEstimate,
																		prospectiveEstimate=prospectiveEstimate,
																		numberOfSimulations=n,
																		functionSyntax="data.frameNames")
# Show the simulation results:
print(sort(summary(individualEvpiResult)),decreasing=TRUE,along="Profit")
#############################################################################################
##############################################################################################
# eviSimulation(model, currentEstimate, prospectiveEstimate, numberOfSimulations, functionSyntax)
##############################################################################################
#' Expected Value of Individual Perfect Information Simulation
#' 
#' The Expected Value of Individual Perfect Information (Individual EVPI) is obtained by Monte Carlo Simulations.
#' @param model either a function or a list with two functions: \code{list(p1,p2)}. In the first case the function is the 
#' net benefit of project approval vs. the status quo. In the second case the element \code{p1} is the function valuing 
#' the first project and the element \code{p2} valueing the second project.
#' @param currentEstimate \code{\link{estimate}} object describing the distribution of the input variables as currently estmated.
#' @param prospectivePefectNames 
#' @param perfectProspectiveValues 
#' @param numberOfSimulations integer; number of simulations to be used in the underlying Monte Carlo analysis
#' @param functionSyntax function character; function syntax used in the model function(s).
#' @return An object of class \code{eviSimulation} with the following elements:
#'  \tabular{ll}{
#' 			\code{current} \tab \code{\link{decisionAnalysis}} object for \code{currentEstimate}\cr
#' 			\code{prospective} \tab \code{\link{decisionAnalysis}} object  for \code{prospectiveEstimate}\cr
#'  		\code{evi}   \tab  Expected Value of Information (EVI) of gained by the prospective estimate w.r.t. 
#'  								the current estimate
#' }
#' @example
#' # Number of simulations:
#' n=100000
#' # Create the current estimate from text:
#' estimateText<-" variable,  distribution, lower, upper
#' 								revenue1,  posnorm,      100,   1000
#' 								revenue2,	 posnorm,      50,    2000
#' 								costs1,    posnorm,      50,    2000
#'                 costs2,    posnorm,      100,   1000"
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
#' @seealso \code{\link{eviSimultion}}, \code{\link{decisionAnalysis}}, \code{\link{mcSimulation}}, \code{\link{estimate}}
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
	evpiResult<-eviSimulation(model=profitModel,
														currentEstimate=currentEstimate,
														prospectiveEstimate=prospectiveEstimate,
														numberOfSimulations=numberOfSimulations,
														functionSyntax=functionSyntax)
#	class(evpiResult)<-c("individualEvpiSimulation", class(evpiResult))
	evpiResult
}
#############################################################################################

# Calculate the Individual EVPI:
individualEvpiResult<-individualEvpiSimulation(model=profitModel,
																							 currentEstimate=currentEstimate,
																							 numberOfSimulations=n,
																							 functionSyntax="data.frameNames")
# Show the simulation results:
print(sort(summary(individualEvpiResult)),decreasing=TRUE,along="Profit")

