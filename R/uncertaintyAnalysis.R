#
# file: uncertaintyAnalysis.R
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
# uncertaintyAnalysis(result_path,input_file,fun,iterations,write_table=TRUE,indicators=FALSE,log_scales=FALSE)
##############################################################################################
#' Uncertainty Analysis Wrapper Function.
#'
#' This function performs a Monte Carlo simulation from input files and analyses the results
#' via Partial Least Squares Regression (PLSR) and calculates the Variable Importance on Projection
#' (VIP). Results are safed as plots.
#' @param inputFileName Path to input csv file, which gives the input \code{\link{estimate}}.
#' @param outputDirectory Path were the result plots and tables are safed.
#' @param modelFunction The model function.
#' @param NumberofSimulations The number of Monte Carlo simulations to be performed.
#' @param randomMethod ToDo
#' @param functionSyntax ToDo
#' @param write_table \code{logical}; If the full Monte Carlo simulation results and PLSR results should be
#'  written to file.
#' @param indicators \code{logical}; If indicator variables should be respected specially.
#' @param log_scales \code{logical}; If the scales in the pls plots should be logarithmic.
#' @param oldInputStandard \code{logical}; If the old input standard should be used
#' 	(\code{\link{estimate_read_csv_old}}).
#' 	@seealso \code{\link{mcSimulation}}, \code{\link{estimate}}, \code{\link{estimate_read_csv}}
#' @export
uncertaintyAnalysis <- function(inputFileName, outputDirectory, modelFunction, NumberofSimulations,
																randomMethod="calculate",	functionSyntax="globalNames",
																write_table=TRUE, indicators=FALSE, log_scales=FALSE,
																oldInputStandard=FALSE){
	# Read estimate from file:
	if(!oldInputStandard){
		#	print("newInputStandard")
		estimateObject<-estimate_read_csv(fileName=input_path_coop)
	}else{
		#	print("oldInputStandard")
		estimateObject<-estimate_read_csv_old(fileName=input_path_coop)
	}
	# Run Monte Carlo simulation:
	mcResults<-mcSimulation(estimate=estimateObject,
													model_function=modelFunction,
													numberOfSimulations=NumberofSimulations,
													randomMethod=randomMethod,
													functionSyntax=functionSyntax)


	# Write histogram of results to png files:
	if ( !file.exists(outputDirectory) )
		dir.create(outputDirectory, recursive=TRUE)
	for(i in names(mcResults$y)) {
		png(file.path(outputDirectory, paste(i, "_distribution.png",sep="")), width=1000, height=500)
		par(mar=c(5.1,5.1,4.1,2.1))
		hist(mcResults, lwd=3, cex.lab=2 ,cex.axis=2, prob=TRUE, resultName=i)
		dev.off()
	}
	# Write the summary of the resulting distributions to file:
	mcSummary<-summary(mcResults, digits=2)
	write.csv(mcSummary$summary,file.path(outputDirectory,"summary_cooperation.csv"))
}
