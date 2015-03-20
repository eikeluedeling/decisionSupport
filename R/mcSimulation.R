#
# file: mcSimulation.R
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
# mcSimulation(estimate, model_function, numberOfSimulations, ...)
##############################################################################################
#' Perform a Monte Carlo Simulation.
#' 
#' This method solves the following problem. Given a multivariate random variable 
#' \eqn{x = (x_1,\ldots,x_k)} with joint probability distribution \eqn{P}, i.e.
#' \deqn{x ~ \sim P.}{x ~ P} Then the continuous function 
#'  \deqn{f:R^k \rightarrow R^l, y = f(x)}{f:R^k --> R^l, y = f(x)}
#' defines another random variable with distribution 
#'  \deqn{y \sim f(P).}{y ~ f(P).} 
#' Given a probability density \eqn{\rho} of x that defines \eqn{P} the problem is the determination of the
#' probability density \eqn{\phi} that defines \eqn{f(P)}. This method samples the probability density \eqn{\phi} 
#' of \eqn{y} by Monte Carlo simulation.
#' 
#' @param estimate Filename or  estimate object representing the joint probability distribution of the input variables.
#' @param model_function A numeric function; The function that describes the value of a certain project.
#' @param  ... Optional arguments of \code{model_function}. 
#' @param numberOfSimulations The number of Monte Carlo simulations to be run. 
#' @param randomMethod \code{character}. The method to be used to sample the distribution representing the input estimate.
#' @param functionSyntax \code{character}. The syntax which has to be used to implement the model function. Possible 
#'    values are \code{globalNames}, \code{data.frameNames} or \code{matrixNames}. Details are given below.
# @param ... Optional arguments to be passed to \code{\link{random}.
#' @return An object of class \code{mcSimulation}.
#' \tabular{ll}{
#'  \code{phi} \tab an l-variate probability distribution\cr
#'  \code{x}   \tab a dataframe containing the sampled \eqn{x -} values\cr
#'  \code{y}   \tab a dataframe containing the simulated \eqn{y -} values
#' }
#' @details If \code{functionSyntax="globalNames"}, the variable names used in the definition of \code{model_function} have
#'  to be defined globally. \code{model_function} has to be of the form \code{function(x,varnames)}. If 
#'  \code{functionSyntax="data.frameNames"}, the model function is constructed, e.g. like this:\cr 
#'  \code{
#'    profit<-function(x){
#'      x[["revenue"]]-x[["costs"]]
#'    }
#'  }
#'  or like this:
#'  \code{
#'    profit<-function(x){
#'      x$revenue-x$costs
#'    }
#'  }
#'  If \code{functionSyntax="matrixNames"}, the model function is constructed, e.g. like this:\cr 
#'  \code{
#'    profit<-function(x){
#'      x[,"revenue"]-x[,"costs"]
#'    }
#'  }
#'  
#' @examples
#'  #############################################################
#'  # Example 1 (Creating the estimate from the command line):
#'  #############################################################
#'  # Create the estimate object:
#'  variable=c("revenue","costs")
#'  distribution=c("norm","norm")
#'  lower=c(10000,  5000)
#'  upper=c(100000, 50000)
#'  costBenefitEstimate<-estimate(variable, distribution, lower, upper)
#'  # (a) Define the model function without name for the return value:
#'  profit1<-function(x){
#'    x$revenue-x$costs
#'  }
#'  # Perform the Monte Carlo simulation:
#'  predictionProfit1<-mcSimulation( estimate=costBenefitEstimate, 
#'                                  model_function=profit1, 
#'                                  numberOfSimulations=100000,
#'                                  functionSyntax="data.frameNames")
#'  # Show the simulation results:
#'  print(summary(predictionProfit1))
#'  hist(predictionProfit1,xlab="Profit")
#'  #############################################################
#'  # (b) Define the model function with a name for the return value:
#'  profit1<-function(x){
#'    list(Profit=x$revenue-x$costs)
#'  } 
#'  # Perform the Monte Carlo simulation:
#'  predictionProfit1<-mcSimulation( estimate=costBenefitEstimate, 
#'                                  model_function=profit1, 
#'                                  numberOfSimulations=100000,
#'                                  functionSyntax="data.frameNames")
#'  # Show the simulation results:
#'  print(summary(predictionProfit1, classicView=TRUE))
#'  hist(predictionProfit1) 
#'  #########################################################
#'  # (c) Using global names in the model function syntax
#'  #	(CAVE: currently slow!):
#'  profit1<-function(){
#'    list(Profit=revenue-costs)
#'  } 
#'  # Perform the Monte Carlo simulation:
#'  predictionProfit1<-mcSimulation( estimate=costBenefitEstimate, 
#'                                  model_function=profit1, 
#'                                  numberOfSimulations=10000,
#'                                  functionSyntax="globalNames")
#'  # Show the simulation results:
#'  print(summary(predictionProfit1, probs=c(0.05,0.50,0.95)))
#'  hist(predictionProfit1) 
#'   
#'  #############################################################
#'  # Example 2(Reading the estimate from file):
#'  #############################################################
#'  # Define the model function:
#'  profit2<-function(x){
#'    Profit<-x[["sales"]]*(x[["productprice"]] - x[["costprice"]])
#'    list(Profit=Profit)
#'  }  
#'  # Read the estimate of sales, productprice and costprice from file:
#'  inputFileName=system.file("extdata","profit-4.csv",package="decisionSupport")
#'  parameterEstimate<-estimate_read_csv(fileName=inputFileName)
#'  print(parameterEstimate)
#'  # Perform the Monte Carlo simulation:
#'  predictionProfit2<-mcSimulation( estimate=parameterEstimate, 
#'                                  model_function=profit2, 
#'                                  numberOfSimulations=100000,
#'                                  functionSyntax="data.frameNames")
#'  # Show the simulation results:
#'  print(summary(predictionProfit2))
#'  hist(predictionProfit2) 
#'  @seealso \code{\link{print.mcSimulation}}, \code{\link{summary.mcSimulation}}, \code{\link{hist.mcSimulation}}, 
#'  \code{\link{estimate}}, \code{\link{random.estimate}}
#' @export
mcSimulation <- function(estimate, model_function, ..., numberOfSimulations, randomMethod="calculate", functionSyntax="data.frameNames"){
	#ToDo: (i) review code and (ii) test
	x<-random(rho=estimate, n=numberOfSimulations, method=randomMethod)
	if (functionSyntax=="data.frameNames"){
		y<-model_function(as.data.frame(x), ...)
	} else if (functionSyntax=="matrixNames"){
		y<-model_function(as.matrix(x),...)
	} else if (functionSyntax=="globalNames"){
		#ToDo: test and benchmark
		y<-NULL
		for(j in 1:nrow(x)){
			for(i in row.names(estimate)){
#				assign(i,x[j,i],envir=parent.frame())
				assign(i,x[j,i],envir=environment(model_function))
			}
			y<-rbind(y,data.frame(model_function()))
		}
		warning("functionSyntax=\"globalNames\" not tested, yet!")
	} else 
		stop("functionSyntax=",functionSyntax, "is not defined!") 
	# 	# Remove names in y if any.
	# 	names(y)<-NULL
	#	returnObject<-data.frame(y=y, x=x)
	#	returnObject<-list(y=y, x=x)
	returnObject<-list(y=data.frame(y), x=data.frame(x))
	returnObject$call<-match.call()
	class(returnObject)<-cbind("mcSimulation", class(returnObject))
	
	return(returnObject)
}
##############################################################################################
# as.data.frame.mcSimulation(x, row.names, optional, ..., stringsAsFactors)
##############################################################################################
#' Coerce to a Data Frame.
#' 
#' Functions to check if an object is a data frame, or coerce it if possible.
#' @param x An object of class \code{mcSimulation}.
#' @param row.names \code{NULL} or a character vector giving the row names for the data frame. 
#'  Missing values are not allowed.
#' @param optional logical. If \code{TRUE}, setting row names and converting column names 
#' (to syntactic names: see \code{\link{make.names}}) is optional.
#' @param ... additional arguments to be passed to or from methods.
#' @param stringsAsFactors logical: should the character vector be converted to a factor?
#' @seealso \code{\link{as.data.frame}}
#' @export
as.data.frame.mcSimulation <- function(x, row.names = NULL, optional = FALSE, ..., 
																			 stringsAsFactors = default.stringsAsFactors()){
	as.data.frame(list(y=x$y,x=x$x), row.names = row.names, optional = optional, ..., 
								stringsAsFactors = stringsAsFactors)
}
##############################################################################################
# print.mcSimulation(x, ...)
##############################################################################################
#' Print Basic Results from Monte Carlo Simulation.
#' 
#' This function prints basic results from Monte Carlo simulation  and returns it invisible.
#' @param x An object of class \code{mcSimulation}.
#' @param ... Further arguments #ToDo
#' @seealso \code{\link{mcSimulation}}
#' @export
print.mcSimulation <- function(x, ...){
	#ToDo: Review
	cat("Call:\n")
	print(x$call)
	cat("\nMonte Carlo simulation results:\n")
	print.data.frame(as.data.frame(x),...)
}
##############################################################################################
# summary.mcSimulation(object, ...)
##############################################################################################
#' Summarize Results from Monte Carlo Simulation.
#' 
#' summary.mcSimulation produces result summaries of the results of a Monte Carlo simulation obtained by the function \code{\link{mcSimulation}}.
#' @param object An object of class \code{mcSimulation}.
#' @param ... Further arguments #ToDo
#' @return An object of class \code{summary.mcSimulation}.
#' @seealso \code{\link{mcSimulation}}, \code{\link{print.summary.mcSimulation}}
#' @export
# summary.mcSimulation <- function(object, ...){
# 	#ToDo: Review
# 	res<-list(summary=summary.data.frame(as.data.frame(object),...),
# 						call=object$call)
# 	class(res)<-"summary.mcSimulation"
# 	res
# }
summary.mcSimulation <- function(object,
																 ...,
																 digits = max(3, getOption("digits")-3),
																 variables.y=names(object$y),
																 variables.x=if(classicView) names(object$x),
																 classicView=FALSE,
																 probs=c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1)
){
	#ToDo: Review
	data<-as.data.frame(list(y=(object$y)[variables.y],x=(object$x)[variables.x]))
	if( classicView ){
		res<-list(summary=summary.data.frame(object=as.data.frame(data),...,digits=digits),
							call=object$call)
	} else{
		chance_loss<-function(x){
			length(x[x<0])/length(x)
		}
		chance_zero<-function(x){
			length(x[x==0])/length(x)
		}
		chance_gain<-function(x){
			length(x[x>0])/length(x)
		}
		
		#		data<-mcResult$y[variables]
		
		summaryDf<-as.data.frame(t(apply(X=data, MARGIN=2, FUN=quantile, probs=probs)))
		summaryDf<-cbind(summaryDf,
										 mean=colMeans(data),
										 deparse.level=1)
		summaryDf<-cbind(summaryDf,
										 chance_loss=apply(X=data, MARGIN=2, FUN=chance_loss),
										 deparse.level=1)
		summaryDf<-cbind(summaryDf,
										 chance_zero=apply(X=data, MARGIN=2, FUN=chance_zero),
										 deparse.level=1)
		summaryDf<-cbind(summaryDf,
										 chance_gain=apply(X=data, MARGIN=2, FUN=chance_gain),
										 deparse.level=1)
		
		summaryDf<-format(x=summaryDf, digits=digits, ...)
		res<-list(summary=summaryDf,
							call=object$call)
	}
	
	class(res)<-"summary.mcSimulation"
	res
}
##############################################################################################
# print.summary.mcSimulation(x, ...)
##############################################################################################
#' Print the Summary of a Monte Carlo Simulation.
#' 
#' This function prints the summary of of \code{mcSimulation} obtained by \code{\link{summary.mcSimulation}}.
#' @param x An object of class \code{mcSimulation}.
#' @param ... Further arguments #ToDo
#' @seealso \code{\link{mcSimulation}}
#' @export
print.summary.mcSimulation <- function(x, ...){
	cat("Call:\n")
	print(x$call)
	cat("\nSummary of Monte Carlo simulation:\n")
	print(x$summary,...)
}
##############################################################################################
# hist.mcSimulation(x, ...)
##############################################################################################
#' Plot histogram of results of a Monte Carlo Simulation.
#' 
#' This function plots the histogram of the results of \code{mcSimulation}.
#' @param x An object of class \code{mcSimulation}.
#' @param ... Further arguments #ToDo
#' @seealso \code{\link{mcSimulation}}, \code{\link{hist}}
#' @export
hist.mcSimulation <- function(x, breaks=100, col=NULL, xlab=NULL, main=paste("Histogram of " , xlab), ...,
															colorQuantile   =c("GREY", "YELLOW", "ORANGE", "DARK GREEN", "ORANGE", "YELLOW", "GREY"), 
															colorProbability=c(1.00,    0.95,     0.75,     0.55,         0.45,     0.25,     0.05),
															resultName=NULL){
	# ToDo: review!!!
	if( is.list(x$y) ){
		if( !is.null(resultName) ){
			result<-x$y[[resultName]]
			if( is.null(xlab) )
				xlab<-resultName
		} else {
			if(length(names(x$y))==1){
				result<-unlist(x$y)
				if( is.null(xlab) )
					xlab<-names(x$y)[[1]]
			}
			else 
				stop("No element of the model function chosen!")
		}
		if( main==paste("Histogram of " , xlab))
			main<-paste("Histogram of " , xlab, " Monte Carlo Simulation")
	} else { 
		result<-x$y
	}
	if(!isTRUE(is.null(colorQuantile))){
		resultNames<-NULL
		if( length(colorQuantile) != length(colorProbability) )
			stop("length(colorQuantile) != length(colorProbability)")
		histPrepare<-hist(result, breaks=breaks, plot=FALSE)
		probability<-cumsum(histPrepare$density * diff(histPrepare$breaks))
		color<-c()
		for( i in seq(along=probability) ){
			for( j in seq(along=colorQuantile) )
				if(probability[i] < colorProbability[j]) color[i]<-colorQuantile[j]
		}	
	} else
		color=col
	hist(result, breaks=breaks, col=color, xlab=xlab, main=main,...)
}
