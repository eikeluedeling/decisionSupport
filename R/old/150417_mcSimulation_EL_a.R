<<<<<<< HEAD
#
# file: mcSimulation.R
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
#' @include estimate.R
NULL
##############################################################################################
# mcSimulation(estimate, model_function, numberOfSimulations, ...)
##############################################################################################
#' Perform a Monte Carlo simulation.
#' 
#' This function generates a random sample of an output distribution defined as the transformation 
#' of an input distribution by a mathematical model, i.e. a mathematical function. This is called a
#' Monte Carlo simulation. For details cf. below.
#' @param estimate \code{estimate}: estimate of the joint probability distribution of
#'   the input variables.
#' @param model_function \code{function}: The function that transforms the input distribution. It 
#'   has to return a single \code{numeric} value or a \code{list} with named \code{numeric} values.
#' @param  ... Optional arguments of \code{model_function}.
#' @param numberOfSimulations The number of Monte Carlo simulations to be run.
#' @param randomMethod \code{character}: The method to be used to sample the distribution
#'   representing the input estimate. For details see option \code{method} in 
#'   \code{\link{random.estimate}}.
#' @param functionSyntax \code{character}: The syntax which has to be used to implement the model
#'   function. Possible values are \code{"globalNames"}, \code{"data.frameNames"} or 
#'   \code{"matrixNames"}. Details are given below.
# @param ... Optional arguments to be passed to \code{\link{random}.
#' @details 
#' This method solves the following problem. Given a multivariate random variable \eqn{x =
#' (x_1,\ldots,x_k)} with joint probability distribution \eqn{P}, i.e. 
#'   \deqn{x \sim P.}{x ~ P} 
#' Then the continuous function 
#'   \deqn{f:R^k \rightarrow R^l, y = f(x)}{f:R^k --> R^l, y = f(x)} 
#' defines another random variable with distribution 
#'   \deqn{y \sim f(P).}{y ~ f(P).}
#' Given a probability density \eqn{\rho} of x that defines \eqn{P} the problem is the determination 
#' of the probability density \eqn{\phi} that defines \eqn{f(P)}. This method samples the 
#' probability density \eqn{\phi} of \eqn{y} as follows: The input distribution \eqn{P} is provided 
#' as \code{estimate}. From \code{estimate} a sample \code{x} with \code{numberOfSimulations} is  
#' generated using \code{\link{random.estimate}}. Then the function values \eqn{y=f(x)} are 
#' calculated, where \eqn{f} is \code{model_function}.
#' 
#' \code{functionSyntax} defines the syntax of \code{model_function}, which has to be used, as 
#' follows:
#' \describe{
#'   \item{\code{"globalNames"}}{
#'     \code{model_function} is constructed, e.g. like this:
#'        \preformatted{
#'          profit<-function(){
#'            revenue-costs
#'          }
#'        }
#'        \emph{CAVE}: this implementation is currently slow!
#'   }
#'   \item{\code{"data.frameNames"}}{
#'      The model function is constructed, e.g. like this:
#'        \preformatted{
#'          profit<-function(x){
#'            x[["revenue"]]-x[["costs"]]
#'          }
#'        }
#'        or like this:
#'        \preformatted{
#'          profit<-function(x){
#'            x$revenue-x$costs
#'          }
#'        }
#'      }
#'      \item{\code{"matrixNames"}}{
#'         The model function is constructed, e.g. like this:
#'         \preformatted{
#'            profit<-function(x){
#'              x[,"revenue"]-x[,"costs"]
#'            }
#'         }     
#'      }
#'    }
#' @return An object of \code{class mcSimulation}, which is a \code{list} with elements:
#'   \describe{
#'      \item{\code{$x}}{
#'         \code{data.frame} containing the sampled \eqn{x -} (input) values which are generated 
#'         from \code{estimate}.
#'      }
#'      \item{\code{$y}}{
#'        \code{data.frame} containing the simulated \eqn{y -} (output) values, i.e. the model 
#'        function values for \code{x}.
#'      }
#'   } 
#' @examples
#'  #############################################################
#'  # Example 1 (Creating the estimate from the command line):
#'  #############################################################
#'  # Create the estimate object:
#'  variable=c("revenue","costs")
#'  distribution=c("norm","norm")
#'  lower=c(10000,  5000)
#'  upper=c(100000, 50000)
#'  costBenefitEstimate<-as.estimate(variable, distribution, lower, upper)
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
		function_results<-apply(t(sapply(apply(x,1,model_function,varnames=varnames),c)),2,as.numeric)
    for(i in 1:ncol(function_results))
      y[[colnames(function_results)[i]]]<-function_results[,i]
    
#		for(j in 1:nrow(x)){
#			for(i in row.names(estimate)){
#				assign(i,x[j,i],envir=parent.frame())
#				assign(i,x[j,i],envir=environment(model_function))
#			}
#			y<-rbind(y,data.frame(model_function()))
		

#		warning("functionSyntax=\"globalNames\" not tested, yet!")
	} else 
		stop("functionSyntax=",functionSyntax, "is not defined!") 
	# 	# Remove names in y if any.
	# 	names(y)<-NULL
	#	returnObject<-data.frame(y=y, x=x)
	#	returnObject<-list(y=y, x=x)
	returnObject<-list(y=data.frame(y), x=data.frame(x))
	returnObject$call<-match.call()
	# ToDo: is this better?:
	# class(returnObject)<-c("mcSimulation", class(returnObject))
	class(returnObject)<-cbind("mcSimulation", class(returnObject))
	
	return(returnObject)
}
##############################################################################################
# as.data.frame.mcSimulation(x, row.names, optional, ..., stringsAsFactors)
##############################################################################################
#' Coerce Monte Carlo simutlation results to a data frame.
#' 
#' Coerces Monte Carlo simutlation results to a data frame.
#' @param x An object of class \code{mcSimulation}.
#' @inheritParams base::as.data.frame
#' @seealso \code{\link{as.data.frame}}
#' @export
as.data.frame.mcSimulation <- function(x, row.names = NULL, optional = FALSE, ..., 
																			 stringsAsFactors = NA){
  if(is.na(stringsAsFactors)) {
    stringsAsFactors <-
      if(getRversion() < "4.1.0")   default.stringsAsFactors()
        else FALSE}
  
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
#' @param ... Further arguments to be passed to \code{\link{print.data.frame}}.
#' @seealso \code{\link{mcSimulation}}, \code{\link{print.data.frame}}
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
#' Summarize results from Monte Carlo simulation.
#' 
#' A summary of the results of a Monte Carlo simulation obtained by the function 
#' \code{\link{mcSimulation}} is produced.
#' @param object An object of class \code{mcSimulation}.
#' @param ... Further arguments passed to \code{\link{summary.data.frame}} (\code{classicView=TRUE})
#'   or \code{\link{format}} (\code{classicView=FALSE}).
#' @inheritParams base::format
#' @param variables.y \code{character} or \code{character vector}: Names of the components of the
#'   simulation function (\code{model_function}) which results shall be displayed. Defaults to all
#'   components.
#' @param variables.x \code{character} or \code{character vector}: Names of the components of the
#'   input variables to the simulation function, i.e. the names of the variables in the input
#'   \code{estimate} which random sampling results shall be displayed. Defaults to all components.
#' @param classicView \code{logical}: if \code{TRUE} the results are summarized using
#'   \code{\link{summary.data.frame}}, if \code{FALSE} further output is produced and the quantile
#'   information can be chosen. Cf. section Value and argument \code{probs}. Default is
#'   \code{FALSE}.
#' @param probs \code{numeric vector}: quantiles that shall be displayed if 
#'   \code{classicView=FALSE}.
#' @return An object of class \code{summary.mcSimulation}.
#' @seealso \code{\link{mcSimulation}}, \code{\link{print.summary.mcSimulation}}, \code{\link{summary.data.frame}}
#' @export
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
#' Print the summary of a Monte Carlo simulation.
#' 
#' This function prints the summary of of \code{mcSimulation} obtained by \code{\link{summary.mcSimulation}}.
#' @param x An object of class \code{mcSimulation}.
#' @param ... Further arguments to be passed to \code{\link{print.data.frame}}.
#' @seealso \code{\link{mcSimulation}}, \code{\link{summary.mcSimulation}}, 
#'   \code{\link{print.data.frame}}
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
#' Plot Histogram of results of a Monte Carlo Simulation
#' 
#' This function plots the histograms of the results of
#' \code{\link{mcSimulation}}.
#' @param x An object of class \code{mcSimulation}.
#' @param xlab \code{character}: x label of the histogram. If it is not
#'   provided, i.e. equals \code{NULL} the name of the chosen variable by
#'   argument \code{resultName} is used.
#' @param main \code{character}: main title of the histogram.
#' @inheritParams graphics::hist
#' @param ... Further arguments to be passed to \code{\link[graphics]{hist}}.
#' @param colorQuantile \code{character vector}: encoding the colors of the 
#'   quantiles defined in argument \code{colorProbability}.
#' @param colorProbability \code{numeric vector}: defines the quantiles that 
#'   shall be distinguished by the colors chosen in argument 
#'   \code{colorQuantile}. Must be of the same length as \code{colorQuantile}.
#' @param resultName \code{character}: indicating the name of the component of
#'   the simulation function (\code{model_function}) which results histogram
#'   shall be generated. If \code{model_function} is single valued, no name
#'   needs to be supplied. Otherwise, one valid name has to be specified.
#'   Defaults to \code{NULL}.
#' @return an object of class "\code{histogram}". For details see 
#'   \code{\link[graphics]{hist}}.
#' @seealso \code{\link{mcSimulation}}, \code{\link{hist}}. For a list of colors
#'   available in R see \code{\link[grDevices]{colors}}.
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
				stop("No component of the model function chosen!")
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



define_variables<-function(varnames)
{tt<-table(varnames)
 for (t in 1:length(tt))
   assign(names(tt[t]),as.numeric(x[which(varnames==names(tt[t]))]))
 }

=======
#
# file: mcSimulation.R
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
#' @include estimate.R
NULL
##############################################################################################
# mcSimulation(estimate, model_function, numberOfSimulations, ...)
##############################################################################################
#' Perform a Monte Carlo simulation.
#' 
#' This function generates a random sample of an output distribution defined as the transformation 
#' of an input distribution by a mathematical model, i.e. a mathematical function. This is called a
#' Monte Carlo simulation. For details cf. below.
#' @param estimate \code{estimate}: estimate of the joint probability distribution of
#'   the input variables.
#' @param model_function \code{function}: The function that transforms the input distribution. It 
#'   has to return a single \code{numeric} value or a \code{list} with named \code{numeric} values.
#' @param  ... Optional arguments of \code{model_function}.
#' @param numberOfSimulations The number of Monte Carlo simulations to be run.
#' @param randomMethod \code{character}: The method to be used to sample the distribution
#'   representing the input estimate. For details see option \code{method} in 
#'   \code{\link{random.estimate}}.
#' @param functionSyntax \code{character}: The syntax which has to be used to implement the model
#'   function. Possible values are \code{"globalNames"}, \code{"data.frameNames"} or 
#'   \code{"matrixNames"}. Details are given below.
# @param ... Optional arguments to be passed to \code{\link{random}.
#' @details 
#' This method solves the following problem. Given a multivariate random variable \eqn{x =
#' (x_1,\ldots,x_k)} with joint probability distribution \eqn{P}, i.e. 
#'   \deqn{x \sim P.}{x ~ P} 
#' Then the continuous function 
#'   \deqn{f:R^k \rightarrow R^l, y = f(x)}{f:R^k --> R^l, y = f(x)} 
#' defines another random variable with distribution 
#'   \deqn{y \sim f(P).}{y ~ f(P).}
#' Given a probability density \eqn{\rho} of x that defines \eqn{P} the problem is the determination 
#' of the probability density \eqn{\phi} that defines \eqn{f(P)}. This method samples the 
#' probability density \eqn{\phi} of \eqn{y} as follows: The input distribution \eqn{P} is provided 
#' as \code{estimate}. From \code{estimate} a sample \code{x} with \code{numberOfSimulations} is  
#' generated using \code{\link{random.estimate}}. Then the function values \eqn{y=f(x)} are 
#' calculated, where \eqn{f} is \code{model_function}.
#' 
#' \code{functionSyntax} defines the syntax of \code{model_function}, which has to be used, as 
#' follows:
#' \describe{
#'   \item{\code{"globalNames"}}{
#'     \code{model_function} is constructed, e.g. like this:
#'        \preformatted{
#'          profit<-function(){
#'            revenue-costs
#'          }
#'        }
#'        \emph{CAVE}: this implementation is currently slow!
#'   }
#'   \item{\code{"data.frameNames"}}{
#'      The model function is constructed, e.g. like this:
#'        \preformatted{
#'          profit<-function(x){
#'            x[["revenue"]]-x[["costs"]]
#'          }
#'        }
#'        or like this:
#'        \preformatted{
#'          profit<-function(x){
#'            x$revenue-x$costs
#'          }
#'        }
#'      }
#'      \item{\code{"matrixNames"}}{
#'         The model function is constructed, e.g. like this:
#'         \preformatted{
#'            profit<-function(x){
#'              x[,"revenue"]-x[,"costs"]
#'            }
#'         }     
#'      }
#'    }
#' @return An object of \code{class mcSimulation}, which is a \code{list} with elements:
#'   \describe{
#'      \item{\code{$x}}{
#'         \code{data.frame} containing the sampled \eqn{x -} (input) values which are generated 
#'         from \code{estimate}.
#'      }
#'      \item{\code{$y}}{
#'        \code{data.frame} containing the simulated \eqn{y -} (output) values, i.e. the model 
#'        function values for \code{x}.
#'      }
#'   } 
#' @examples
#'  #############################################################
#'  # Example 1 (Creating the estimate from the command line):
#'  #############################################################
#'  # Create the estimate object:
#'  variable=c("revenue","costs")
#'  distribution=c("norm","norm")
#'  lower=c(10000,  5000)
#'  upper=c(100000, 50000)
#'  costBenefitEstimate<-as.estimate(variable, distribution, lower, upper)
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
		function_results<-apply(t(sapply(apply(x,1,model_function,varnames=varnames),c)),2,as.numeric)
    for(i in 1:ncol(function_results))
      y[[colnames(function_results)[i]]]<-function_results[,i]
    
#		for(j in 1:nrow(x)){
#			for(i in row.names(estimate)){
#				assign(i,x[j,i],envir=parent.frame())
#				assign(i,x[j,i],envir=environment(model_function))
#			}
#			y<-rbind(y,data.frame(model_function()))
		

#		warning("functionSyntax=\"globalNames\" not tested, yet!")
	} else 
		stop("functionSyntax=",functionSyntax, "is not defined!") 
	# 	# Remove names in y if any.
	# 	names(y)<-NULL
	#	returnObject<-data.frame(y=y, x=x)
	#	returnObject<-list(y=y, x=x)
	returnObject<-list(y=data.frame(y), x=data.frame(x))
	returnObject$call<-match.call()
	# ToDo: is this better?:
	# class(returnObject)<-c("mcSimulation", class(returnObject))
	class(returnObject)<-cbind("mcSimulation", class(returnObject))
	
	return(returnObject)
}
##############################################################################################
# as.data.frame.mcSimulation(x, row.names, optional, ..., stringsAsFactors)
##############################################################################################
#' Coerce Monte Carlo simutlation results to a data frame.
#' 
#' Coerces Monte Carlo simutlation results to a data frame.
#' @param x An object of class \code{mcSimulation}.
#' @inheritParams base::as.data.frame
#' @seealso \code{\link{as.data.frame}}
#' @export
as.data.frame.mcSimulation <- function(x, row.names = NULL, optional = FALSE, ..., 
																			 stringsAsFactors = NA){
  if(is.na(stringsAsFactors)) {
    stringsAsFactors <-
      if(getRversion() < "4.1.0")  default.stringsAsFactors()
        else FALSE}
  
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
#' @param ... Further arguments to be passed to \code{\link{print.data.frame}}.
#' @seealso \code{\link{mcSimulation}}, \code{\link{print.data.frame}}
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
#' Summarize results from Monte Carlo simulation.
#' 
#' A summary of the results of a Monte Carlo simulation obtained by the function 
#' \code{\link{mcSimulation}} is produced.
#' @param object An object of class \code{mcSimulation}.
#' @param ... Further arguments passed to \code{\link{summary.data.frame}} (\code{classicView=TRUE})
#'   or \code{\link{format}} (\code{classicView=FALSE}).
#' @inheritParams base::format
#' @param variables.y \code{character} or \code{character vector}: Names of the components of the
#'   simulation function (\code{model_function}) which results shall be displayed. Defaults to all
#'   components.
#' @param variables.x \code{character} or \code{character vector}: Names of the components of the
#'   input variables to the simulation function, i.e. the names of the variables in the input
#'   \code{estimate} which random sampling results shall be displayed. Defaults to all components.
#' @param classicView \code{logical}: if \code{TRUE} the results are summarized using
#'   \code{\link{summary.data.frame}}, if \code{FALSE} further output is produced and the quantile
#'   information can be chosen. Cf. section Value and argument \code{probs}. Default is
#'   \code{FALSE}.
#' @param probs \code{numeric vector}: quantiles that shall be displayed if 
#'   \code{classicView=FALSE}.
#' @return An object of class \code{summary.mcSimulation}.
#' @seealso \code{\link{mcSimulation}}, \code{\link{print.summary.mcSimulation}}, \code{\link{summary.data.frame}}
#' @export
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
#' Print the summary of a Monte Carlo simulation.
#' 
#' This function prints the summary of of \code{mcSimulation} obtained by \code{\link{summary.mcSimulation}}.
#' @param x An object of class \code{mcSimulation}.
#' @param ... Further arguments to be passed to \code{\link{print.data.frame}}.
#' @seealso \code{\link{mcSimulation}}, \code{\link{summary.mcSimulation}}, 
#'   \code{\link{print.data.frame}}
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
#' Plot Histogram of results of a Monte Carlo Simulation
#' 
#' This function plots the histograms of the results of
#' \code{\link{mcSimulation}}.
#' @param x An object of class \code{mcSimulation}.
#' @param xlab \code{character}: x label of the histogram. If it is not
#'   provided, i.e. equals \code{NULL} the name of the chosen variable by
#'   argument \code{resultName} is used.
#' @param main \code{character}: main title of the histogram.
#' @inheritParams graphics::hist
#' @param ... Further arguments to be passed to \code{\link[graphics]{hist}}.
#' @param colorQuantile \code{character vector}: encoding the colors of the 
#'   quantiles defined in argument \code{colorProbability}.
#' @param colorProbability \code{numeric vector}: defines the quantiles that 
#'   shall be distinguished by the colors chosen in argument 
#'   \code{colorQuantile}. Must be of the same length as \code{colorQuantile}.
#' @param resultName \code{character}: indicating the name of the component of
#'   the simulation function (\code{model_function}) which results histogram
#'   shall be generated. If \code{model_function} is single valued, no name
#'   needs to be supplied. Otherwise, one valid name has to be specified.
#'   Defaults to \code{NULL}.
#' @return an object of class "\code{histogram}". For details see 
#'   \code{\link[graphics]{hist}}.
#' @seealso \code{\link{mcSimulation}}, \code{\link{hist}}. For a list of colors
#'   available in R see \code{\link[grDevices]{colors}}.
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
				stop("No component of the model function chosen!")
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



define_variables<-function(varnames)
{tt<-table(varnames)
 for (t in 1:length(tt))
   assign(names(tt[t]),as.numeric(x[which(varnames==names(tt[t]))]))
 }

>>>>>>> 48e144dcc6c2a9ad66698d489d3691d829d8cd4d
