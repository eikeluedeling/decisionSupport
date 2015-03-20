#
# file: rposnorm90ci_iter.R
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
# rposnorm90ci_iter(n, lower, upper, relativeTolerance, maxIter)
##############################################################################################
#' Generate positive normal random numbers based on the 90\%-confidence interval.
#' 
#' This function generates positive normal random numbers based on the 90\% confidence interval
#' using an iteration algorithm. 
#' @param n Number of generated observations.
#' @param lower \code{numeric}; lower bound of the 90\% confidence intervall.
#' @param upper \code{numeric}; upper bound of the 90\% confidence intervall.
#' @param relativeTolerance \code{numeric}; the relative tolerance level of deviation of the generated confidence 
#' interval from the specified interval.
#' @param maxIter \code{numeric}; maximum number of iterations.
#' @details
#' The generation of random numbers is repeated until the generated 90\% - confidence interval is
#' close enough to the desired value.
#' @export
rposnorm90ci_iter <- function(n, lower, upper,  relativeTolerance=0.05, maxIter=40){
	# Constants:
	# 95%-critical value of standard normal distribution (c_0.95=1.645):
	c_0.95=qnorm(0.95)
	# Check preconditions
	if ( is.null(lower) || is.null(upper) || is.na(lower) || is.na(upper) )
		stop("lower and upper value of the 90%-confidence intervall must be given.")
	# Prepare input variable: types
	ci<-c(lower=as.numeric(lower), upper=as.numeric(upper))
	if ( ci[["lower"]] > ci[["upper"]] )
		stop("lower value must be less than upper value.")
	if ( ci[["lower"]] < 0)
		stop("lower value must be greater than zero.")
	# Create output vector for the random numbers to be generated
	x<-vector(length=n)
	# Initialize loop:
	mean_i <- mean(ci)
	sd_i <- (mean_i - ci[["lower"]])/c_0.95
	ci_i <- c(lower=-Inf, upper=Inf)	
	i<-0	
	# Generate the random numbers until the generated 90\% - confidence interval is
	# close enough to the desired value:
	while( !isTRUE(all.equal(ci_i, ci, tolerance=relativeTolerance, scale=min(abs(ci))  )) && i < maxIter  ){	
		x<-rtnorm(n=n,
							mean=mean_i,
							sd=sd_i,
							lower=0,
							upper=Inf)
		quantiles<- quantile(x=x, probs=c(0.05, 0.95))
		ci_i[["lower"]] <- quantiles[["5%"]]
		ci_i[["upper"]] <- quantiles[["95%"]]
		if( (ci[["lower"]] - ci_i[["lower"]])*( ci_i[["upper"]] - ci[["upper"]]) > 0)
			sd_i <- (ci[["upper"]] - ci[["lower"]])/(ci_i[["upper"]] - ci_i[["lower"]])*sd_i
		else
			mean_i <-  ci_i[["lower"]]/ci[["lower"]] *  mean(ci - ci_i) + mean_i
		i<-i+1
	}
	# Check postcondition:
	if( !isTRUE(msg<-all.equal(ci_i, ci, tolerance=relativeTolerance, scale=min(abs(ci)))) )
		warning(msg)
	#Return
	x
}

