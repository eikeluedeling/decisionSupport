#
# file: rposnorm90ci_numeric.R
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
# rposnorm90ci_numeric(n, lower, upper, relativeTolerance)
##############################################################################################
#' Generate positive normal random numbers based on the 90\%-confidence interval.
#' 
#' This function generates positive normal random numbers based on the 90\% confidence interval
#' calculating the distribution parameter numerically from the  90\%-confidence interval. 
#' @param n Number of generated observations.
#' @param lower \code{numeric}; lower bound of the 90\% confidence intervall.
#' @param upper \code{numeric}; upper bound of the 90\% confidence intervall.
#' @param relativeTolerance \code{numeric}; the relative tolerance level of deviation of the generated confidence 
#' interval from the specified interval.
#' @details
#' #ToDo
#' @export
rposnorm90ci_numeric <- function(n, lower, upper, relativeTolerance=0.05){
	# Constants:
	# 95%-critical value of standard normal distribution (c_0.95=1.645):
#	c_0.95=qnorm(0.95)
	# Check preconditions
	if ( is.null(lower) || is.null(upper) || is.na(lower) || is.na(upper) )
		stop("lower and upper value of the 90%-confidence intervall must be given.")
	# Prepare input variable: types
	ci<-c(lower=as.numeric(lower), upper=as.numeric(upper))
	if ( ci[["lower"]] >= ci[["upper"]] )
		stop("lower value must be less than upper value.")
	if ( ci[["lower"]] <= 0)
		stop("lower value must be greater than zero.")
	# Create output vector for the random numbers to be generated
	x<-vector(length=n)
	# Calculate mean and sd corresponding to confidence interval:
	param<-paramposnorm90ci(lower=ci[["lower"]], upper=ci[["upper"]], relativeTolerance=relativeTolerance, method="numeric")
	# Generate the random numbers:
	x<-rtnorm(n=n,
						mean=param$mean,
						sd=param$sd,
						lower=0,
						upper=Inf)
	#Return
	x
}
