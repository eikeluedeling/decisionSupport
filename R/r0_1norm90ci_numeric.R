#
# file: r0_1norm90ci_numeric.R
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
# r0_1norm90ci_numeric(n, lower, upper, relativeTolerance)
##############################################################################################
#' Generate  normal random numbers truncated to \eqn{[0,1]} based on the 90\%-confidence interval.
#' 
#' This function generatesnormal random numbers truncated to \eqn{[0,1]} based on the 90\% confidence interval
#' calculating the distribution parameter numerically from the  90\%-confidence interval. 
#' @param n Number of generated observations.
#' @param lower \code{numeric}; lower bound of the 90\% confidence intervall.
#' @param upper \code{numeric}; upper bound of the 90\% confidence intervall.
#' @param relativeTolerance \code{numeric}; the relative tolerance level of deviation of the generated confidence 
#' interval from the specified interval.
#' @details
#' #ToDo
#' @export
r0_1norm90ci_numeric <- function(n, lower, upper, relativeTolerance=0.05){
	# Check preconditions
	if ( is.null(lower) || is.null(upper) || is.na(lower) || is.na(upper) )
		stop("lower and upper value of the 90%-confidence intervall must be given.")
	# Prepare input variable: types
	ci<-c(lower=as.numeric(lower), upper=as.numeric(upper))
	if ( ci[["lower"]] > ci[["upper"]] )
		stop("lower value must be less than upper value.")
	if ( ci[["lower"]] <= 0)
		stop("lower value must be greater than zero.")
	if ( 1 <= ci[["upper"]] )
		stop("upper value must be less than one.")
	# Create output vector for the random numbers to be generated
	x<-vector(length=n)
	# Calculate mean and sd corresponding to confidence interval:
	param<-paramtnormci(p=c(0.05, 0.95), ci=ci, lowerTrunc=0, upperTrunc=1, relativeTolerance=relativeTolerance, method="numeric")
	# Generate the random numbers:
	x<-rtnorm(n=n,
						mean=param$mean,
						sd=param$sd,
						lower=0,
						upper=1)
	# Check postcondition:
	ci_i <- quantile(x=x, probs=c(0.05, 0.95))
	if( !isTRUE(msg<-all.equal(ci_i, c("5%"=ci[["lower"]], "95%"=ci[["upper"]]), tolerance=relativeTolerance, scale=min(abs(ci)))) )
		warning(msg)
	#Return
	x
}
