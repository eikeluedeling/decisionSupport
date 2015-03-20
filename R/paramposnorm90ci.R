#
# file: paramposnorm90ci.R
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
# paramposnorm90ci(lower, upper, relativeTolerance, method)
##############################################################################################
#' Return parameters of positive normal random distribution based on the 90\%-confidence interval.
#' 
#' This function calculates the distribution parameters from the  90\%-confidence interval. 
#' @param lower \code{numeric}; lower bound of the 90\% confidence intervall.
#' @param upper \code{numeric}; upper bound of the 90\% confidence intervall.
#' @param relativeTolerance \code{numeric}; the relative tolerance level of deviation of the generated confidence 
#' interval from the specified interval.
#' @param method The method to calculate the parameters. Default is \code{"numeric"}.
#' @details
#' #ToDo
#' @export
paramposnorm90ci <- function(lower, upper, relativeTolerance=0.05, method="numeric"){
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
	if (method=="numeric"){
		# Initialize the initialization of the root finding:
		mean_i <- mean(ci)
		sd_i <- (mean_i - ci[["lower"]])/c_0.95
		ci_i <- c("lower"=NULL, "upper"=NULL)
			# Generate the the initial values for mean  and sd:
		ci_i[["lower"]] <- qtnorm(p=0.05, mean=mean_i, sd=sd_i, lower=0, upper=Inf)
		ci_i[["upper"]] <- qtnorm(p=0.95, mean=mean_i, sd=sd_i, lower=0, upper=Inf)		
		sd_i <- (ci[["upper"]] - ci[["lower"]])/(ci_i[["upper"]] - ci_i[["lower"]])*sd_i
		mean_i <-  ci_i[["lower"]]/ci[["lower"]] *  max(ci - ci_i) + mean_i
		
		# Auxiliary function defining mean and sd by lower and upper by f(x) = 0 
		# (x[1]:=mean, x[2]:=sd): 
		f <- function(x){
			y<-numeric(2)
			y[1]<- qtnorm(p=0.05, mean=x[1], sd=x[2], lower=0, upper=Inf) - ci[["lower"]]
			y[2]<- qtnorm(p=0.95, mean=x[1], sd=x[2], lower=0, upper=Inf) - ci[["upper"]]
			y
		}
		# The root of f are mean and sd:
		#	x_0<-nleqslv(x=c(mean_i, sd_i), fn=f, control=list(maxit=10000))
		x_0<-nleqslv(x=c(mean_i, sd_i), fn=f)	
		mean<-x_0$x[1]
		sd<-x_0$x[2]
		
		# Alternative calculation (depreciated):
		# Calculate the mean and sd from lower and upper:
		# Auxiliary function defining mean and sd by lower and upper by f(x) = 0 
		# (x[1]:=mean/sd, x[2]:=sd): 
		# 	f <- function(x){
		# 		a<-pnorm(q=x[1])
		# 		y<-numeric(2)
		# 		y[1]<- pnorm(q=lower/x[2]-x[1]) + 1.05*a - 1.05
		# 		y[2]<- pnorm(q=upper/x[2]-x[1]) + 1.95*a - 1.95
		# 		y
		# 	}
		# The root of f are mean/sd and sd:
		#	x_0<-nleqslv(x=c(mean_i/sd_i, sd_i), fn=f)	
		#	sd<-x_0$x[2]
		#	mean<-x_0$x[1]*sd	
	} else 
		stop("Not implemented: method=", method)
	# Check postcondition:
	ci_i[["lower"]] <- qtnorm(p=0.05, mean=mean, sd=sd, lower=0, upper=Inf)
	ci_i[["upper"]] <- qtnorm(p=0.95, mean=mean, sd=sd, lower=0, upper=Inf)
	if( !isTRUE(msg<-all.equal(ci_i, ci, tolerance=relativeTolerance, scale=min(abs(ci)))) )
		warning(msg)
	#Return
	list(mean=mean, sd=sd)
}


