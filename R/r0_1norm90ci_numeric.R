#
# file: r0_1norm90ci_numeric.R
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
#' @include paramtnormci.R
NULL
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
	x<-msm::rtnorm(n=n,
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
