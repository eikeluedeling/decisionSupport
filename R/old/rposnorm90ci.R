#
# file: rposnorm90ci.R
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
#' @include paramtnormci_numeric.R
#' @include paramtnormci_fit.R
NULL
##############################################################################################
# rposnorm90ci(n, lower, upper, relativeTolerance)
##############################################################################################
#' 90\%-confidence interval based positive normal random number generation.
#' 
#' This function generates positive normal random numbers based on the 90\% confidence interval
#' calculating the distribution parameter numerically from the  90\%-confidence interval or via a 
#' fit on the 90\%-confidence interval. The fit might include the median or not.
#' @param n Number of generated observations.
#' @param lower \code{numeric}; lower bound of the 90\% confidence intervall.
#' @param median if \code{NULL}: truncated normal is fitted only to lower and upper value of the 
#'   confidence interval; if \code{numeric}: truncated normal is fitted on the confidence interval 
#'   and the median simultaneously. For details cf. below. This option is only relevant if 
#'   \code{method="fit"}.
#' @param upper \code{numeric}; upper bound of the 90\% confidence intervall.
#' @param method method used to determine the parameters of the positive normal; possible methods 
#'   are \code{"numeric"} (the default) and \code{"fit"}.
#' @param relativeTolerance \code{numeric}; the relative tolerance level of deviation of the
#'   generated confidence interval from the specified interval. If this deviation is greater than
#'   \code{relativeTolerance} a warning is given.
#' @param ... further parameters to be passed to \code{\link{paramtnormci_numeric}} or 
#'   \code{\link{paramtnormci_fit}}, respectively.
#' @details
#' For the implementation of \code{method="numeric"}: \code{\link{paramtnormci_numeric}}; 
#'  for the implementation of \code{method="fit"}: \code{\link{paramtnormci_fit}}.
#' @seealso For the implementation of \code{method="numeric"}: \code{\link{paramtnormci_numeric}}; 
#'  for the implementation of \code{method="fit"}: \code{\link{paramtnormci_fit}}.
#' @export
rposnorm90ci <- function(n, lower, median=mean(c(lower,upper)), upper, method="numeric", relativeTolerance=0.05,...){
  # Constants:
  p=c(0.05, 0.95)
  lowerTrunc=0
  upperTrunc=Inf
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
  # Calculate mean and sd corresponding to confidence interval according to method:
  if( method=="numeric" )
  param<-paramtnormci_numeric(p=p, ci=ci, lowerTrunc=lowerTrunc, upperTrunc=upperTrunc, 
                                                  relativeTolerance=relativeTolerance)
  else if(method=="fit")
    param<-paramtnormci_fit(p=p, ci=ci, median=median, lowerTrunc=lowerTrunc, upperTrunc=upperTrunc, 
                                relativeTolerance=relativeTolerance)
  else
    stop("method=\"", method, "\" does not exist!")
  #param<-paramposnorm90ci(lower=ci[["lower"]], upper=ci[["upper"]], relativeTolerance=relativeTolerance, method="numeric")
  # Generate the random numbers:
  x<-msm::rtnorm(n=n,
                 mean=param$mean,
                 sd=param$sd,
                 lower=lowerTrunc,
                 upper=upperTrunc)
  #Return the generated positive normal random numbers:
  x
}
