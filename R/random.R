# 
# file: random.R
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
#' @include rdistq_fit.R
NULL
##############################################################################################
# generic: random(rho,n,method,...)
# ToD: rename to rdistq or rq?
##############################################################################################
#' Quantiles based generic random number generation.
#' 
#' This function generates random numbers for parametric distributions, parameters of which are 
#' determined by given quantiles. 
#'  @export
random <- function(rho,n,method, relativeTolerance, ...) UseMethod("random")
##############################################################################################
# random.default(rho,n,method,...)
##############################################################################################
#' Quantiles based generic univariate random number generation.
#' 
#' The default method generates univariate random numbers specified by arbitrary quantiles.
#' @param rho \code{list} Distribution to be randomly sampled. The list elements are 
#'  \code{distribution}, \code{probabilities} and \code{quantiles}. For details cf. below.
#' @param n \code{integer} Number of observations to be generated
#' @param method \code{character} Particular method to be used for random number generation. 
#' Currently only method \code{\link{rdistq_fit}{fit}} is implemented which is the default.
#' @param relativeTolerance \code{numeric}; the relative tolerance level of deviation of the
#'   generated confidence interval from the specified interval. If this deviation is greater than
#'   \code{relativeTolerance} a warning is given.
#' @param ... Optional arguments to be passed to the particular random number
#'  generating function, i.e. \code{\link{rdistq_fit}}.
#' @details 
#'   The distribution family is determined by \code{rho[["distribution"]]}. For the possibilities 
#'   cf. \code{\link{rdistq_fit}}.
#' 
#'   \code{rho[["probabilities"]]} and \code{[[rho"quantiles"]]} are numeric vectors of the same 
#'   length. The first defines the probabilites of the quantiles, the second defines the quantiles 
#'   values which determine the parametric distribution.
#' @return 
#'  A numeric vector of length \code{n} containing the generated random numbers.  
#'  @describeIn random Univariate random number generation.
#' @examples
#'  x<-random(n=10000)
#'  hist(x,breaks=100)
#'  mean(x)
#'  sd(x)
#'   
#'  rho<-list(distribution="norm", 
#'            probabilities=c(0.05,0.4,0.8), 
#'            quantiles=c(-4, 20, 100))
#'  x<-random(rho=rho, n=10000, tolConv=0.01)
#'  hist(x,breaks=100)
#'  quantile(x,p=rho[["probabilities"]])
#'  @seealso \code{\link{rdistq_fit}}
#' @export
random.default <- function(rho=list(distribution="norm", probabilities=c(0.05,0.95), quantiles=c( -qnorm(0.95), qnorm(0.95) )), 
                           n, method="fit", relativeTolerance=0.05, ...){
  # Check preconditions:
  if ( !is.list(rho) )
    stop("rho must be a list with elements \"distribution\", \"probabilities\" and \"quantiles\"")
  if( is.null(rho[["distribution"]]) )
    stop("rho[[\"distribution\"]] must be supplied.")
    if( is.null(rho[["probabilities"]]) || !all(!is.na(as.numeric(rho[["probabilities"]]))) )
      stop("rho[\"probabilities\"] must be supplied.")
  if( is.null(rho[["quantiles"]]) || !all(!is.na(as.numeric(rho[["quantiles"]]))) )
    stop("rho[\"quantiles\"] must be supplied.")
  if( length(rho[["probabilities"]])!=length(rho[["quantiles"]]) )
    stop( "length(rho[[\"probabilities\"]])!=length(rho[[\"quantiles\"]])" )
  # Constants are neither calculated nor fitted, i.e. the procedure is the same for all methods as they are constant:
  if(match(rho["distribution"], "const", nomatch = 0)){
    stop("const not implemented, yet")
  } 
  else if (method=="fit"){
    # The next few lines apply a curve fitting procedure based on given distributions and specified quantiles:
    if(match(rho["distribution"], c("norm", 
                                    "beta",
                                    "cauchy",
                                    "logis",
                                    "t",
                                    "chisq",
                                    "exp",
                                    "f",
                                    "gamma",
                                    "lnorm",
                                    "unif",    
                                    "weibull",
                                    "triang",
                                    "gompertz"), nomatch = 0)){
      x<-rdistq_fit(distribution=rho["distribution"], 
                    n=n, 
                    percentiles=as.numeric(rho[["probabilities"]]), 
                    quantiles=as.numeric(rho[["quantiles"]]), 
                    relativeTolerance=relativeTolerance,
                    ...) 
    }
    else
      stop("\"", rho[["distribution"]], "\" is not a valid distribution type for method=\"", method, "\".")
  }
  else
    stop ("method must be \"fit\".")
  # Return generated random numbers:
  x
}


