#
# file: random_estimate_1d.R
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
#' @include rdist90ci_exact.R 
#' @include rposnorm90ci.R 
#' @include r0_1norm90ci_numeric.R 
#' @include rdistq_fit.R
NULL
##############################################################################################
# random_estimate_1d(rho,n,method, ...)
##############################################################################################
#' Generate univariate random numbers based on a 1-d estimate.
#' 
#' This function generates random numbers for general univariate parametric distributions, which 
#' parameters are determined by a one dimensional \code{\link{estimate}}.
#' @param rho \code{estimate} object; Univariate distribution to be randomly sampled. 
#' @param n Number of generated observations
#' @param method Particular method to be used for random number generation.
#' @param relativeTolerance \code{numeric}; the relative tolerance level of deviation of the
#'   generated confidence interval from the specified interval. If this deviation is greater than
#'   \code{relativeTolerance} a warning is given.
#' @param ... Optional arguments to be passed to the particular random number
#'  generating function.
#'  @details
#'  \code{method} can be either \code{"calculate"} (the default) or \code{"fit"}.
#'  
#' The follwing table shows the available distributions and the implemented generation method:
#'  \tabular{lll}{
#'  \bold{Identification} \tab  \bold{Distribution} \tab \bold{\code{method}} \cr
#'  \code{const}  \tab ToDo \tab not applicable because there is nothing to be calculated or fitted\cr
#'  \code{\link[=Normal]{norm}}       \tab Normal distribution  \tab \code{\link[=rdist90ci_exact]{calculate}}, \code{\link[=rdistq_fit]{fit}}  \cr
#'  \code{\link[=rposnorm90ci]{posnorm}}\tab ToDo \tab \code{\link[=paramtnormci_numeric]{calculate}}, \code{\link[=paramtnormci_fit]{fit}} \cr
#'  \code{\link[=r0_1norm90ci_numeric]{0_1norm}} \tab ToDo \tab \code{\link[=r0_1norm90ci_numeric]{calculate}} \cr
#'  \code{\link[=Beta]{beta}}         \tab Beta distribution    \tab \code{\link[=rdistq_fit]{fit}}  \cr
#'  \code{cauchy}     \tab ToDo \tab \code{\link[=rdistq_fit]{fit}}  \cr
#'  \code{logis}      \tab ToDo \tab \code{\link[=rdistq_fit]{fit}}  \cr
#'  \code{t}          \tab ToDo \tab \code{\link[=rdistq_fit]{fit}}  \cr
#'  \code{chisq}      \tab ToDo \tab \code{\link[=rdistq_fit]{fit}}  \cr
#'  \code{chisqnc}    \tab ToDo: implement? \tab \code{\link[=rdistq_fit]{fit}}  \cr
#'  \code{exp}        \tab ToDo \tab \code{\link[=rdistq_fit]{fit}}  \cr  
#'  \code{f}          \tab ToDo \tab \code{\link[=rdistq_fit]{fit}}  \cr
#'  \code{gamma}      \tab ToDo \tab \code{\link[=rdistq_fit]{fit}}  \cr
#'  \code{lnorm}      \tab ToDo \tab \code{\link[=rdistq_fit]{fit}}  \cr
#'  \code{unif}       \tab ToDo \tab \code{\link[=rdist90ci_exact]{calculate}}, \code{\link[=rdistq_fit]{fit}}  \cr
#'  \code{weibull}    \tab ToDo \tab \code{\link[=rdistq_fit]{fit}}  \cr
#'  \code{triang}     \tab ToDo \tab \code{\link[=rdistq_fit]{fit}}  \cr
#'  \code{gompertz}   \tab ToDo \tab \code{\link[=rdistq_fit]{fit}}  \cr
#'  \code{pert}       \tab ToDo \tab \code{\link[=rdistq_fit]{fit}}  
#  \code{\link[msm]{tnorm}}      \tab Truncated normal distribution \tab \code{\link[=rdistq_fit]{fit}} 
#'  }
#'  
#' @seealso For \code{method="calculate"}: \code{\link{rdist90ci_exact}}, and
#'   \code{\link{r0_1norm90ci_numeric}}; for \code{method="fit"}: \code{\link{rdistq_fit}}; for both
#'   methods: \code{\link{rposnorm90ci}}
#' @export
random_estimate_1d<-function(rho,n,method="calculate", relativeTolerance=0.05, ...){
  # Create output vector for the random numbers to be generated
  x<-vector(length=n)
  # Generate the random numbers according to the distribution type:
  if(0){
    #     if ( match(rho["distribution"], c("constant", 
    #                                    "normal", 
    #                                    "pos_normal", 
    #                                    "normal_0_1", 
    #                                    "poisson", 
    #                                    "binomial", 
    #                                    "uniform", 
    #                                    "lognorm", 
    #                                    "lognorm_lim2" ), nomatch = 0 )){
    #       if ( is.numeric(lower) && is.numeric(upper) )
    #         x <-  rdist90ci(distribution=rho["distribution"],
    #                         n=n,
    #                         lower=rho["lower"],
    #                         upper=rho["upper"])
    #       else if ( is.numeric(mean) && is.numeric(sd) )
    #         x <- rdistmsd(distribution=rho["distribution"],
    #                       n=n,
    #                       mean=rho["mean"],
    #                       sd=rho["sd"])
    #       else
    #         stop(rho["distribution"], "-distribution must be suplied with either the 90%-confidence intervall, 
    #            i.e. lower and upper value, or with the mean and standard deviation (sd)!")
    #       
    #     }  else 
  } 
  # Constants are neither calculated nor fitted, i.e. the procedure is the same for all methods as they are constant:
  if(match(rho["distribution"], "const", nomatch = 0)){
    x <-  rdist90ci_exact(distribution="const",
                          n=n,
                          lower=rho["lower"],
                          upper=rho["upper"])
  } 
  else if(method=="calculate"){
    # ToDo: extract this block as function rdistq_calculate()
    if(match(rho["distribution"], c("norm", 
                                    "unif"), nomatch = 0)){
      x <-  rdist90ci_exact(distribution=rho["distribution"],
                            n=n,
                            lower=rho["lower"],
                            upper=rho["upper"])
    }
    else if(match(rho["distribution"], c("posnorm"), nomatch = 0)){
      x <-  rposnorm90ci(n=n,
                         lower=rho["lower"],
                         upper=rho["upper"],
                         method="numeric",
                         relativeTolerance = relativeTolerance)
    } 
    else if(match(rho["distribution"], c("0_1norm"), nomatch = 0)){
      x <-  r0_1norm90ci_numeric(n=n,
                                 lower=rho["lower"],
                                 upper=rho["upper"],
                                 relativeTolerance = relativeTolerance)
    }
    else
      stop("\"", rho["distribution"], "\" is not a valid distribution type for method=\"", method, "\".")
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
      if( rho["distribution"]=="unif" ) { 
        percentiles<-c(0.05,0.95)
        quantiles<-c(rho["lower"], rho["upper"])
      } else if( !match("median", names(rho), nomatch = 0) || is.null(rho["median"]) || is.na(as.numeric(rho["median"]))){
        percentiles<-c(0.05,0.95)
        quantiles<-c(rho["lower"], rho["upper"])
      }  else {
        percentiles<-c(0.05,0.5,0.95)
        quantiles<-c(rho["lower"], rho["median"], rho["upper"])
      }   
      x<-rdistq_fit(distribution=rho["distribution"], 
                    n=n, 
                    percentiles=percentiles, 
                    quantiles=as.numeric(quantiles), 
                    relativeTolerance=relativeTolerance,
                    ...) 
    }  
    else if(match(rho["distribution"], c("posnorm"), nomatch = 0)){
      if( !match("median", names(rho), nomatch = 0) || is.null(rho["median"]) || is.na(as.numeric(rho["median"])))
        median<-NULL 
      else 
        median<-rho["median"]
      x <-  rposnorm90ci(n=n,
                         lower=rho["lower"],
                         median=median,
                         upper=rho["upper"],
                         method="fit",
                         relativeTolerance=relativeTolerance)
    } 
    else
      stop("\"", rho["distribution"], "\" is not a valid distribution type for method=\"", method, "\".")
  }
  else
    stop ("method must be either \"calculate\" or \"fit\".")
  # Return generated random numbers:
  x
}
