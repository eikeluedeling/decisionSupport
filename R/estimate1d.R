#
# file: estimate1d.R
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
#' @include rdistq_fit.R
#' @include rtnorm90ci.R 
NULL
##############################################################################################
# estimate1d(...)
# ToDo: review documentation (if pre and postconditions are correct)
##############################################################################################
#' Create an 1-dimensional estimate object
#'
#' This function creates an object of class \code{estimate1d}. 
#' @param distribution \code{character}; A character string that defines the type of the univariate
#'   parametric distribution. 
#' @param lower \code{numeric}; lower bound of the 90\% confidence intervall, i.e the 5\%-quantile 
#'   of this estimate.
#' @param upper \code{numeric}; upper bound of the 90\% confidence intervall, i.e the 95\%-quantile 
#'   of this estimate.
#' @param ... arguments that can be coerced to a data frame (ToDo: or list?) comprising the base of the estimate.
#' @details The parameters in \code{...} provide the base information of an estimate.
#' \subsection{The structure of the estimate}{ \cr
#'    Mandatory elements:
#'    \tabular{lll}{
#'      \bold{Element}      \tab  \bold{R-type}    \tab \bold{Explanation}\cr
#'      \code{distribution} \tab  \code{character} \tab  Distribution type of the estimate \cr
#'      \code{lower}        \tab  \code{numeric}   \tab   5\%-quantile of the estimate\cr
#'      \code{upper}        \tab  \code{numeric}   \tab  95\%-quantile of the estimate
#'    }
#' Optional elements:
#'     \tabular{lll}{
#'      \bold{Element}      \tab  \bold{R-type}    \tab \bold{Explanation}\cr
#'      \code{variable}     \tab  \code{character} \tab  Variable name\cr
#'      \code{median}       \tab  \code{numeric}   \tab  50\%-quantile of the estimate\cr
#'      \code{method}       \tab  \code{character} \tab  Method for calculation of distribution parameters
#'    }
#'  }
#' @return An object of type \code{estimate1d} which is a list with at least (!) the elements:
#'    \tabular{lll}{
#'      \bold{Element}      \tab  \bold{R-type}    \tab \bold{Explanation}\cr
#'      \code{distribution} \tab  \code{character} \tab  Distribution type of the estimate \cr
#'      \code{lower}        \tab  \code{numeric}   \tab   5\%-quantile of the estimate\cr
#'      \code{median}       \tab  \code{numeric}   \tab  50\%-quantile of the estimate\cr
#'      \code{upper}        \tab  \code{numeric}   \tab  95\%-quantile of the estimate
#'    }
#'    \subsection{The \code{median}}{
#'      Note that the \emph{\code{median}} is a mandatory element of an \code{estimate1d}, although it
#'      is not necessary as input. In case that no element \code{median} is provided, the default is
#'      \code{median=mean(c(lower, upper))}. If no median shall be available it has to be set actively
#'      to \code{NULL}!
#'    }
#' @seealso  \code{\link{random.estimate1d}}
#' @export
estimate1d<-function(distribution, lower, upper, ...){
  estimate1dObject<-list(distribution=distribution, lower=lower, upper=upper)
  # ToDo check 1-d property
  # ToDo: process "..."
  if(0){
    estimate1dObject<-data.frame(..., stringsAsFactors=FALSE)
    if( is.null(estimate1dObject$variable) ){
      rownames(estimate1dObject)<-estimate1dObject$variable
      estimate1dObject<-estimate1dObject[!colnames(estimate1dObject) %in% "variable"]
    }
    # Drop rows without variable name:
    estimate1dObject<-subset(estimate1dObject, row.names(estimate1dObject) != "")
    if( is.null(estimate1dObject$distribution) )
      stop("estimate1dObject must be supplied with a distribution column.")
  }
  # Return object:
  class(estimate1dObject)<-"estimate1d"
  estimate1dObject
}
##############################################################################################
# as.estimate1d(...)
# ToDo: review documentation (if pre and postconditions are correct)
##############################################################################################
#' Transform to an 1-dimensional estimate object
#'
#' This function tries to transform an object to class \code{estimate1d}.
#' @param x an object to be transformed to class \code{estimate1d}. 
#' @rdname estimate1d
#' @export
as.estimate1d<-function(x, ...){
  # ToDo check 1-d property
  # ToDo: check correct element types: distribution is character etc.
  # ToDo: process "..."
  x_vec<-unlist(x)
  if ( !"distribution" %in% names(x_vec) )
    stop( "no distribution element!")
  if ( !"lower" %in% names(x_vec) )
    stop( "no lower element!")
  if ( !"upper" %in% names(x_vec) )
    stop( "no upper element!")
  estimate1dObject<-list(distribution=x_vec[["distribution"]], 
                         lower=x_vec[["lower"]],
                         upper=x_vec[["upper"]])
  # Return object:
  class(estimate1dObject)<-"estimate1d"
  estimate1dObject
}
##############################################################################################
# random.estimate1d(rho,n,method, ...)
##############################################################################################
#' Generate univariate random numbers based on a 1-d estimate.
#' 
#' This function generates random numbers for general univariate parametric distributions, which 
#' parameters are determined by a one dimensional \code{\link{estimate1d}}.
#' @param rho \code{estimate1d} object; Univariate distribution to be randomly sampled. 
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
#'  \bold{\code{distribution}}  \tab\bold{Distribution Name}                         \tab \bold{\code{method}} \cr
#'  \code{"const"}                \tab Deterministic case                            \tab not applicable\cr
#'  \code{"norm"}                 \tab \link{Normal}                                 \tab \code{\link[=rdist90ci_exact]{calculate}}, \code{\link[=rdistq_fit]{fit}}  \cr
#'  \code{"posnorm"}              \tab \link[=rposnorm90ci]{Positive normal}         \tab \code{\link[=paramtnormci_numeric]{calculate}}, \code{\link[=paramtnormci_fit]{fit}} \cr
#'  \code{"tnorm_0_1"}            \tab \link[=rtnorm_0_1_90ci]{0-1-truncated normal} \tab \code{\link[=paramtnormci_numeric]{calculate}}, \code{\link[=paramtnormci_fit]{fit}} \cr
#'  \code{"beta"}                 \tab \link{Beta}                                   \tab \code{\link[=rdistq_fit]{fit}}  \cr
#'  \code{"cauchy"}               \tab \link{Cauchy}                                 \tab \code{\link[=rdistq_fit]{fit}}  \cr
#'  \code{"logis"}                \tab \link{Logistic}                               \tab \code{\link[=rdistq_fit]{fit}}  \cr
#'  \code{"t"}                    \tab \link[=TDist]{Student t}                      \tab \code{\link[=rdistq_fit]{fit}}  \cr
#'  \code{"chisq"}                \tab \link[=Chisquare]{Central Chi-Squared}        \tab \code{\link[=rdistq_fit]{fit}}  \cr
#'  \code{"chisqnc"}              \tab \link[=Chisquare]{Non-central Chi-Squared}    \tab \code{\link[=rdistq_fit]{fit}}  \cr
#'  \code{"exp"}                  \tab \link{Exponential}                            \tab \code{\link[=rdistq_fit]{fit}}  \cr  
#'  \code{"f"}                    \tab \link[=FDist]{Central F}                      \tab \code{\link[=rdistq_fit]{fit}}  \cr
#'  \code{"gamma"}                \tab \link[=GammaDist]{Gamma} with \code{scale=1/rate} \tab \code{\link[=rdistq_fit]{fit}}  \cr
#'  \code{"lnorm"}                \tab \link[=Lognormal]{Log Normal}                 \tab \code{\link[=rdist90ci_exact]{calculate}}, \code{\link[=rdistq_fit]{fit}}  \cr
#'  \code{"unif"}                 \tab \link{Uniform}                                \tab \code{\link[=rdist90ci_exact]{calculate}}, \code{\link[=rdistq_fit]{fit}}  \cr
#'  \code{"weibull"}              \tab \link{Weibull}                                \tab \code{\link[=rdistq_fit]{fit}}  \cr
#'  \code{"triang"}               \tab \link[mc2d:triangular]{Triangular}            \tab \code{\link[=rdistq_fit]{fit}}  \cr
#'  \code{"gompertz"}             \tab \link[eha:Gompertz]{Gompertz}                 \tab \code{\link[=rdistq_fit]{fit}}  \cr
#'  \code{"pert"}                 \tab  \link[mc2d:pert]{(Modified) PERT}            \tab \code{\link[=rdistq_fit]{fit}}  \cr
#'  \code{"tnorm"}                \tab  \link[msm:tnorm]{Truncated Normal}           \tab \code{\link[=rdistq_fit]{fit}} 
#'  }
#'  
#'  For \code{distribution="const"} the argument \code{method} is obsolete, as a constant is neither
#'  fitted nor calculated.
#'  \subsection{Only applicable to \code{method="fit"}}{
#'  Given that \code{rho["median"]==NULL} the distribution is fitted only to \code{lower} and \code{upper};
#'   if \code{is.numeric(rho["median"])} the distribution is fitted to \code{lower}, \code{upper} 
#'   and \code{median}. 
#'   }
#'  
#' @seealso \code{\link{estimate1d}}; For \code{method="calculate"}: \code{\link{rdist90ci_exact}}; for \code{method="fit"}: \code{\link{rdistq_fit}}; for both
#'   methods: \code{\link{rposnorm90ci}} and \code{\link{rtnorm_0_1_90ci}}. For the default method: \code{\link{random}}.
#' @export
random.estimate1d<-function(rho,n,method="calculate", relativeTolerance=0.05, ...){
  # Create output vector for the random numbers to be generated
  x<-vector(length=n)
  # Generate the random numbers according to the distribution type:
  ## Constants are neither calculated nor fitted, i.e. the procedure is the same for all methods as they are constant:
  if(match(rho["distribution"], "const", nomatch = 0)){
    x <-  rdist90ci_exact(distribution="const",
                          n=n,
                          lower=rho["lower"],
                          upper=rho["upper"])
  } 
  else if(method=="calculate"){
    # ToDo: extract this block as function rdist90ci_calculate()?
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
    else if(match(rho["distribution"], c("tnorm_0_1"), nomatch = 0)){
      x <-  rtnorm_0_1_90ci(n=n,
                            lower=rho["lower"],
                            upper=rho["upper"],
                            method="numeric",
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
    else if(match(rho["distribution"], c("tnorm_0_1"), nomatch = 0)){
      if( !match("median", names(rho), nomatch = 0) || is.null(rho["median"]) || is.na(as.numeric(rho["median"])))
        median<-NULL 
      else 
        median<-rho["median"]
      x <-  rtnorm_0_1_90ci(n=n,
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
