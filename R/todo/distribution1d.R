#
# file: R/distribution1d.R
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
# distribution1d(distribution, theta, upper,...)
##############################################################################################
#' Create a 1-dimensional estimate object.
#' 
#' \code{distribution1d} creates an object of class \code{distribution1d}. The estimate of a one dimensional
#' variable is at minimum defined by the type of a univariate parametric distribution, the 5\% - and
#' 95\% quantiles. Optionally, the median can be supplied.
#' @param distribution \code{character}: A character string that defines the type of the univariate
#'   parametric distribution. 
#' @param theta \code{numeric vector}: distribution parameter(s).
#' @param ... arguments that can be coerced to a list comprising further elements of the 1-d 
#'   distribution (for details cf. below). Each element must be atomic and of length 1 (1-d property).
#' @details 
#'   It must hold that \code{lower <= upper}.
#'   \subsection{The structure of the input arguments}{
#'     \subsection{Mandatory input elements:}{
#'     \tabular{lll}{
#'       \bold{Argument}    \tab  \bold{R-type}     \tab \bold{Explanation}\cr
#'       \code{type}         \tab  \code{character} \tab  Distribution type\cr
#'       \code{theta}        \tab  \code{numeric}   \tab  Distribution parameter(s)\cr
#'     }
#'     }
#'     \subsection{Optional input elements:}{
#'     The optional parameters in \code{...} provide additional characteristics of the 1-d estimate. 
#'     Frequent optional elements are:
#'     \tabular{lll}{
#'       \bold{Argument}     \tab  \bold{R-type}                 \tab \bold{Explanation}\cr
#'       \code{variable}     \tab  \code{character}              \tab  Variable name\cr
#'       \code{median}       \tab  cf. below                     \tab  50\%-quantile of the estimate\cr
#'       \code{method}       \tab  \code{character}              \tab  Method for calculation of distribution parameters
#'    }
#'    }
#'  }
#' @return An object of class \code{distribution1d} and \code{list} with at least (!) the elements:
#'    \tabular{lll}{
#'      \bold{Element}      \tab  \bold{R-type}                 \tab \bold{Explanation}\cr
#'      \code{type} \tab  \code{character}              \tab  Distribution type of the estimate \cr
#'      \code{theta}        \tab  \code{numeric}                \tab   Distribtution parameters
#'    }
#' @seealso  \code{\link{random.distribution1d}}
#' @export
distribution1d<-function(type, theta, upper, ...){
  # Check preconditions:
  ## Mandatory arguments:
  ### Check argument types:
  if ( is.null(type) || !is.character(type))
    stop("\"type\" must be supplied as character string.")
  if ( is.null(theta) || is.na(theta<-as.numeric(theta)) )
    stop("\"theta\" must be supplied as numeric.")
  #### Check 1-d property:
  if ( length(type) > 1 || length(theta) > 1 || length(upper) > 1)
    stop("Input dimension is larger than 1. All input elements must be 1 dimensional.")
  ## Optional arguments:
  optionalArguments<-if( missing(...) || length(unlist(list(...)))==0 ) NULL else as.list(unlist(list(...))) 
  #optionalArguments<-list(...)
  median<-NULL
  if( !is.null(optionalArguments) )
    for ( i in names(optionalArguments) ){
      ### Check argument types:
      if (i == "variable" && !is.character(optionalArguments[[i]]))
        stop("Optional argument \"", i, "\" is not character.")
      if (i == "method" && !is.character(optionalArguments[[i]]))
        stop("Optional argument \"", i, "\" is not character.")
      #Process median:
      if (i == "median"){
        median<-optionalArguments[[i]]
        optionalArguments<-optionalArguments[!names(optionalArguments) %in% "median"]
        if ( !is.null(median) ){
          if( is.na(median) )
            median<-NULL
          else if( is.character(median) && median=="mean")
            median<-as.numeric(mean(c(theta, upper)))
          #else if ( !is.numeric(median) ) stop("\"median=", median, "\" is not allowed!")
          else if (  is.na(median<-as.numeric(median)) ) 
            stop("\"median=", median, "\" is not allowed!")
          else if(  (theta > median || median > upper) )
            stop("It must hold: \"theta <= median <= upper\"")
        }
      }
      ### Check 1-d property:
      if ( !is.atomic(optionalArguments[[i]]) )
        stop("Optional argument \"", i, "\" is not atomic.")
      if ( length(optionalArguments[[i]]) > 1 )
        stop("Optional argument \"", i, "\" is not 1 dimensional.")
    }
  # Create the distribution1d:
  if( as.logical(length(optionalArguments)) )
    distribution1dObject<-as.list( c(type=type, theta=theta, median=median, upper=upper, optionalArguments) )
  else
    distribution1dObject<-list(type=type, theta=theta, median=median, upper=upper )    
  # Return object:
  class(distribution1dObject)<-c("distribution1d", class(distribution1dObject))
  distribution1dObject
}
##############################################################################################
# as.distribution1d(x, ...)
##############################################################################################
#' Transform to an 1-dimensional estimate object
#'
#' \code{as.distribution1d} tries to transform an object to class \code{distribution1d}.
#' @param x an object to be transformed to class \code{distribution1d}. 
#' @rdname distribution1d
#' @export
as.distribution1d<-function(x, ...){
  x_vec<-unlist(x)
  if ( !"type" %in% names(x_vec) )
    stop( "no type element!")
  if ( !"theta" %in% names(x_vec) )
    stop( "no theta element!")
  if ( !"upper" %in% names(x_vec) )
    stop( "no upper element!")
  distribution1dObject<-distribution1d(distribution=x_vec[["type"]], 
                               theta=x_vec[["theta"]],
                               as.list(x_vec[!names(x_vec) %in% c("type", "theta", "upper")]))
  # Return object:
  distribution1dObject
}
##############################################################################################
# random.distribution1d(rho,n,method, ...)
##############################################################################################
#' Generate univariate random numbers defined by a 1-d estimate.
#' 
#' This function generates random numbers for univariate parametric distributions, which 
#' parameters are determined by a one dimensional estimate (\code{\link{distribution1d}}).
#' @param rho \code{distribution1d}: Univariate distribution to be randomly sampled. 
#' @param n \code{integer}: Number of observations to be generated
#' @param method \code{character}: Particular method to be used for random number generation. It 
#'    can be either \code{"calculate"} (the default) or \code{"fit"}. Details below.
#' @param relativeTolerance \code{numeric}: the relative tolerance level of deviation of the
#'   generated confidence interval from the specified interval. If this deviation is greater than
#'   \code{relativeTolerance} a warning is given.
#' @param ... Optional arguments to be passed to the particular random number
#'  generating function (cf. below).
#'  @details
#'  \describe{
#'    \item{\code{rho[["type"]]}:}{
#'    The follwing table shows the available distributions and the implemented generation method:
#'    \tabular{lll}{
#'    \bold{\code{rho[["type"]]}}  \tab\bold{Distribution Name}                         \tab \bold{\code{method}} \cr
#'    \code{"const"}                \tab Deterministic case                            \tab not applicable\cr
#'    \code{"norm"}                 \tab \link{Normal}                                 \tab \code{\link[=rdist90ci_exact]{calculate}}, \code{\link[=rdistq_fit]{fit}}  \cr
#'    \code{"posnorm"}              \tab \link[=rposnorm90ci]{Positive normal}         \tab \code{\link[=paramtnormci_numeric]{calculate}}, \code{\link[=paramtnormci_fit]{fit}} \cr
#'    \code{"tnorm_0_1"}            \tab \link[=rtnorm_0_1_90ci]{0-1-truncated normal} \tab \code{\link[=paramtnormci_numeric]{calculate}}, \code{\link[=paramtnormci_fit]{fit}} \cr
#'    \code{"beta"}                 \tab \link{Beta}                                   \tab \code{\link[=rdistq_fit]{fit}}  \cr
#'    \code{"cauchy"}               \tab \link{Cauchy}                                 \tab \code{\link[=rdistq_fit]{fit}}  \cr
#'    \code{"logis"}                \tab \link{Logistic}                               \tab \code{\link[=rdistq_fit]{fit}}  \cr
#'    \code{"t"}                    \tab \link[=TDist]{Student t}                      \tab \code{\link[=rdistq_fit]{fit}}  \cr
#'    \code{"chisq"}                \tab \link[=Chisquare]{Central Chi-Squared}        \tab \code{\link[=rdistq_fit]{fit}}  \cr
#'    \code{"chisqnc"}              \tab \link[=Chisquare]{Non-central Chi-Squared}    \tab \code{\link[=rdistq_fit]{fit}}  \cr
#'    \code{"exp"}                  \tab \link{Exponential}                            \tab \code{\link[=rdistq_fit]{fit}}  \cr  
#'    \code{"f"}                    \tab \link[=FDist]{Central F}                      \tab \code{\link[=rdistq_fit]{fit}}  \cr
#'    \code{"gamma"}                \tab \link[=GammaDist]{Gamma} with \code{scale=1/rate} \tab \code{\link[=rdistq_fit]{fit}}  \cr
#'    \code{"lnorm"}                \tab \link[=Lognormal]{Log Normal}                 \tab \code{\link[=rdist90ci_exact]{calculate}}, \code{\link[=rdistq_fit]{fit}}  \cr
#'    \code{"unif"}                 \tab \link{Uniform}                                \tab \code{\link[=rdist90ci_exact]{calculate}}, \code{\link[=rdistq_fit]{fit}}  \cr
#'    \code{"weibull"}              \tab \link{Weibull}                                \tab \code{\link[=rdistq_fit]{fit}}  \cr
#'    \code{"triang"}               \tab \link[mc2d:triangular]{Triangular}            \tab \code{\link[=rdistq_fit]{fit}}  \cr
#'    \code{"gompertz"}             \tab \link[eha:Gompertz]{Gompertz}                 \tab \code{\link[=rdistq_fit]{fit}}  \cr
#'    \code{"pert"}                 \tab  \link[mc2d:pert]{(Modified) PERT}            \tab \code{\link[=rdistq_fit]{fit}}  \cr
#    \code{"tnorm"}                \tab  \link[msm:tnorm]{Truncated Normal}           \tab \code{\link[=rdistq_fit]{fit}} 
#'    }
#' @seealso \code{\link{distribution1d}}; For \code{method="calculate"}: \code{\link{rdist90ci_exact}}; for \code{method="fit"}: \code{\link{rdistq_fit}}; for both
#'   methods: \code{\link{rposnorm90ci}} and \code{\link{rtnorm_0_1_90ci}}. For the default method: \code{\link{random}}.
#' @examples
#' # Generate log normal distributed random numbers:
#' x<-random(distribution1d("lnorm",50,100), n=100000)
#' quantile(x, probs=c(0.05, 0.95))
#' hist(x, breaks=100)
#' @export
random.distribution1d<-function(rho ,n , method="calculate", relativeTolerance=0.05, ...){
  if(rho[["distribution"]]=="binom")
    x<-rbinom(n=n, size=theta[["size"]], prob=theta[["prob"]])

  # Return generated random numbers:
  x
}

