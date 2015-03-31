#
# file: rdistq_fit.R
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
##############################################################################################
# rdistq_fit(distribution, n, percentiles, quantiles)
##############################################################################################
#' Generate univariate random numbers based on quantiles.
#' 
#' This function generates random numbers for a set of univariate distributions based on the 
#' distribution quantiles. Internally, this is achieved by fitting the distribution function
#' to the given quantiles using \code{\link[rriskDistributions]{rriskFitdist.perc}}. 
#' @param distribution A character string that defines the univariate distribution
#'  to be randomly sampled. 
#' @param n Number of generated observations.
#' @param percentiles Numeric vector giving the percentiles. 
#' @param quantiles Numeric vector giving the quantiles. 
#' @details
#' The follwing table shows the available distributions and their identification as a character string:
#'  \tabular{lll}{
#'  \bold{Identification} \tab  \bold{Distribution} \tab \bold{Number of quantiles}\cr
#'  \code{\link[=Normal]{norm}}       \tab Normal distribution  \tab >=2  \cr
#'  \code{\link[=Beta]{beta}}         \tab Beta distribution    \tab ToDo \cr  
#'  \code{cauchy}     \tab ToDo \tab ToDo\cr
#'  \code{logis}      \tab ToDo \tab ToDo\cr 
#'  \code{t}          \tab ToDo \tab ToDo\cr 
#'  \code{chisq}      \tab ToDo \tab ToDo\cr 
#'  \code{chisqnc}    \tab ToDo: implement? \tab ToDo\cr 
#'  \code{exp}        \tab ToDo \tab ToDo\cr    
#'  \code{f}          \tab ToDo \tab ToDo\cr 
#'  \code{gamma}      \tab ToDo \tab ToDo\cr
#'  \code{lnorm}      \tab ToDo  \tab ToDo\cr
#'  \code{unif}       \tab ToDo \tab ToDo\cr
#'  \code{weibull}    \tab ToDo \tab ToDo\cr
#'  \code{\link[mc2d:triangular]{triang}}   \tab Triangular Distribution, Note: package \code{mc2d} needed. \tab ToDo\cr
#'  \code{\link[eha:Gompertz]{gompertz}}   \tab Gompertz distribution, Note: package \code{eha} needed.\tab ToDo\cr
#'  \code{\link[mc2d]{pert}}       \tab The (modified) PERT distribution,  Note: package \code{mc2d} needed. \tab ToDo\cr
#'  \code{\link[msm]{tnorm}}      \tab Truncated normal distribution \tab ToDo
#'  }
#' The default for \code{percentiles} is 0.05, 0.5 and 0.95, so for the default, 
#' the quantiles argument should be a vector with 3 elements. If this is to be longer,
#' the percentiles argument has to be adjusted to match the length of quantiles.
#' @return ToDo
#' @export
rdistq_fit <- function(distribution, n, percentiles=c(0.05,0.5,0.95), quantiles, tolConv=0.001, fit.weights=rep(1,length(percentiles))){
  # Relative tolerance of the maximum relative deviation of generated quantiles from desired value:
  relativeTolerance = 0.05
  #   # Initialize the loop:
  #   maxRelativeDeviation <- relativeTolerance + 1
  # tolConv=0.001
  # Fit the desired distribution until the goodnes of fit is tolerated:
  #  while (maxRelativeDeviation > relativeTolerance){
  # Fit the distribution to the given quantiles:
  # ToDo: include namespace check and move rriskDistributions to Suggests in file DESCRIPTION
  capture.output(dists<-try(rriskDistributions::rriskFitdist.perc(p=percentiles,
                                                                  q=quantiles,
                                                                  show.output=TRUE,
                                                                  tolConv=tolConv,
                                                                  fit.weights=fit.weights)),
                 file='NUL')
  
  if(length(dists)==1 && is.na(dists))
    stop("no distribution could be fitted.")
  possible_dists<-colnames(dists$results[,3:ncol(dists$results)])[which(!is.na(dists$results[1,3:ncol(dists$results)]))]
#ToDo: here implement check, something like this:
  # if(match(distribution, possible_dists, nomatch=0) )
  # because e.g. msm::rtnorm() will be in an endless loop for NAs in dists$results!!!
  if( match(distribution, possible_dists, nomatch=0) ){
  #output<-NA
  # Generate the random numbers according to the distribution type:
  if(distribution=="norm") 
    output<-rnorm(n=n, 
                  mean=dists$results[1,"norm"],
                  sd=dists$results[2,"norm"])
  else if(distribution=="beta") 
    output<-rbeta(n=n,
                  shape1=dists$results[1,"beta"],
                  shape2=dists$results[2,"beta"])
  else if(distribution=="cauchy") 
    output<-rcauchy(n=n,
                    location=dists$results[1,"cauchy"],
                    scale=dists$results[2,"cauchy"])
  else if(distribution=="logis") 
    output<-rlogis(n=n,
                   location=dists$results[1,"logis"],
                   scale=dists$results[2,"logis"])
  else if(distribution=="t") 
    output<-rt(n=n,
               df=dists$results[1,"t"])
  else if(distribution=="chisq"){ 
    output<-rchisq(n=n,
                   df=dists$results[1,"chisq"])
  #if(distribution=="chisqnc") output<-rchisqnc(n,dists$results[1,"chisqnc"],dists$results[2,"chisqnc"])
  #not sure how chisqnc works
  } else if(distribution=="exp") 
    output<-rexp(n=n,
                 rate=dists$results[1,"exp"])
  else if(distribution=="f") 
    output<-rf(n=n,
               df1=dists$results[1,"f"],
               df2=dists$results[2,"f"])
  else if(distribution=="gamma") 
    output<-rgamma(n=n,
                   shape=dists$results[1,"gamma"],
                   rate=dists$results[2,"gamma"])
  else if(distribution=="lnorm") 
    output<-rlnorm(n=n,
                   meanlog=dists$results[1,"lnorm"],
                   sdlog=dists$results[2,"lnorm"])
  else if(distribution=="unif") {
    output<-runif(n=n,
                  min=dists$results[1,"unif"],
                  max=dists$results[2,"unif"])
  #unif needs 2 quantiles (can't handle 3)
  } else if(distribution=="weibull") 
    output<-rweibull(n=n,
                     shape=dists$results[1,"weibull"],
                     scale=dists$results[2,"weibull"])
  else if(distribution=="triang"){ 
    if (!requireNamespace("mc2d", quietly = TRUE)) {
      stop("Package mc2d needed for option distribution=", distribution, ". Please install it.",
           call. = FALSE)
    } else
      output<-mc2d::rtriang(n=n,
                            min=dists$results[1,"triang"],
                            mode=dists$results[2,"triang"],
                            max=dists$results[3,"triang"])
  }
  else if(distribution=="gompertz") { 
    if (!requireNamespace("eha", quietly = TRUE)) {
      stop("Package eha needed for option distribution=", distribution, ". Please install it.",
           call. = FALSE)
    } else
      output<-eha::rgompertz(n=n,
                             shape=dists$results[1,"gompertz"],
                             scale=dists$results[2,"gompertz"])
  }
  else if(distribution=="pert"){
    #pert needs 4 quantiles
    if (!requireNamespace("mc2d", quietly = TRUE)) {
      stop("Package mc2d needed for option distribution=", distribution, ". Please install it.",
           call. = FALSE)
    } else 
    output<-mc2d::rpert(n=n,
                  min=dists$results[1,"pert"],
                  mode=dists$results[2,"pert"],
                  max=dists$results[3,"pert"],
                  shape=dists$results[4,"pert"])
  }
  else if(distribution=="tnorm") {
    output<-msm::rtnorm(n=n,
                        mean=dists$results[1,"tnorm"],
                        sd=dists$results[2,"tnorm"],
                        lower=dists$results[3,"tnorm"],
                        upper=dists$results[4,"tnorm"])
  #tnorm needs 4 quantiles
  } else
    stop("\"", distribution, "\" is not a valid distribution type.")
  } else {
  # if(is.na(output[1])) 
    stop("\"", distribution, "\" distribution could not be fitted. One of the following should work:", 
         paste(possible_dists,collapse=", "))
  }
  # Maximum relative deviation of generated quantiles from desired value
  # ToDo: review, in particular catch division by zero or numeric zero
  maxRelativeDeviation <- max ( abs((quantile(x=output, probs=percentiles) - quantiles ) / quantiles ) )
  #     # Increase the absolute tolerance for next loop
  #     tolConv<-tolConv/2
  #   }
  # Check goodnes of fit:
  if( maxRelativeDeviation > relativeTolerance )
    warning ("The maximum relative deviation of generated quantiles from desired value is: ", maxRelativeDeviation)
  # Return sampeled distribution if it could be achieved, NA otherwise:
  return(output)
}
