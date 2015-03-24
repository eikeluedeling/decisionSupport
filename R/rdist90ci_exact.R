#
# file: rdist90ci_exact.R
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
# rdist90ci_exact(distribution, n, lower, upper)
##############################################################################################
#' Generate univariate random numbers based on the 90\%-confidence interval.
#' 
#' This function generates random numbers for general univariate
#' distributions based on the 90\% confidence interval. 
#' @param distribution \code{character}; A character string that defines the univariate distribution
#'  to be randomly sampled. 
#' @param n Number of generated observations.
#' @param lower \code{numeric}; lower bound of the 90\% confidence intervall.
#' @param upper \code{numeric}; upper bound of the 90\% confidence intervall.
#' @details
#' The follwing table shows the available distributions and their identification as a character string:
#'  \tabular{ll}{
#'  Distribution encoding \tab  Distribution\cr
#'  \code{const}       \tab ToDo \cr
#'  \code{norm}         \tab ToDo \cr
#'  \code{pos_norm}     \tab ToDo \cr 
#'  \code{norm_0_1}     \tab ToDo \cr 
#'  \code{pois}        \tab ToDo \cr 
#'  \code{binom}       \tab ToDo \cr 
#'  \code{unif}        \tab ToDo \cr 
#'  \code{lnorm}        \tab ToDo \cr
#'  \code{lnorm_lim2}   \tab ToDo  
#'  }
#' @export
rdist90ci_exact <- function(distribution, n, lower, upper){
  # Constants:
  # 95%-critical value of standard normal distribution (c_0.95=1.645):
  c_0.95=qnorm(0.95)
  # Check preconditions
  if ( is.null(lower) || is.null(upper) || is.na(lower) || is.na(upper) )
  	stop("lower and upper value of the 90%-confidence intervall must be given.")
  # Prepare input variable: types
  lower<-as.numeric(lower)
  upper<-as.numeric(upper)

  # Create output vector for the random numbers to be generated
  x<-vector(length=n)
  # Generate the random numbers according to the distribution type:
  if(distribution=="const") {
    if( isTRUE(all.equal(lower,upper)) ) 
      x<-rep(lower,n)
    else
      stop("lower: ", lower, " is not equal to upper: ", upper)
  }
  else if(distribution=="norm")     
    x<-rnorm(n=n,
             mean=mean(c(lower,upper)),
             sd=(mean(c(lower,upper))-lower)/c_0.95)
  #   else if(distribution=="pos_norm") {
  #     x<-rnorm(n,mean=mean(c(lower,upper)),sd=(mean(c(lower,upper))-lower)/c_0.95)
  #     # ToDo: check this:
  #     x[which(x<0)]<-0
  #   }
  #   else if(distribution=="norm_0_1") {
  #     x<-rnorm(n,mean=mean(c(lower,upper)),sd=(mean(c(lower,upper))-lower)/c_0.95)
  #     # ToDo: check this:
  #     x[which(x<0)]<-0
  #     x[which(x>1)]<-1
  #   }
  #   else if(distribution=="pois")    
  #     x<-rpois(n, mean(c(lower,upper)))
  #   else if(distribution=="binom")   
  #     x<-rbinom(n,1,lower)  # ToDo: Why size=1? Why prob=lower?
  else if(distribution=="unif"){ 
    x<-runif(n=n, 
             min=lower-(upper-lower)*0.05, 
             max=upper+(upper-lower)*0.05)
    #old:    x<-runif(n,lower,upper)
  }
  #   else if(distribution=="lnorm")    
  #     #ToDo: this produces a wrong result
  #     x<-rlnorm(n,meanlog=mean(log(upper),log(lower)),sdlog=(mean(c(log(upper),log(lower)))-log(lower))/c_0.95) 
  #   else if(distribution=="lnorm_lim2") {
  #     temp<-rlnorm(n,meanlog=mean(log(upper),log(lower)),sdlog=(mean(c(log(upper),log(lower)))-log(lower))/c_0.95)
  #     temp[which(temp>2*upper)]<-2*upper
  #     x<-temp
  #   }
  else
    stop("\"", distribution, "\" is not a valid distribution type.")
  #Return
  x
}
