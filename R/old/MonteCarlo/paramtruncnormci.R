#
# file: paramtruncnormci.R
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
# paramtruncnormci(p, ci, lowerTrunc, upperTrunc, relativeTolerance, method)
##############################################################################################
#' Return parameters of truncated normal distribution based on a confidence interval.
#' 
#' This function calculates the distribution parameters, i.e. \code{mean} and \code{sd}, of a truncated normal distribution 
#' from an arbitrary confidence interval. 
#' @param p \code{numeric} 2-dimensional vector; probabilities of upper and lower bound of the corresponding 
#' confidence interval.
#' @param ci \code{numeric} 2-dimensional vector; lower and upper bound of the  confidence interval.
#' @param lowerTrunc \code{numeric}; lower truncation point of the distribution (>= \code{-Inf}). 
#' @param upperTrunc \code{numeric}; upper truncation point of the distribution (<= \code{Inf}).
#' @param relativeTolerance \code{numeric}; the relative tolerance level of deviation of the generated confidence 
#' interval from the specified interval. If this deviation is greater than \code{relativeTolerance} a warning is given.
#' @param method The method to calculate the parameters. Default is \code{"numeric"}.
#' @return A list with elements \code{mean} and \code{sd}. 
#' @details
#'  For details of the truncated normal distribution see \code{\link[truncnorm]{truncnorm}}.
#'  
#'  
#' @section Warning:
#'   This method has not been tested systematically!
#' @seealso \code{\link[truncnorm]{truncnorm}}
#' @export
paramtruncnormci <- function(p, ci, lowerTrunc=-Inf, upperTrunc=Inf, relativeTolerance=0.05, method="numeric"){
  # Constants:
  # 95%-critical value of standard normal distribution (c_0.95=1.645):
  c_0.95=qnorm(0.95)
  # Check preconditions
  if ( is.null(p) || !all(!is.na(p)))
    stop("p must be supplied.")
  if ( is.null(ci) || !all(!is.na(ci)))
    stop("ci must be supplied.")
  if ( is.null(lowerTrunc) || is.null(upperTrunc) || is.na(lowerTrunc) || is.na(upperTrunc) )
    stop("lower and upper truncation points must be supplied.")
  if (length(p)!=2)
    stop("p must be of length 2.")
  if (length(ci)!=2)
    stop("ci must be of length 2.")
  # Prepare input variable: types
  p<-as.numeric(p)
  ci<-as.numeric(ci)
  lowerTrunc<-as.numeric(lowerTrunc)
  upperTrunc<-as.numeric(upperTrunc)
  if(p[[1]] >= p[[2]])
    stop("p[[1]] >= p[[2]]")
  if(ci[[1]] >= ci[[2]])
    stop("ci[[1]] >= ci[[2]]")
  names(p)<-c("lower", "upper")
  names(ci)<-c("lower", "upper")	
  if ( !((lowerTrunc < ci[["lower"]] &&  ci[["upper"]] < upperTrunc))  )
    stop("ci is not a subset of [lowerTrunc, upperTrunc]!")
  
  if (method=="numeric"){
    # Initialize the initialization of the root finding (ToDo: review):
 #   mean_i <- mean(ci)
    mean_i <- p%*%ci
#    sd_i <- (mean_i - ci[["lower"]])/c_0.95
    sd_i <- mean(c(mean_i - ci[["lower"]], ci[["upper"]] - mean_i))
    ci_i <- c("lower"=NULL, "upper"=NULL)
if(0){
    # Generate the the initial values for mean  and sd:
    ci_i[["lower"]] <- truncnorm::qtruncnorm(p=p[["lower"]], a=lowerTrunc, b=upperTrunc, mean=mean_i, sd=sd_i)
    ci_i[["upper"]] <- truncnorm::qtruncnorm(p=p[["upper"]], a=lowerTrunc, b=upperTrunc, mean=mean_i, sd=sd_i)	
    
    cat("ci_i: ", ci_i, "\n")
    
    sd_i <- (ci[["upper"]] - ci[["lower"]])/(ci_i[["upper"]] - ci_i[["lower"]])*sd_i
    mean_i <-  ci_i[["lower"]]/ci[["lower"]] *  max(ci - ci_i) + mean_i
}    
    cat("mean_i: ", mean_i,"\n")
    cat("sd_i: ", sd_i,"\n")
    
    # Auxiliary function defining mean and sd by lower and upper by f(x) = 0 
    # (x[1]:=mean, x[2]:=sd): 
    f <- function(x){
      y<-numeric(2)
      y[1]<- truncnorm::qtruncnorm(p=p[["lower"]], a=lowerTrunc, b=upperTrunc, mean=x[1], sd=x[2]) - ci[["lower"]]
      y[2]<- truncnorm::qtruncnorm(p=p[["upper"]], a=lowerTrunc, b=upperTrunc, mean=x[1], sd=x[2]) - ci[["upper"]]
      y
    }
    # The root of f are mean and sd:
    #	x_0<-nleqslv::nleqslv(x=c(mean_i, sd_i), fn=f, control=list(maxit=10000, trace=1)))
    x_0<-nleqslv::nleqslv(x=c(mean_i, sd_i), fn=f)	
    mean<-x_0$x[1]
    sd<-x_0$x[2]
  } else 
    stop("Not implemented: method=", method)
  # Check postcondition:
  ci_i[["lower"]] <- truncnorm::qtruncnorm(p=p[["lower"]], a=lowerTrunc, b=upperTrunc, mean=mean, sd=sd)
  ci_i[["upper"]] <- truncnorm::qtruncnorm(p=p[["upper"]], a=lowerTrunc, b=upperTrunc, mean=mean, sd=sd)
#  scale<-min(abs(ci-ci_i))
#  scale<-mean(abs(ci-ci_i))
#  scale<-0
#  cat("scale: ", scale, "\n")
#  cat("if(scale>0) scale else NULL): ", if(scale>0) scale else NULL, "\n")
  cat("ci_i:") 
  print(ci_i)
  for( j in c("lower", "upper") ){
    scale<-abs(ci[[j]])
    #  scale<-0
    cat("scale: ", scale, "\n")
    cat("if(scale>0) scale else NULL): ", if(scale>0) scale else NULL, "\n")
    
 #   if( !isTRUE( msg<-all.equal( ci_i[[j]], ci[[j]], tolerance=relativeTolerance, scale=if(scale>0) scale else NULL ) ) ){
     if( !isTRUE( msg<-all.equal(ci[[j]], ci_i[[j]],  tolerance=relativeTolerance) ) ){
      #ToDo: add details of parameters
      warning("Calculated value of ", j, " - confidence level: ", ci_i[[j]], "\n  ",
              "Target value of ", j, " - confidence level:     ", ci[[j]],   "\n  ",
              "Termination code of nleqslv: ", x_0$termcd,                   "\n  ",
              "Termination message of nleqslv: ", x_0$message,               "\n  ",
              msg)
    }
  }
#  if( !isTRUE( msg<-all.equal( ci_i, ci, tolerance=relativeTolerance, scale=mean(abs(ci)) ) ) )
  if( !isTRUE( msg<-all.equal( ci, ci_i, tolerance=relativeTolerance ) ) )
    warning(msg)
  #Return
  list(mean=mean, sd=sd)
}
