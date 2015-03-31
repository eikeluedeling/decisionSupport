#
# file: paramtnormci_numeric.R
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
# paramtnormci_numeric(p, ci, lowerTrunc, upperTrunc, relativeTolerance)
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
#' @return A list with elements \code{mean} and \code{sd}. 
#' @details
#'  For details of the truncated normal distribution see \code{\link[msm]{tnorm}}.
#'  
#'  p-\% confidence interval: \eqn{[c_l, c_u]}
#'  \deqn{tN(\mu,\sigma)}
#'  \deqn{\int_{-\infty}^{c_l} d tN(\mu,\sigma) = p_l}{\ifelse{html}{&int;}{int}}
#'  \deqn{\int_{-\infty}^{c_u} d tN(\mu,\sigma) = p_u}
#'  \eqn{p_l - p_u = p} and \eqn{p_l = 1- p_u}
#'  \if{latex}{\eqn{\int_{-\infty}^{c_l} d tN(\mu,\sigma) = p_l}}\if{html}{\out{<MATH>&int;{{su|b=-&infin;|p=c{{su|b=l}};}};</MATH>}}
#'  
#'  \if{html}{\out{&int;</sup><sub>-&infin;</sub><sup>c<sub>l</sub>}}\cr
#'  
#'  \if{html}{\out{<math>C_6^4</math>}}\cr
#'  
#'  \if{html}{\out{C_6^4}}
#'  
#' @section Warning:
#'   This method has not been tested systematically!
#' @seealso \code{\link[msm]{tnorm}}
#' @export
paramtnormci_numeric <- function(p, ci, lowerTrunc=-Inf, upperTrunc=Inf, relativeTolerance=0.05){
  root<-"quantile"
  # root<-"probability"
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
  

    # Initialize the initialization of the root finding (ToDo: review):
    mean_i <- mean(ci)
    sd_i <- (mean_i - ci[["lower"]])/c_0.95
    ci_i <- c("lower"=NULL, "upper"=NULL)
    if(0){  
      # Generate the the initial values for mean  and sd:
      ci_i[["lower"]] <- msm::qtnorm(p=p[["lower"]], mean=mean_i, sd=sd_i, lower=lowerTrunc, upper=upperTrunc)
      ci_i[["upper"]] <- msm::qtnorm(p=p[["upper"]], mean=mean_i, sd=sd_i, lower=lowerTrunc, upper=upperTrunc)
      
      sd_i <- (ci[["upper"]] - ci[["lower"]])/(ci_i[["upper"]] - ci_i[["lower"]])*sd_i
      mean_i <-  ci_i[["lower"]]/ci[["lower"]] *  max(ci - ci_i) + mean_i
    }

      if ( root=="quantile"){  
        f_calc <-function(x){
          y<-numeric(2)
          y[1] <- msm::qtnorm(p=p[["lower"]], mean=x[1], sd=x[2], lower=lowerTrunc, upper=upperTrunc) - ci[["lower"]]
          y[2] <- msm::qtnorm(p=p[["upper"]], mean=x[1], sd=x[2], lower=lowerTrunc, upper=upperTrunc) - ci[["upper"]]
          y
        }
        f_sim<-function(x){
          y<-numeric(2)
          n<-100*as.integer(1/(relativeTolerance*relativeTolerance))
          r<- msm::rtnorm(n=n, mean=x[1], sd=x[2], lower=lowerTrunc, upper=upperTrunc) 
          y[1] <- quantile(x=r,probs=p[["lower"]]) - ci[["lower"]]
          y[2] <- quantile(x=r,probs=p[["upper"]]) - ci[["upper"]]
          y
        }
        # Auxiliary function defining mean and sd by lower and upper by f(x) = 0 
        # (x[1]:=mean, x[2]:=sd): 
        f <- function(x){
          y<-numeric(2)
          tryCatch(y <- f_calc(x=x),
                   error=function(e){ 
                     y <- f_sim(x=x)
                   }
          )
          y
        } 
      } else if( root=="probability"){
        # Function defined by the difference between confidence probabilities p and the calculated probability 
        # for certain values of the parameters mean and sd:
        f_calc <-function(x){
          y<-numeric(2)
          y[1] <- msm::ptnorm(q=ci[["lower"]], mean=x[1], sd=x[2], lower=lowerTrunc, upper=upperTrunc) - p[["lower"]]
          y[2] <- msm::ptnorm(q=ci[["upper"]], mean=x[1], sd=x[2], lower=lowerTrunc, upper=upperTrunc) - p[["upper"]]
          if (any(is.na(y))) stop ("NAs produced")
          y
        }
        # Fall back function by random sampling simulation, i.e. function defined by the difference between 
        # confidence probabilities p and the simulated probability for certain values of the parameters mean and sd:
        f_sim<-function(x){
          y<-numeric(2)
          n<-100*as.integer(1/(relativeTolerance*relativeTolerance))
          r<- msm::rtnorm(n=n, mean=x[1], sd=x[2], lower=lowerTrunc, upper=upperTrunc) 
          y[1] <- length(r[ r<= ci[["lower"]] ])/n - p[["lower"]]
          y[2] <- length(r[ r<= ci[["upper"]] ])/n - p[["upper"]]
          y
        }
        # Auxiliary function defining mean and sd by lower and upper by f(x) = 0 
        # (x[1]:=mean, x[2]:=sd): 
        f <- function(x){
          y<-numeric(2)
          tryCatch(y <- f_calc(x=x),
                   error=function(e){
                     y <- f_sim(x=x)
                   }
          )
          y
        } 
      } else
        stop("No root finding method chosen.")
      cat("mean_i: ", mean_i, "\n")
      cat("sd_i: ", sd_i, "\n")
      # The root of f are mean and sd:
      #	x_0<-nleqslv::nleqslv(x=c(mean_i, sd_i), fn=f, control=list(maxit=10000))
      x_0<-nleqslv::nleqslv(x=c(mean_i, sd_i), fn=f)	
      mean<-x_0$x[1]
      sd<-x_0$x[2]

    
  # Check postcondition:
  tryCatch({
    ci_i[["lower"]] <- msm::qtnorm(p=p[["lower"]], mean=mean, sd=sd, lower=lowerTrunc, upper=upperTrunc)
    ci_i[["upper"]] <- msm::qtnorm(p=p[["upper"]], mean=mean, sd=sd, lower=lowerTrunc, upper=upperTrunc)
  }, error=function(e){
    n<-100*as.integer(1/(relativeTolerance*relativeTolerance))
    r<- msm::rtnorm(n=n, mean=mean, sd=sd, lower=lowerTrunc, upper=upperTrunc) 
    ci_i[["lower"]] <- quantile(x=r,probs=p[["lower"]])
    ci_i[["upper"]] <- quantile(x=r,probs=p[["upper"]]) - ci[["upper"]]
  }
  )
  print("ci_i: \n")
  print(ci_i)
  if( !isTRUE(msg<-all.equal(ci_i, ci, tolerance=relativeTolerance, scale=min(abs(ci)))) )
    warning(msg)
  #Return
  list(mean=mean, sd=sd)
}

