#
# file: scratchparamtnormci.R
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
# scratchparamtnormci(p, ci, lowerTrunc, upperTrunc, relativeTolerance, method)
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
scratchparamtnormci <- function(p, ci, lowerTrunc=-Inf, upperTrunc=Inf, relativeTolerance=0.05, method="numeric"){
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
  
  if (method=="numeric"){
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
    if(0){
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
    }
    
  } else 
    stop("Not implemented: method=", method)
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

##############################################################################################
# minimization(p, ci, lowerTrunc, upperTrunc, relativeTolerance, method)
##############################################################################################
# median: if NULL: truncated normal is fitted only on lower and upper (ToDo: remove corresponding constraints in this case); 
#     otherwise truncated is fitted on lower, median and upper quantiles simultaneously.
minimization <- function(p, ci, median=mean(ci), lowerTrunc=-Inf, upperTrunc=Inf, relativeTolerance=0.05, method="numeric"){
  #  root<-"quantile"
  root<-"probability"
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
  if( sum(p) != 1)
    stop( "sum(p) != 1 " )
  # Quantile definition:
  if(!is.null(median)){
    if ( !(( ci[["lower"]] < median &&  median < ci[["upper"]] ))  )
      stop("ci does not contain the median!")
    q<-c(ci[["lower"]], median, ci[["upper"]])
    p<-c(p[["lower"]], 0.5, p[["upper"]])
  }
  # Initialize the initialization of the root finding (ToDo: review):
  mean_i <- mean(ci)
  sd_i <- (mean(ci) - ci[["lower"]])/c_0.95
  cat("mean_i: ", mean_i, "\n")
  cat("sd_i: ", sd_i, "\n")
  #ci_i <- c("lower"=NULL, "upper"=NULL)
  if ( root=="quantile"){  
    f_calc <-function(x){
      y<-numeric(length(q))
      y <- msm::qtnorm(p=p, mean=x[1], sd=x[2], lower=lowerTrunc, upper=upperTrunc) - q
      #y[1] <- msm::qtnorm(p=p[["lower"]], mean=x[1], sd=x[2], lower=lowerTrunc, upper=upperTrunc) - ci[["lower"]]
      #y[2] <- msm::qtnorm(p=p[["upper"]], mean=x[1], sd=x[2], lower=lowerTrunc, upper=upperTrunc) - ci[["upper"]]
      if (any(is.na(y))) stop ("NAs produced")
      y
    }
    f_sim<-function(x){
      y<-numeric(length(q))
      n<-100*as.integer(1/(relativeTolerance*relativeTolerance))
      r<- msm::rtnorm(n=n, mean=x[1], sd=x[2], lower=lowerTrunc, upper=upperTrunc) 
      y <- quantile(x=r,probs=p) - q
      #y[1] <- quantile(x=r,probs=p[["lower"]]) - ci[["lower"]]
      #y[2] <- quantile(x=r,probs=p[["upper"]]) - ci[["upper"]]
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
      y<-numeric(length(q))
      y <- msm::ptnorm(q=q, mean=x[1], sd=x[2], lower=lowerTrunc, upper=upperTrunc) - p
      #y[1] <- msm::ptnorm(q=ci[["lower"]], mean=x[1], sd=x[2], lower=lowerTrunc, upper=upperTrunc) - p[["lower"]]
      #y[2] <- msm::ptnorm(q=ci[["upper"]], mean=x[1], sd=x[2], lower=lowerTrunc, upper=upperTrunc) - p[["upper"]]
      if (any(is.na(y))) stop ("NAs produced")
      y
    }
    # Fall back function by random sampling simulation, i.e. function defined by the difference between 
    # confidence probabilities p and the simulated probability for certain values of the parameters mean and sd:
    f_sim<-function(x){
      y<-numeric(length(q))
      n<-100*as.integer(1/(relativeTolerance*relativeTolerance))
      r<- msm::rtnorm(n=n, mean=x[1], sd=x[2], lower=lowerTrunc, upper=upperTrunc) 
      y <- length(r[ r<= q ])/n - p
      #y[1] <- length(r[ r<= ci[["lower"]] ])/n - p[["lower"]]
      #y[2] <- length(r[ r<= ci[["upper"]] ])/n - p[["upper"]]
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
  #  g <- function(x) -max(abs(f(x)))
  g <- function(x) -sum(f(x)*f(x))
  # g <- function(x) -sum(abs(f(x)))
  #  g <- function(x) -(1/f(x))%*%(1/f(x))
  # The optimization constraints:
  # (1) lowerTrunc < mean < upperTrunc, ToDo: necessary???
  # (2) sd > 0
  # are implemented as follows:
  # Constraints A x + B > 0
  # A_1 = ( 1  0 )
  # A_2 = (-1  0 ) 
  # A_3 = ( 0  1 )
  # x_1 = mean
  # x_2 = sd
  # B_1 = - lowerTrunc
  # B_2 = upperTrunc
  # B_3 = 0
  if (upperTrunc < Inf){
    A <- rbind(c( 1, 0), # , ToDo: necessary???
               c(-1, 0), # , ToDo: necessary???
               c( 0, 1))
    B <- rbind(c(-lowerTrunc), # , ToDo: necessary???
               c( upperTrunc), # , ToDo: necessary???
               c(          0))
  } else {
    A <- rbind(c( 1, 0), # , ToDo: necessary???
               c( 0, 1))
    B <- rbind(c(-lowerTrunc), # , ToDo: necessary???
               c(          0))
  }
  constraints<-list(ineqA=A, ineqB=B)
  print("A%*%c(mean_i, sd_i) + B:")
  print(A%*%c(mean_i, sd_i) + B)
  print(A%*%c(mean_i, sd_i) + B > 0)
  ##################################
  # using stats:
  if (0){
    optimizationResult<-stats::constrOptim(theta=c(mean_i, sd_i), 
                                           f=g,
                                           grad=NULL,
                                           ui=A,
                                           ci=-B,
                                           method="SANN",
                                           control = list(
                                             fnscale=-1,
                                             parscale=c(ci[["lower"]],1),
                                             reltol=1e-15
                                           )
    )
    mean<-optimizationResult$par[1]
    sd<-optimizationResult$par[2]
  }
  ##############
  # usning maxLik:
  # ToDo: options
  #  -constraints: lowerTrunc <= mean <= upperTrunc or c[["lower"]] <= mean <= c[["upper"]]
  #  -gradtol=1e-06*1e-02:
  # optimizationResult<-maxLik::maxSANN(fn=g, start=c(mean_i, sd_i), iterlim=1e+05, 
  #                                     constraints=constraints, 
  #                                     parscale=c(ci[["lower"]],sd_i*ci[["lower"]]/(ci[["upper"]]-ci[["lower"]]))) #temp=1e+08, tmax=1000)
  # )
  #optimizationResult<-maxLik::maxSANN(fn=g, start=c(mean_i, sd_i), iterlim=1e+05, constraints=constraints, tmax=1)
  
  # optimizationResult<-maxLik::maxBFGS(fn=g, start=c(mean_i, sd_i), constraints=constraints, reltol=1e-20)
  #  optimizationResult<-maxLik::maxCG(fn=g, start=c(mean_i, sd_i), tol=1e-20)
   optimizationResult<-maxLik::maxNM(fn=g, start=c(mean_i, sd_i), constraints=constraints)
  #  optimizationResult<-maxLik::maxNM(fn=g, start=c(mean_i, sd_i), constraints=constraints, parscale=c(1,10000))
  #  optimizationResult<-maxLik::maxNR(fn=g, start=c(mean_i, sd_i),  gradtol=1e-12, tol=1e-20)
  #  optimizationResult<-maxLik::maxBFGSR(fn=g, start=c(mean_i, sd_i))
  # does not work adhoc:
  #optimizationResult<-maxLik::maxBHHH(fn=g, start=c(mean_i, sd_i))
  
  mean<-optimizationResult$estimate[1]
  sd<-optimizationResult$estimate[2]
  
  ####################
  cat("mean: ", mean, "\n")
  cat("sd: ", sd, "\n")
  print("p: ")
  print(p)
  # Check postcondition:
  q_calc<-numeric(length(q))
  tryCatch({
    q_calc <- msm::qtnorm(p, mean=mean, sd=sd, lower=lowerTrunc, upper=upperTrunc)
    #ci_i[["lower"]] <- msm::qtnorm(p=p[["lower"]], mean=mean, sd=sd, lower=lowerTrunc, upper=upperTrunc)
    #ci_i[["upper"]] <- msm::qtnorm(p=p[["upper"]], mean=mean, sd=sd, lower=lowerTrunc, upper=upperTrunc)
  }, error=function(e){
    n<-100*as.integer(1/(relativeTolerance*relativeTolerance))
    r<- msm::rtnorm(n=n, mean=mean, sd=sd, lower=lowerTrunc, upper=upperTrunc) 
    q_calc <- quantile(x=r,probs=p)
  }
  )
  print("q_calc: \n")
  print(q_calc)
  p_calc<-numeric(length(p))
  p_calc<-msm::ptnorm(q=q, mean=mean, sd=sd, lower=lowerTrunc, upper=upperTrunc)

  print("p_calc:")
  print(p_calc)
  
  for( j in seq(along=p) ){
    if( p[[j]] > 0 )
      scale=p[[j]]
    else scale=NULL
    if( !isTRUE( msg<-all.equal(p[[j]], p_calc[[j]],  scale=scale, tolerance=relativeTolerance) ) ){
      #ToDo: add details of parameters
      warning("Calculated value of ", 100*p[[j]], "%-quantile: ", q_calc[[j]], "\n  ",
              "Target value of ", 100*p[[j]], "%-quantile:     ", q[[j]],   "\n  ",
              "Calculated cumulative probability at value ", q[[j]], " : ", p_calc[[j]], "\n  ",
              "Target  cumulative probability at value ", q[[j]], " : ", p[[j]], "\n  ",
              msg)
    }
  }
  
  optimizationResult
}

if (0){
  ci=c("lower"=1, "upper"=2e+05)
  median=mean(ci)
  q=c(ci[["lower"]], median, ci[["upper"]])
  p=c(0.05, 0.5, 0.95)
  
  lowerTrunc=0
  upperTrunc=Inf
  relativeTolerance=0.05
  
  # Function defined by the difference between confidence probabilities p and the calculated probability 
  # for certain values of the parameters mean and sd:
  f_calc <-function(x_1, x_2){
    y<-numeric(length(p))
    y <- msm::ptnorm(q=q, mean=x_1, sd=x_2, lower=lowerTrunc, upper=upperTrunc) - p
    #y[1] <- msm::ptnorm(q=ci[["lower"]], mean=x_1, sd=x_2, lower=lowerTrunc, upper=upperTrunc) - p[["lower"]]
    #y[2] <- msm::ptnorm(q=ci[["upper"]], mean=x_1, sd=x_2, lower=lowerTrunc, upper=upperTrunc) - p[["upper"]]
    if (any(is.na(y))) stop ("NAs produced")
    y
  }
  # Fall back function by random sampling simulation, i.e. function defined by the difference between 
  # confidence probabilities p and the simulated probability for certain values of the parameters mean and sd:
  f_sim<-function(x_1, x_2){
    # y<-numeric(2)
    y<-numeric(length(p))
    n<-100*as.integer(1/(relativeTolerance*relativeTolerance))
    r<- msm::rtnorm(n=n, mean=x_1, sd=x_2, lower=lowerTrunc, upper=upperTrunc) 
    y <- length(r[ r<= q ])/n - p
    #y[1] <- length(r[ r<= ci[["lower"]] ])/n - p[["lower"]]
    #y[2] <- length(r[ r<= ci[["upper"]] ])/n - p[["upper"]]
    y
  }
  # Auxiliary function defining mean and sd by lower and upper by f(x) = 0 
  # (x[1]:=mean, x[2]:=sd): 
  f <- function(x_1, x_2){
    y<-numeric(2)
    tryCatch(y <- f_calc(x_1=x_1, x_2=x_2),
             error=function(e){
               y <- f_sim(x_1=x_1, x_2=x_2)
             }
    )
    y
  } 
  #g <- function(x_1, x_2) - max(abs(f(x_1=x_1,x_2=x_2)))
  #g <- function(x_1, x_2) - sum(abs(f(x_1=x_1,x_2=x_2)))
  g <- function(x_1, x_2) -sum(f(x_1, x_2)*f(x_1, x_2))
  
  mean<-seq(from=ci[["lower"]],to=ci[["upper"]], length=100)
  sd<-seq(from=ci[["lower"]],to=ci[["upper"]], length=100)
  
  library(lattice)
  #mean%*%sd
  #outer(mean, sd)
  #g_num<-outer(X=mean, Y=sd, FUN=g)
  g_num<-matrix(nrow=length(mean), ncol=length(sd))
  for (i in seq(along=mean))
    for(j in seq(along=sd))
      g_num[i,j]<-g(mean[[i]],sd[[j]])
    print(persp(mean, sd, g_num, theta = 90, phi = 0, expand = 0.5, col = "lightblue", ticktype="detailed", zlim = c(-0.06, 0.0)))
    
    max(g_num)
    arrayInd(which.max(g_num),dim(g_num)) 
}
