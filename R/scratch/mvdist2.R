<<<<<<< HEAD
#
# file: mvdist2.R
#
# R package: decisionSupport
# 
# Authors (ToDo order?): 
#   Lutz Göhring <lutz.goehring@gmx.de>
#   Eike Luedeling (ICRAF) <E.Luedeling@cgiar.org>
#
# Affiliation: ToDo
# 
# License: ToDo
#
##############################################################################################

##############################################################################################
# mvdist2 (distribution_type, mean, sigma, var_names=c(""), var_description=c(""), ...)
##############################################################################################
#' Define a general multivariate probability density based on the first two moments.
#' 
#' This function defines a multivariate probability density that can be characterized 
#' by the joint first two moments, viz. the mean and covariance. 
#' @param distribution_type string vector; characterizing the underlying one dimensional
#' distributions.
#' @param mean numerical vector; containing the mean values of the underlying one dimensional 
#' distributions.
#' @param sigma covariance matrix of the underlying one dimensional distributions.
#' @param var_names string vector; names of variables, optional
#' @param var_description string vector; variable descriptions, optional
#' @param ... further options passed to data.frame
#' @return ToDo
#' @export
mvdist2 <- function(distribution_type, mean, sigma, var_names=c(""), var_description=c(""), ...){
  #ToDo: check preconditions
  #ToDo: check this:
  returnObject<-data.frame(distribution_type=distribution_type, 
                           mean=mean, 
                           sigma=sigma, 
                           var_description=var_description, 
                           row.names=var_names,
                           ...)
  class(returnObject)<-"mvdist2"
  return(returnObject)
}
##############################################################################################
# mvdist2_read_csv(filename,...)
##############################################################################################
#' Read an \code{\link{mvdist2}} object from csv file(s)
#'
#' The csv file must have the columns:
#' \tabular{lll}{
#'  \code{distribution_type}  \tab string \tab  \cr
#'  \code{mean}               \tab numeric \tab  mean of the distribution \cr
#'  \code{sigma}              \tab numeric  \tab variance of the distribution
#'  }
#'  Optional columns are:
#'  \tabular{lll}{
#'  \code{var_names}        \tab string \tab  \cr
#'  \code{var_description}  \tab string \tab
#' }
#' Optionally a second csv file in the same directory, which is named
#' \code{<filename>-cov.csv}, can be provided. This file provides the covariance 
#' matrix of the variables. It can be purely numeric or with optional variable 
#' names in the first row and column respectively. 
mvdist2_read_csv <- function(filename,...){
  object <- read.csv(filename, ...)
  mvdist2(distribution_type=object$distribution_type,
          mean=object$mean,
          sigma=object$sigma,
          var_names=object$var_names,
          var_description=object$var_description)
  
}
##############################################################################################
# random.mvdist2(rho,n,method,...)
##############################################################################################
#' Generate general multivariate random numbers based on the first two moments.
#' 
#' This function generates multivariate random numbers for general multivariate 
#' distributions that can be characterized by the joint first two moments, viz. 
#' the mean and covariance. 
#' @param rho \code{\link{mvdist2}} object; Distribution to be randomly sampled. 
#' @param n Number of generated observations.
#' @param method Particular method to be used for random number generation.
#' @param ... Optional arguments to be passed to the particular random number
#'  generating function.
random.mvdist2 <- function(rho,n,method, ...){
  #ToDo
  #ToDo: this replaces the "heart" of make_variables()
  #ToDo: check preconditions
}

=======
#
# file: mvdist2.R
#
# R package: decisionSupport
# 
# Authors (ToDo order?): 
#   Lutz Göhring <lutz.goehring@gmx.de>
#   Eike Luedeling (ICRAF) <E.Luedeling@cgiar.org>
#
# Affiliation: ToDo
# 
# License: ToDo
#
##############################################################################################

##############################################################################################
# mvdist2 (distribution_type, mean, sigma, var_names=c(""), var_description=c(""), ...)
##############################################################################################
#' Define a general multivariate probability density based on the first two moments.
#' 
#' This function defines a multivariate probability density that can be characterized 
#' by the joint first two moments, viz. the mean and covariance. 
#' @param distribution_type string vector; characterizing the underlying one dimensional
#' distributions.
#' @param mean numerical vector; containing the mean values of the underlying one dimensional 
#' distributions.
#' @param sigma covariance matrix of the underlying one dimensional distributions.
#' @param var_names string vector; names of variables, optional
#' @param var_description string vector; variable descriptions, optional
#' @param ... further options passed to data.frame
#' @return ToDo
#' @export
mvdist2 <- function(distribution_type, mean, sigma, var_names=c(""), var_description=c(""), ...){
  #ToDo: check preconditions
  #ToDo: check this:
  returnObject<-data.frame(distribution_type=distribution_type, 
                           mean=mean, 
                           sigma=sigma, 
                           var_description=var_description, 
                           row.names=var_names,
                           ...)
  class(returnObject)<-"mvdist2"
  return(returnObject)
}
##############################################################################################
# mvdist2_read_csv(filename,...)
##############################################################################################
#' Read an \code{\link{mvdist2}} object from csv file(s)
#'
#' The csv file must have the columns:
#' \tabular{lll}{
#'  \code{distribution_type}  \tab string \tab  \cr
#'  \code{mean}               \tab numeric \tab  mean of the distribution \cr
#'  \code{sigma}              \tab numeric  \tab variance of the distribution
#'  }
#'  Optional columns are:
#'  \tabular{lll}{
#'  \code{var_names}        \tab string \tab  \cr
#'  \code{var_description}  \tab string \tab
#' }
#' Optionally a second csv file in the same directory, which is named
#' \code{<filename>-cov.csv}, can be provided. This file provides the covariance 
#' matrix of the variables. It can be purely numeric or with optional variable 
#' names in the first row and column respectively. 
mvdist2_read_csv <- function(filename,...){
  object <- read.csv(filename, ...)
  mvdist2(distribution_type=object$distribution_type,
          mean=object$mean,
          sigma=object$sigma,
          var_names=object$var_names,
          var_description=object$var_description)
  
}
##############################################################################################
# random.mvdist2(rho,n,method,...)
##############################################################################################
#' Generate general multivariate random numbers based on the first two moments.
#' 
#' This function generates multivariate random numbers for general multivariate 
#' distributions that can be characterized by the joint first two moments, viz. 
#' the mean and covariance. 
#' @param rho \code{\link{mvdist2}} object; Distribution to be randomly sampled. 
#' @param n Number of generated observations.
#' @param method Particular method to be used for random number generation.
#' @param ... Optional arguments to be passed to the particular random number
#'  generating function.
random.mvdist2 <- function(rho,n,method, ...){
  #ToDo
  #ToDo: this replaces the "heart" of make_variables()
  #ToDo: check preconditions
}

>>>>>>> 48e144dcc6c2a9ad66698d489d3691d829d8cd4d
