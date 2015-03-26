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
##############################################################################################
# generic: random(rho,n,method,...)
##############################################################################################
#' Generate random numbers for a certain probability distribution.
#' 
#' This function generates multivariate random numbers for general multivariate 
#' distributions. 
#' @param rho Distribution to be randomly sampled.
#' @param n Number of generated observations.
#' @param method Particular method to be used for random number generation.
#' @param ... Optional arguments to be passed to the particular random number
#'  generating function.
#'  @export
random <- function(rho,n,method, ...) UseMethod("random")
##############################################################################################
# random.default(rho,n,method,...)
##############################################################################################
#' Generate random numbers based on the first two moments of a certain probability distribution.
#' 
#' This function generates random numbers for general multivariate 
#' distributions that can be characterized by the joint first two moments, viz. 
#' the mean and covariance. 
#' @param rho list; Distribution to be randomly sampled. 
#' @param n Number of generated observations
#' @param method Particular method to be used for random number generation.
#' @param ... Optional arguments to be passed to the particular random number
#'  generating function.
#' @export
#random.default <- function(rho=list(distribution_type,mean,sd),n,method, ...){
random.default <- function(rho,n,method, ...){
  #ToDo: implement
  stop("function random.default() not implemented, yet!")
}


