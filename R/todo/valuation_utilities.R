#
# file: valuation_utilities.R
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
# NPV(x, discount_rate)
##############################################################################################
#' Net Present Value (NPV).
#' 
#' Function that calculates the Net Present Value (NPV).
#' @param x ToDo
#' @param discount_rate ToDo
#' @export
NPV<-function(x,discount_rate) {
	disc<-c(1:length(x))
	disc<-(1-discount_rate/100)^(disc-1)
	return(x*disc)
}
##############################################################################################
# vv(var_mean,var_CV,distribution="normal",n=n_years, absolute_trend=NA,relative_trend=NA)
##############################################################################################
#' Value Varier Function.
#' 
#' Value Varier Function.
#' @param var_mean ToDo
#' @param var_CV ToDo
#' @param distribution ToDo
#' @param n ToDo
#' @param absolute_trend ToDo
#' @param relative_trend ToDo
#' @export
vv<-function(var_mean,var_CV,distribution="normal",n, absolute_trend=NA,relative_trend=NA)
{if(distribution=="normal") {
  if(!is.na(absolute_trend[1])) absolute_trend<-mean(absolute_trend)
  if(!is.na(relative_trend[1])) relative_trend<-mean(relative_trend)
  if(is.na(absolute_trend)&is.na(relative_trend)) annual_means<-var_mean
  if(!is.na(absolute_trend)&is.na(relative_trend)) annual_means<-rep(var_mean,n)+absolute_trend*c(0:(n-1))
  if(is.na(absolute_trend)&!is.na(relative_trend)) annual_means<-rep(var_mean,n)*(1+relative_trend/100)^c(0:(n-1))
  if(!is.na(absolute_trend)&!is.na(relative_trend)) {print("both absolute and relative trend specified. returning only original means")
                                                     annual_means<-var_mean}
  out<-rnorm(n,annual_means,abs(annual_means*var_CV/100))
}
return(out)
}
##############################################################################################
# vv(var_mean,var_CV,distribution="normal",n=n_years, absolute_trend=NA,relative_trend=NA)
##############################################################################################
#' Value Varier Function with NPV Calculation.
#' 
#' Value Varier Function with NPV Calculation.
#' @param var_mean ToDo
#' @param var_CV ToDo
#' @param discount_rate ToDo
#' @param distribution ToDo
#' @param n ToDo
#' @param absolute_trend ToDo
#' @param relative_trend ToDo
#' @export
vvNPV<-function(var_mean,var_CV,discount_rate,distribution="normal",n, absolute_trend=NA,relative_trend=NA)
{if(distribution=="normal") {
  if(!is.na(absolute_trend[1])) absolute_trend<-mean(absolute_trend)
  if(!is.na(relative_trend[1])) relative_trend<-mean(relative_trend)
  if(is.na(absolute_trend)&is.na(relative_trend)) annual_means<-var_mean
  if(!is.na(absolute_trend)&is.na(relative_trend)) annual_means<-rep(var_mean,n)+absolute_trend*c(0:(n-1))
  if(is.na(absolute_trend)&!is.na(relative_trend)) annual_means<-rep(var_mean,n)*(1+relative_trend/100)^c(0:(n-1))
  if(!is.na(absolute_trend)&!is.na(relative_trend)) print("both absolute and relative trend specified. returning only original means")
  out<-NPV(rnorm(n,annual_means,abs(annual_means*var_CV/100)),discount_rate)}
 return(out)
}
