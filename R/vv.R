#' value varier function
#' 
#' Many variables vary over time and it may not be desirable to ignore this
#' variation in time series analyses. This function produces time series that
#' contain variation from a specified mean and a desired coefficient of
#' variation. A trend can be added to this time series
#' 
#' Note that only one type of trend can be specified. If neither of the trend
#' parameters are NA, the function uses only the original means
#' 
#' @param var_mean mean of the variable to be varied
#' @param var_CV desired coefficient of variation (in percent)
#' @param n integer; number of values to produce
#' @param distribution probability distribution for the introducing variation.
#' Currently only implemented for "normal"
#' @param absolute_trend absolute increment in the var_mean in each time step.
#' Defaults to NA, which means no such absolute value trend is present. If both
#' absolute and relative trends are specified, only original means are used
#' @param relative_trend relative trend in the var_mean in each time step (in
#' percent). Defaults to NA, which means no such relative value trend is
#' present. If both absolute and relative trends are specified, only original
#' means are used
#' @param lower_limit lowest possible value for elements of the resulting vector
#' @param upper_limit upper possible value for elements of the resulting vector
#' @return vector of n numeric values, representing a variable time series,
#' which initially has the mean var_mean, and then increases according to the
#' specified trends. Variation is determined by the given coefficient of
#' variation var_CV
#' @author Eike Luedeling
#' @keywords "value varier" utility
#' @examples
#' 
#' valvar<-vv(100,10,30)
#' plot(valvar)
#' 
#' valvar<-vv(100,10,30,absolute_trend=5)
#' plot(valvar)
#' 
#' valvar<-vv(100,10,30,relative_trend=5)
#' plot(valvar)
#' 
#' @export vv
vv <-
function(var_mean,var_CV,n,distribution="normal",absolute_trend=NA,relative_trend=NA,lower_limit=NA,upper_limit=NA)
  {if(distribution=="normal") {
    if(is.na(absolute_trend)&is.na(relative_trend)) annual_means<-var_mean
    if(!is.na(absolute_trend)&is.na(relative_trend)) annual_means<-rep(var_mean,n)+absolute_trend*c(0:(n-1))
    if(is.na(absolute_trend)&!is.na(relative_trend)) annual_means<-rep(var_mean,n)*(1+relative_trend/100)^c(0:(n-1))
    if(!is.na(absolute_trend)&!is.na(relative_trend)) print("both absolute and relative trend specified. returning only original means")
    out<-rnorm(n,annual_means,abs(annual_means*var_CV/100))
    if(!is.na(lower_limit)) out<-sapply(out,function(x) max(x,lower_limit))
    if(!is.na(upper_limit)) out<-sapply(out,function(x) min(x,upper_limit))
    
    return(out)
  }}
