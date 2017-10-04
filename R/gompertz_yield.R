#calculate time-dependent yields from fruit trees


#' Gompertz function yield prediction for perennials
#' 
#' Yields of trees or other perennial plants have to be simulated in order to
#' predict the outcomes of many interventions. Unlike annual crops, however,
#' trees normally yield nothing for a few years after planting, following which
#' yields gradually increase until they reach a tree-specific maximum. This is
#' simulated with this function, which assumes that a Gompertz function is a
#' good way to describe this (based on the general shape of the curve, not on
#' extensive research...). The function assumes that yields remain at the
#' maximum level, once this is reached. For long simulations, this may not be a
#' valid assumption!
#' The function parameters are estimated based on yield estimates for two
#' points in time, which the user can specify. They are described by a year
#' number and by a percentage of the maximum yield that is attained at that
#' time.
#' 
#' @param max_harvest maximum harvest from the tree (in number of fruits, kg or
#' other units)
#' @param time_to_first_yield_estimate year (or other time unit) number, for
#' which the first yield estimate is provided by first_yield_estimate_percent
#' @param time_to_second_yield_estimate year (or other time unit) number, for
#' which the second yield estimate is provided by second_yield_estimate_percent
#' @param first_yield_estimate_percent percentage of the maximum yield that is
#' attained in the year (or other time unit) given by
#' time_to_first_yield_estimate
#' @param second_yield_estimate_percent percentage of the maximum yield that is
#' attained in the year (or other time unit) given by
#' time_to_second_yield_estimate
#' @param n_years number of years to run the simulation
#' @param var_CV coefficient indicating how much variation should be introduced
#' into the time series of n_targeted_per_year, annual_adoption_rate,
#' perc_disadopt and spontaneous adoption. If this is one numeric value, then
#' this value is used for all variables. If var_CV is a numeric vector with 4
#' elements, each of these is used to introduce variation in one of these
#' variables (in the sequence: n_targeted_per_year, annual_adoption_rate,
#' perc_disadopt and spontaneous adoption). The numbers correspond to the
#' coefficient of variation that the resulting time series should have. The
#' default is 0, for a time series with no artificially introduced variation.
#' See description of the vv function for more details on this.
#' @param no_yield_before_first_estimate boolean variable indicating whether
#' yields before the time unit indicated by time_to_first_yield_estimate should
#' be 0
#' @return vector of n_years numeric values, describing the simulated yield of
#' the perennial. This starts at 0 and, if the simulation runs for a sufficient
#' number of years, approaches max_harvest. If var_CV>0, this time series
#' includes artificial variation.
#' @author Eike Luedeling
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' gompertz_yield(max_harvest=1000,
#'                time_to_first_yield_estimate=5,
#'                time_to_second_yield_estimate=15,
#'                first_yield_estimate_percent=10,
#'                second_yield_estimate_percent=90,
#'                n_years=30,
#'                var_CV=5,
#'                no_yield_before_first_estimate=TRUE)
#' @export gompertz_yield
gompertz_yield<-function(max_harvest,time_to_first_yield_estimate,
                         time_to_second_yield_estimate,
                         first_yield_estimate_percent,
                         second_yield_estimate_percent,n_years,var_CV=0,
                         no_yield_before_first_estimate=TRUE)
{
  a=max_harvest
  t1=time_to_first_yield_estimate
  t2=time_to_second_yield_estimate
  p1=first_yield_estimate_percent/100
  
  p2=second_yield_estimate_percent/100

  if (p1>0.999) p1<-0.999
  if (p1<0.001) p1<-0.001
  if (p2>0.999) p2<-0.999
  if (p2<0.001) p2<-0.001
  if(t1==t2) t2<-t1+1
  c<-sum(log(log(p2)/log(p1))/(t1-t2))
  b<-(-log(p1)/exp(-c*t1))
  gompertz<-function(x){a*exp(-b*exp(-c*x))}
  
  yield_n_years_ideal<-gompertz(1:n_years)
  yield_n_years_real<-unlist(lapply(yield_n_years_ideal,vv,var_CV=var_CV,n=1))
  if(no_yield_before_first_estimate&t1>1)
  {yield_n_years_real[1:min(c(n_years,t1-1))]<-0}
  return(yield_n_years_real)
}


