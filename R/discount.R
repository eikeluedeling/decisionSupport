#' Discount time series for Net Present Value (NPV) calculation
#' 
#' This function discounts values along a time series, applying the specified
#' discount rate. It can also calculate the Net Present Value (NPV), which is
#' the sum of these discounted values.
#' 
#' 
#' @param x numeric vector, typically containing time series data of costs or
#' benefits
#' @param discount_rate numeric; the discount rate (in percent), expressing the
#' time preference of whoever is evaluating these data economically
#' @param calculate_NPV boolean; if set to TRUE, the discounted time values are
#' summed, otherwise, they are returned as a vector
#' @return If calculate_NPV=TRUE, the function returns the Net Present Value
#' (NPV) as a numeric value. If calculate_NPV=FALSE, the time-discounted values
#' are returned as a numeric vector.
#' @author Eike Luedeling
#' @keywords "discount rate" utility
#' @examples
#' 
#' x<-c(3,6,2,5,4,3,9,0,110)
#' discount_rate<-5
#' 
#' discount(x,discount_rate)
#' discount(x,discount_rate,calculate_NPV=TRUE)
#' 
#' 
#' @export discount
discount <-
function(x,discount_rate,calculate_NPV=FALSE)
  {disc<-c(1:length(x))
  disc<-1/(1+discount_rate/100)^(disc-1)
  discounted<-x*disc
  if(calculate_NPV) discounted<-sum(discounted)
  return(discounted)
  }
