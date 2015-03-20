#
# file: random_estimate_1d.R
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
# random_estimate_1d(rho,n,method, ...)
##############################################################################################
#' Generate univariate random numbers based on an estimate.
#' 
#' This function generates random numbers for general univariate
#' distributions. 
#' @param rho \code{estimate} object; Univariate distribution to be randomly sampled. 
#' @param n Number of generated observations
#' @param method Particular method to be used for random number generation.
#' @param ... Optional arguments to be passed to the particular random number
#'  generating function.
#'  @details
#'  \code{method} can be either \code{"calculate"} (the default) or \code{"fit"}.
#' @export
random_estimate_1d<-function(rho,n,method="calculate", ...){
  if(0){
    # Rename variables for convenience:
    distribution<-rho["distribution"]
    lower<-rho["lower"]
    upper<-rho["upper"]
  }  
  # Create output vector for the random numbers to be generated
  x<-vector(length=n)
  # Generate the random numbers according to the distribution type:
  if(0){
    #     if ( match(rho["distribution"], c("constant", 
    #                                    "normal", 
    #                                    "pos_normal", 
    #                                    "normal_0_1", 
    #                                    "poisson", 
    #                                    "binomial", 
    #                                    "uniform", 
    #                                    "lognorm", 
    #                                    "lognorm_lim2" ), nomatch = 0 )){
    #       if ( is.numeric(lower) && is.numeric(upper) )
    #         x <-  rdist90ci(distribution=rho["distribution"],
    #                         n=n,
    #                         lower=rho["lower"],
    #                         upper=rho["upper"])
    #       else if ( is.numeric(mean) && is.numeric(sd) )
    #         x <- rdistmsd(distribution=rho["distribution"],
    #                       n=n,
    #                       mean=rho["mean"],
    #                       sd=rho["sd"])
    #       else
    #         stop(rho["distribution"], "-distribution must be suplied with either the 90%-confidence intervall, 
    #            i.e. lower and upper value, or with the mean and standard deviation (sd)!")
    #       
    #     }  else 
  }  
  if(method=="calculate"){
    if(match(rho["distribution"], c("const",
                                 "norm", 
                                 "unif"), nomatch = 0)){
      x <-  rdist90ci_exact(distribution=rho["distribution"],
                            n=n,
                            lower=rho["lower"],
                            upper=rho["upper"])
    }
    else if(match(rho["distribution"], c("posnorm"), nomatch = 0)){
    	x <-  rposnorm90ci_numeric(n=n,
    												lower=rho["lower"],
    												upper=rho["upper"])
    }
    else if(match(rho["distribution"], c("0_1norm"), nomatch = 0)){
    	x <-  r0_1norm90ci_numeric(n=n,
    														 lower=rho["lower"],
    														 upper=rho["upper"])
    }
    else
      stop("\"", rho["distribution"], "\" is not a valid distribution type for method=\"", method, "\".")
  }
  else if (method=="fit"){
    # The next few lines apply a curve fitting procedure based on given distributions and specified quantiles:
    if(match(rho["distribution"], c("norm", 
                                 "beta",
                                 "cauchy",
                                 "logis",
                                 "t",
                                 "chisq",
                                 "exp",
                                 "f",
                                 "gamma",
                                 "lnorm",
                                 "unif",    
                                 "weibull",
                                 "triang",
                                 "gompertz"), nomatch = 0)){
      if( rho["distribution"]=="unif" ) { 
        percentiles<-c(0.05,0.95)
        quantiles<-c(rho["lower"], rho["upper"])
      } else if( is.null(rho["median"]) ){
        #      print("isnull(rho["median"]==TRUE)")
        percentiles<-c(0.05,0.95)
        quantiles<-c(rho["lower"], rho["upper"])
      } else if ( is.na(rho["median"]) ){
        percentiles<-c(0.05,0.95)
        quantiles<-c(rho["lower"], rho["upper"])
      }  else {
        percentiles<-c(0.05,0.5,0.95)
        quantiles<-c(rho["lower"], rho["median"], rho["upper"])
      }   
      x<-rdistq_fit(distribution=rho["distribution"], 
                    n=n, 
                    percentiles=percentiles, 
                    quantiles=quantiles) 
    }  
    else
      stop("\"", rho["distribution"], "\" is not a valid distribution type for method=\"", method, "\".")
  }
  else 
    stop ("method must be either \"calculate\" or \"fit\".")
  # Return generated random numbers:
  x
}
