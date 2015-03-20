#MonteCaRlo utilities

variable_definition<-function (x=x, varnames=varnames){
  tt<-table(varnames)
  for (t in 1:length(tt))
    assign(names(tt[t]),as.numeric(x[which(varnames==names(tt[t]))+1]))
} 

NPV<-function(x,discount_rate) {
  disc<-c(1:length(x))
  disc<-(1-discount_rate/100)^(disc-1)
  return(x*disc)
}

#value varier function
vv<-function(var_mean,var_CV,distribution="normal",n=n_years, absolute_trend=NA,relative_trend=NA) {
  if(distribution=="normal") {
    if(!is.na(absolute_trend[1])) 
      absolute_trend<-mean(absolute_trend)
    if(!is.na(relative_trend[1])) 
      relative_trend<-mean(relative_trend)
    if(is.na(absolute_trend)&is.na(relative_trend)) 
      annual_means<-var_mean
    if(!is.na(absolute_trend)&is.na(relative_trend)) 
      annual_means<-rep(var_mean,n)+absolute_trend*c(0:(n-1))
    if(is.na(absolute_trend)&!is.na(relative_trend)) 
      annual_means<-rep(var_mean,n)*(1+relative_trend/100)^c(0:(n-1))
    if( !is.na(absolute_trend) & !is.na(relative_trend) ) {
      print("both absolute and relative trend specified. returning only original means")
      annual_means<-var_mean
    }
    out<-rnorm(n,annual_means,abs(annual_means*var_CV/100))
  }
  return(out)
}


#value varier function with NPV calculation
vvNPV<-function(var_mean,var_CV,discount_rate,distribution="normal",n=n_years, absolute_trend=NA,relative_trend=NA){
  if(distribution=="normal") {
    if(!is.na(absolute_trend[1])) 
      absolute_trend<-mean(absolute_trend)
    if(!is.na(relative_trend[1])) 
      relative_trend<-mean(relative_trend)
    if(is.na(absolute_trend)&is.na(relative_trend)) 
      annual_means<-var_mean
    if(!is.na(absolute_trend)&is.na(relative_trend)) 
      annual_means<-rep(var_mean,n)+absolute_trend*c(0:(n-1))
    if(is.na(absolute_trend)&!is.na(relative_trend))
      annual_means<-rep(var_mean,n)*(1+relative_trend/100)^c(0:(n-1))
    if(!is.na(absolute_trend)&!is.na(relative_trend)) 
      print("both absolute and relative trend specified. returning only original means")
    out<-NPV(rnorm(n,annual_means,abs(annual_means*var_CV/100)),discount_rate)
  }
  return(out)
}
