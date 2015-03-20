###This function fits a distribution to percentile data. The default for percentiles is 0.05, 0.5 and 0.95, so for the default, the quants
#argument should be a vector with 3 elements. If this is to be longer, the percentiles argument has to be adjusted to match the length of quants.

#require(rriskDistributions)
#distribution<-"gompertz"
#lower<-1
#best<-2
#upper<-3
#samples<-100
#vec<-c(lower,best,upper)

#get.beta.par(p=c(0.05,0.5,0.95),q=vec)
#get.cauchy.par(p=c(0.05,0.5,0.95),q=vec)
#get.chisq.par(p=c(0.05,0.5,0.95),q=vec)
#get.chisqnc.par(p=c(0.05,0.5,0.95),q=c(0.1,0.5,0.9))
#get.exp.par(p=c(0.05,0.5,0.95),q=vec)
#get.f.par(p=c(0.05,0.5,0.95),q=c(0.1,0.5,0.9))
#get.gamma.par(p=c(0.05,0.5,0.95),q=vec)
#get.gompertz.par(p=c(0.05,0.5,0.95),q=vec)
#get.hyper.par(p=c(0.05,0.5,0.95),q=vec)
#get.lnorm.par(p=c(0.05,0.5,0.95),q=vec)
#get.logis.par(p=c(0.05,0.5,0.95),q=vec)
#get.nbinom.par(p=c(0.05,0.5,0.95),q=vec)
#get.norm.par(p=c(0.05,0.5,0.95),q=vec)
#get.norm.sd(p=c(0.05,0.5,0.95),q=vec)
#get.pert.par(p=c(0.05,0.5,0.95),q=vec)
#get.pois.par(p=c(0.05,0.5,0.95),q=vec)
#get.t.par(p=c(0.05,0.5,0.95),q=vec)
#get.tnorm.par(p=c(0.05,0.5,0.95),q=vec)
#get.triang.par(p=c(0.05,0.5,0.95),q=vec)
#get.unif.par(p=c(0.05,0.5,0.95),q=vec)
#get.weibull.par(p=c(0.05,0.5,0.95),q=vec)


sample_fitted_curve<-function(samples,quants,distribution,percentiles=c(0.05,0.5,0.95)){
  require(rriskDistributions)
  
  vec<-quants
  
  capture.output(dists<-try(rriskFitdist.perc(p=percentiles,q=vec,
                                              show.output=FALSE,
                                              tolConv=0.001,fit.weights=rep(1,length(percentiles)))),file='NUL')
  dists$results
  possible_dists<-colnames(dists$results[,3:ncol(dists$results)])[which(!is.na(dists$results[1,3:ncol(dists$results)]))]
  
  output<-NA
  
  if(distribution=="beta") 
    output<-rbeta(samples,dists$results[1,"beta"],dists$results[2,"beta"])
  if(distribution=="norm") 
    output<-rnorm(samples,dists$results[1,"norm"],dists$results[2,"norm"])
  if(distribution=="cauchy") 
    output<-rcauchy(samples,dists$results[1,"cauchy"],dists$results[2,"cauchy"])
  if(distribution=="logis") 
    output<-rlogis(samples,dists$results[1,"logis"],dists$results[2,"logis"])
  if(distribution=="t") 
    output<-rt(samples,dists$results[1,"t"])
  if(distribution=="chisq") 
    output<-rchisq(samples,dists$results[1,"chisq"])
  #if(distribution=="chisqnc") output<-rchisqnc(samples,dists$results[1,"chisqnc"],dists$results[2,"chisqnc"])
  #not sure how chisqnc works
  if(distribution=="exp") 
    output<-rexp(samples,dists$results[1,"exp"])
  if(distribution=="f") 
    output<-rf(samples,dists$results[1,"f"],dists$results[2,"f"])
  if(distribution=="gamma") 
    output<-rgamma(samples,dists$results[1,"gamma"],dists$results[2,"gamma"])
  if(distribution=="lnorm") 
    output<-rlnorm(samples,dists$results[1,"lnorm"],dists$results[2,"lnorm"])
  if(distribution=="unif") 
    output<-runif(samples,dists$results[1,"unif"],dists$results[2,"unif"])
  #unif needs 2 quantiles (can't handle 3)
  if(distribution=="weibull") 
    output<-rweibull(samples,dists$results[1,"weibull"],dists$results[2,"weibull"])
  if(distribution=="triang") 
    output<-rtriang(samples,dists$results[1,"triang"],dists$results[2,"triang"],dists$results[3,"triang"])
  if(distribution=="gompertz") 
    output<-rgompertz(samples,dists$results[1,"gompertz"],dists$results[2,"gompertz"])
  if(distribution=="pert") 
    output<-rpert(samples,dists$results[1,"pert"],dists$results[2,"pert"],dists$results[3,"pert"],dists$results[4,"pert"])
  #pert needs 4 quantiles
  if(distribution=="tnorm") 
    output<-rtnorm(samples,dists$results[1,"tnorm"],dists$results[2,"tnorm"],dists$results[3,"tnorm"],dists$results[4,"tnorm"])
  #tnorm needs 4 quantiles
  if(is.na(output[1])) 
    output<-paste(distribution,"distribution could not be fitted. One of the following should work:",paste(possible_dists,collapse=", "))
  
  return(output)
}
