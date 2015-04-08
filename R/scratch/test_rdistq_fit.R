# 
# file: scratch/test_rdistq_fit.R
#
percentiles<-c(0,0.1,0.5,0.9,1)
#percentiles<-c(0.1,0.2,0.5,0.8)
#percentiles<-c(0.1,0.5,0.9)
#percentiles<-c(0.1,0.9)
#percentiles<-c(0.05)
quantiles<-c(600,950,1000,1150,1400)
#quantiles<-c(200,400,1000,1600)
#quantiles<-c(200,1000,1800)
#quantiles<-c(200,800)
#quantiles<-c(200)
#distribution<-"norm"
#distribution<-"cauchy"
#distribution<-"logis"
#distribution<-"t"
#distribution<-"chisq"
#distribution<-"chisqnc"
#distribution<-"exp"
#distribution<-"f"
#distribution<-"gamma"
#distribution<-"lnorm"
#distribution<-"unif"
#distribution<-"weibull"
#distribution<-"triang"
#distribution="gompertz"
#distribution="pert"
distribution="tnorm"

relativeTolerance=0.1
#verbosity<-2
verbosity<-1
r<-rdistq_fit(distribution=distribution, 
           n=10000, 
           percentiles=percentiles, 
           quantiles=quantiles, 
           relativeTolerance=relativeTolerance, 
           tolConv=0.01, 
           fit.weights=rep(1,length(percentiles)),
           verbosity=verbosity)

#hist(r, xlim=c(-max(abs(quantiles)), max(abs(quantiles))), breaks=100)
hist(r,breaks=100)
#hist(r)
print(quantile(x=r, probs=percentiles))
