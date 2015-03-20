# file: scratch.R


# Sample size
n=10000

x1<-data.frame(firstComponent=vector(length=n),secondComponent=vector(length=n))
x1$firstComponent <- rnorm(n)
#x1$secondComponent <- rep(0,n)
x1$secondComponent <- rnorm(n)

# Define a function f(x) = ax + b:
f <- function(x,a,b){
  x*a+b
}

# Define a function g(x) = ax + b:
g <- function(x,a,b){
  x["firstComponent"]*a+b+x["secondComponent"]
}
h<- function(x,a,b){
  x[,"firstComponent"]*a+b+x[,"secondComponent"]
}
library(microbenchmark)
a=2
b=3
microbenchmark(
#  directf<-f(x1,a=2,b=3),
  directg<-g(x1,a=a,b=b),
  directgm<-g(as.matrix(x1),a=a,b=b),
  #10-times faster than the others!!!:
  directhm<-h(as.matrix(x1),a=a,b=b),
#  indirectf<-apply(X=x1, 1, FUN=f, a=2, b=3),
  indirectg<-apply(X=x1, 1,FUN=g, a=a, b=b),
  indirectgm<-apply(X=as.matrix(x1), 1,FUN=g, a=a, b=b),
  indirectg1<-apply(t(sapply(apply(X=x1,1,FUN=g, a=a, b=b),c)),2,as.numeric),
  indirectg1m<-apply(t(sapply(apply(X=as.matrix(x1),1,FUN=g, a=a, b=b),c)),2,as.numeric)

)
data.frame(directg,directgm,directhm,indirectg,indirectgm,indirectg1,indirectg1m,x1["firstComponent"]*a+b)
#data.frame(directf,directg,indirectf,indirectg)
###############################################################################################################
# Profit Example
###############################################################################################################
profit <- function(x) {
  x["revenue"] - x["costs"]
}
profit1 <- function(x) {
  x[,"revenue"] - x[,"costs"]
}
# Sample size
n=10000

x2<-data.frame(revenue=vector(length=n),costs=vector(length=n))
x2$revenue <- rnorm(n)
#x1$secondComponent <- rep(0,n)
x2$costs <- rnorm(n)
microbenchmark(
  direct<-profit(x2),
  directm<-profit1(as.matrix(x2)),
  indirect<-apply(X=x2, 1,FUN=profit),
  indirectm<-apply(X=as.matrix(x2), 1,FUN=profit),
  indirect1<-apply(t(sapply(apply(X=x2,1,FUN=profit),c)),2,as.numeric),
  indirect1m<-apply(t(sapply(apply(X=as.matrix(x2),1,FUN=profit),c)),2,as.numeric)
)
data.frame(direct,directm, indirect, indirectm,indirect1,indirect1m, x2["revenue"] - x2["costs"])[1:20,]
###########################################
# testing rdistq_fit()
##########################################
distribution="norm"
#distribution="lnorm"
#distribution="fantasy"
n=10000
percentiles=c(0.05,0.95)
quantiles=c(10, 100)
x <- rdistq_fit (distribution=distribution, n=n, percentiles=percentiles, quantiles=quantiles)
summary(x)
sd(x)
quantile(x, probs=percentiles)
hist(x)
###########################################
# testing random_estimate_1d()
##########################################
distribution="norm"
n=10
lower=10
median=55
upper=100
variable="costs"
#rho<-data.frame(distribution,lower,median,upper)
#rho<-data.frame(distribution,lower,upper,variable)
rho<-data.frame(distribution,lower,upper,row.names=c(variable))
rho
x<-random_estimate_1d(rho=rho, n=n)
quantile(x, probs=c(0.05,0.5,0.95))
summary(x)
sd(x)
hist(x)
(quantiles=qnorm(p=c(0.05,0.5,0.95), mean=mean(x), sd=sd(x)))
names(x)
###########################################
# testing random.estimate()
##########################################
n=10000
profit_1<-estimate_read_csv("tests/testthat/profit-1.csv")
profit_1
x<-random(rho=profit_1,n=n)
summary(x)
apply(x,MARGIN=2,FUN=sd)
apply(x,MARGIN=2,FUN=mean)
apply(x,MARGIN=2,FUN=hist)
###########################################
# experiments for mcSimulation()
##########################################

numberOfSimulations=10000
randomMethod=NULL
estimate<-estimate_read_csv("tests/testthat/profit-1.csv")
############################
# x as matrix:
profit_mat<-function(x){
  x[,"revenue"]-x[,"costs"]
}
model_function=profit_mat
x<-random(rho=estimate, n=numberOfSimulations, method=randomMethod)
#y<-model_function(as.matrix(x))
y<-model_function(x)
#is.matrix(x)
#is.data.frame(x)
#as.data.frame(x)[1:10,]
#is.data.frame(as.data.frame(x))
summary(x)
summary(y)
hist(y)
mean(y)
sd(y)
data.frame(y,x)[1:10,]
data.frame(y=y,x=x)[1:10,]
returnObject<-data.frame(y=y, x=x)
##########################
# x as data.frame (1):
profit_df1<-function(x){
  x["revenue"]-x["costs"]
}
model_function=profit_df1
x<-random(rho=estimate, n=numberOfSimulations, method="exact")
y<-model_function(as.data.frame(x))
summary(x)
summary(y)
summary(as.matrix(y))
summary(data.matrix(y))
hist(y)
hist(as.matrix(y))
hist(data.matrix(y))
mean(as.matrix(y))
sd(as.matrix(y))
percentiles=c(0.05,0.95)
quantile(as.matrix(y), probs=percentiles)
estimate$base
data.frame(y,x)[1:10,]
data.frame(y=y,x=x)[1:10,]
##########################
# x as data.frame (2):
profit_df2<-function(x){
  x$revenue-x$costs
}
model_function=profit_df2
x<-random(rho=estimate, n=numberOfSimulations, method=randomMethod)
y<-model_function(as.data.frame(x))
summary(x)
summary(y)
summary(as.matrix(y))
summary(data.matrix(y))
hist(y)
mean(y)
sd(y)
percentiles=c(0.05,0.95)
quantile(as.matrix(y), probs=percentiles)
estimate$base

data.frame(y,x)[1:10,]
data.frame(y=y,x=x)[1:10,]
###############################
# Tensor product
s = c(1,
      2,
      3)
C_1=diag(c(1,1,1))
C_2=matrix(rep(1,9),nrow=3,ncol=3)
s*C_1
s*C_2
(t(s*C_1))*s
(t(s*C_2))*s
############################################
profit<-function(x){
	y<-numeric()
	y$profit1<-x$revenue-x$costs
	y$profit2<- x$revenue
	y
}
# Create the estimate object:
variable=c("revenue","costs")
distribution=c("norm","norm")
lower=c(10000,  5000)
upper=c(100000, 50000)
costBenefitEstimate<-estimate(variable, distribution, lower, upper)
# Perform the Monte Carlo simulation:
predictionProfit<-mcSimulation( estimate=costBenefitEstimate,
																 model_function=profit,
																 numberOfSimulations=100000,
																 functionSyntax="data.frameNames")
# Show the simulation results:
print(summary(predictionProfit))
hist(predictionProfit)
############################################
# Global names
############################################
n=10000
# Create the estimate object:
variable=c("revenue","costs")
distribution=c("norm","norm")
lower=c(10000,  5000)
upper=c(100000, 50000)
costBenefitEstimate<-estimate(variable, distribution, lower, upper)
# (a) Define the model function without name for the return value:
profit1<-function(){
	list(Profit=revenue-costs)
}
x<-random(rho=costBenefitEstimate,n=n)
attach(data.frame(x))
y<-profit1()
detach(data.frame(x))
summary(as.data.frame(y))
hist(y$Profit)
