#model_function is constructed, e.g. like this:
  
  profit<-function(){
    # Assign the variable names to the function environement:
    #tt<-table(varnames)
    #for (t in 1:length(tt))
    #  assign(names(tt[t]),as.numeric(x[which(varnames==names(tt[t]))]))
    #for(i in varnames)
    #  assign(i, as.numeric(x[i]))
  
    revenue-costs
  }

varnames<-c("revenue", "costs")
x<-c("costs"=0.3, "revenue"=0.1)

for(i in varnames)
  assign(i, as.numeric(x[i]))

sapply(X=varnames, 
       FUN=function(i) assign(i, as.numeric(x[i]), pos=1)
)
#e <- new.env(parent = emptyenv())
e<-sapply(X=varnames, 
       FUN=function(i) as.numeric(x[i])
)

for(i in varnames)
  i<-as.numeric(x[i])
e_<-as.environment(as.list(e))
evalq(revenue-costs, as.list(e))
eval(body(profit), as.list(e))

with(as.list(e), revenue-costs)
with(as.list(e),body(profit))
do.call(what="profit",args=list(),envir=as.environment(as.list(e)))
##################################################################################################
# Create the estimate object:
variable=c("revenue","costs")
distribution=c("norm","norm")
lower=c(10000,  5000)
upper=c(100000, 50000)
costBenefitEstimate<-as.estimate(variable, distribution, lower, upper)

#model_function is constructed, e.g. like this:

profit<-function(){
  revenue-costs
}
estimate<-costBenefitEstimate
numberOfModelRuns<-10
randomMethod="calculate"
relativeTolerance<-0.10
model_function<-profit
##################################################################################################
x<-random(rho=estimate, n=numberOfModelRuns, 
          method=randomMethod,
          relativeTolerance=relativeTolerance)
# Auxiliary model function:
# model_function_ <- function (x) {
#   sapply(X=row.names(estimate), 
#          FUN=function(i) assign(i, as.numeric(x[i]), pos=1)
#   )
#   model_function()
# }
model_function_ <- function (x) {
  sapply(X=row.names(estimate), 
         FUN=function(i) i<-as.numeric(x[i])
  )
  model_function()
}

y<-do.call(what=rbind,
           args=lapply(X=apply(X=x,
                               MARGIN=1,
                               FUN=model_function_),
                       FUN=unlist))
