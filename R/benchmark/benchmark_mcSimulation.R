#
# file: benchmark_mcSimulation.R
#
# R package: decisionSupport
# 
# Authors (ToDo order?): 
#   Lutz Göhring <lutz.goehring@gmx.de>
#   Eike Luedeling (ICRAF) <eike@eikeluedeling.com>
#
# Affiliation: ToDo
# 
# License: ToDo
#
##############################################################################################
source("mcSimulation_apply.R")
library(microbenchmark)
###############################################################################################################
# Profit Example
###############################################################################################################
profit0<-function(x){
  x$revenue-x$costs
}
profit <- function(x) {
  x["revenue"] - x["costs"]
}
profit1 <- function(x) {
  x[,"revenue"] - x[,"costs"]
}
profit2 <- function(x) {
	x[["revenue"]] - x[["costs"]]
}
# Sample size
n=10000

c_0.95=qnorm(0.95)
variable=c("revenue","costs")
distribution=c("norm","norm")
# Characterize standard normal distributions by confidence intervals: 
lower=c(-c_0.95,  -c_0.95)
upper=c(c_0.95, c_0.95)
estimate<-as.estimate(distribution=distribution,lower=lower,upper=upper,row.names=variable)
#estimate=list(marginal=marginal, correlation_matrix="" )
#class(estimate)<-"estimate"

microbenchmark(
{
  set.seed(100)
  direct0<-mcSimulation(estimate=estimate, model_function=profit0, numberOfModelRuns=n, 
                       randomMethod="exact", functionSyntax="data.frameNames")  
},  
{
  set.seed(100)
  direct<-mcSimulation(estimate=estimate, model_function=profit, numberOfModelRuns=n, 
                       randomMethod="exact", functionSyntax="data.frameNames")  
},
{
  set.seed(100)
  direct1m<-mcSimulation(estimate=estimate, model_function=profit1, numberOfModelRuns=n, 
                        randomMethod="exact", functionSyntax="matrixNames")
},
{
	set.seed(100)
	direct2<-mcSimulation(estimate=estimate, model_function=profit2, numberOfModelRuns=n, 
												 randomMethod="exact", functionSyntax="data.frameNames")
},
{
  set.seed(100)
  indirectSimp<-mcSimulation_apply(estimate=estimate, model_function=profit, numberOfModelRuns=n, 
                               randomMethod="exact", xAs="data.frame", applyMethod="simple")
},
{
  set.seed(100)
  indirectmSimp<-mcSimulation_apply(estimate=estimate, model_function=profit, numberOfModelRuns=n, 
                               randomMethod="exact", xAs="matrix", applyMethod="simple")
},
# This syntax does not work: 
# {
#   set.seed(100)
#   indirect<-mcSimulation_apply(estimate=estimate, model_function=profit0, numberOfModelRuns=n, 
#                                randomMethod="exact", xAs="data.frame", applyMethod="simple")
# },
# This syntax does not work: 
# {
#   set.seed(100)
#   indirect<-mcSimulation_apply(estimate=estimate, model_function=profit0, numberOfModelRuns=n, 
#                                randomMethod="exact", xAs="matrix", applyMethod="simple")
# },
# This syntax does not work: 
# {
#   set.seed(100)
#   indirect0<-mcSimulation_apply(estimate=estimate, model_function=profit0, numberOfModelRuns=n, 
#                                randomMethod="exact", xAs="data.frame")
# },
{
  set.seed(100)
  indirect<-mcSimulation_apply(estimate=estimate, model_function=profit, numberOfModelRuns=n, 
                                randomMethod="exact", xAs="data.frame")
},
# This syntax does not work: 
# {
#   set.seed(100)
#   indirect0m<-mcSimulation_apply(estimate=estimate, model_function=profit0, numberOfModelRuns=n, 
#                                 randomMethod="exact", xAs="matrix")
# },
{
  set.seed(100)
  indirectm<-mcSimulation_apply(estimate=estimate, model_function=profit, numberOfModelRuns=n, 
                                 randomMethod="exact", xAs="matrix")
})
set.seed(100)
x<-random(rho=estimate, n=n, method="exact")
data.frame(direct0$y,direct$y,direct1m$y,direct2$y, indirectSimp$y, indirectmSimp$y, indirect$y,indirectm$y, x[,"revenue"] - x[,"costs"])[1:20,]
