examp<-function(x){
  define_variables()
  a<-(x$b+x$c)^x$d+x$e+x$f-x$g
  aa<-x$b+x$c+x$d-x$e+x$f*x$g
  return(list(profit=a,schnecke=aa))
}


examp_glob<-function(x, varnames){
  tt<-table(varnames)
  for (t in 1:length(tt))
    assign(names(tt[t]),as.numeric(x[which(varnames==names(tt[t]))]))
  
  a<-(b+c)^d+e+f-g
  aa<-b+c+d-e+f*g
  return(list(profit=a,schnecke=aa))
}


#est<-estimate_read_csv("D:/DECISIONS/R_development/1504_decisionSupport/estimate.csv")
est<-estimate_read_csv("estimate.csv")
mcs<-mcSimulation(est,examp,numberOfSimulations=10000)
summary(mcs)
hist(mcs,resultName="profit")
wda<-welfareDecisionAnalysis(est,examp,numberOfSimulations=10000)
summary(wda)
ies<-individualEvpiSimulation(examp,est,numberOfSimulations=10000)
summary(ies)
PLS_out<-plsr.mcSimulation(mcs,resultName="schnecke")
uncertaintyAnalysis("D:/DECISIONS/R_development/1504_decisionSupport/estimate.csv",
                    "D:/Temp/merti2/",examp
                    
                    
                    est<-estimate_read_csv("D:/DECISIONS/R_development/1504_decisionSupport/estimate.csv")
                    mcs<-mcSimulation(est,examp_glob,numberOfSimulations=10000,functionSyntax="globalNames")
                    
                    summary(mcs)
                    hist(mcs,resultName="profit")
                    wda<-welfareDecisionAnalysis(est,examp_glob,numberOfSimulations=10000,functionSyntax="globalNames")
                    summary(wda)
                    ies<-individualEvpiSimulation(examp_glob,est,numberOfSimulations=10000,functionSyntax="globalNames")
                    summary(ies)
                    PLS_out<-plsr.mcSimulation(mcs,resultName="schnecke")
                    uncertaintyAnalysis("D:/DECISIONS/R_development/1504_decisionSupport/estimate.csv",
                                        "D:/Temp/merti3/",examp_glob,10000,functionSyntax="globalNames")
                    
                    
                    
                    system.time(mcs<-mcSimulation(est,examp,numberOfSimulations=10000))
                    system.time(mcs<-mcSimulation(est,examp_glob,numberOfSimulations=10000,functionSyntax="globalNames"))
                    
                    
                    estimate<-est
                    model_function<-examp_glob
                    numberOfSimulations<-2000
                    functionSyntax="globalNames"
                    
