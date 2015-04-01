
#r_coefficients<-list(c(0.1,0.4,0.5),c(0.9,0.2),c(0.4))
#means<-c(4,2,10,300)
#CVs<-c(40,70,20,5)
#var_names<-c("a","b","c","d")

#MC_utilities functions

make_correlated_variables<-function(n,r_coefficients,means,CVs,var_names,distribution_types){
  library(MASS)
  
  n_vars<-length(r_coefficients)+1
  rel_mat<-matrix(rep(1,n_vars*n_vars),nrow=n_vars,byrow=T)
  for(j in 1:(n_vars-1)){
    rel_mat[(1+j):n_vars,j]<-r_coefficients[[j]]
    rel_mat[j,(1+j):n_vars]<-r_coefficients[[j]]}
  mu<-rep(0,n_vars)
  mat <- mvrnorm(n, Sigma = rel_mat, mu = mu, empirical = TRUE)
  outmatrix<-matrix(rep(NA,n*n_vars),nrow=n)
  for(i in 1:n_vars){
    outmatrix[,i]<-means[i]+mat[,i]*means[i]*CVs[i]/100
  }
  colnames(outmatrix)<-var_names
  for (i in 1:ncol(outmatrix)){
    if (distribution_types[i]=="pos_normal")
      outmatrix[which(outmatrix[,i]<0),i]<-0
    if (distribution_types[i]=="normal_0_1"){
      outmatrix[which(outmatrix[,i]<0),i]<-0
      outmatrix[which(outmatrix[,i]>1),i]<-1}
  }
  
  return(outmatrix)
}



#MAKE VARIABLES, INCLUDES CORRELATED VARIABLES
make_variables<-function(input_file=tab,iterations=5){  
if(0){  
  variable_list<-read.csv(input_file)
  variable_list<-variable_list[which(!variable_list$variable==""),]
}

  if(length(which(variable_list$indicator=="only")>0)) {
    indi_only<-as.character(variable_list[which(variable_list$indicator=="only"),1])
    variable_list<-variable_list[which(!variable_list$indicator=="only"),]
  }
  #variable_list_expansion
  for (l in 1:nrow(variable_list))
    if(!(variable_list[l,"start"]==variable_list[l,"end"]))
      for(c in 1:(variable_list[l,"end"]-variable_list[l,"start"])){
        variable_list<-rbind(variable_list,variable_list[l,])
        variable_list[nrow(variable_list),"start"]<-variable_list[l,"start"]+c
      }  
  
  variable_list<-variable_list[order(variable_list[,1],variable_list[,6]),]
  
  correlated_variables<-as.character(variable_list[which(variable_list$distribution=="correlated"),"variable"])
  if(length(correlated_variables>0)) 
    variable_list<-variable_list[which(!variable_list$distribution=="correlated"),]

if(0){ 
  res<-data.frame(iter=c(1:iterations))
  
  for (r in 1:nrow(variable_list)){
    low<-as.numeric(as.numeric(gsub(",","", as.character(variable_list[r,"lower"]))))
    up<-as.numeric(as.numeric(gsub(",","", as.character(variable_list[r,"upper"]))))
    dis<-variable_list[r,"distribution"]
    
    if(dis=="constant")   
      res[,r+1]<-rep(low,iterations)
    if(dis=="normal")     
      res[,r+1]<-rnorm(iterations,mean=mean(c(low,up)),sd=(mean(c(low,up))-low)/1.645)
    if(dis=="pos_normal") {
      res[,r+1]<-rnorm(iterations,mean=mean(c(low,up)),sd=(mean(c(low,up))-low)/1.645)
      if(length(which(res[,r+1]<0)>0)) res[which(res[,r+1]<0),r+1]<-0
    }
    if(dis=="normal_0_1") {
      res[,r+1]<-rnorm(iterations,mean=mean(c(low,up)),sd=(mean(c(low,up))-low)/1.645)
      if(length(which(res[,r+1]<0)>0)) res[which(res[,r+1]<0),r+1]<-0
      if(length(which(res[,r+1]>1)>0)) res[which(res[,r+1]>1),r+1]<-1
    }
    if(dis=="poisson")    
      res[,r+1]<-rpois(iterations, mean(c(low,up)))
    if(dis=="binomial")   
      res[,r+1]<-rbinom(iterations,1,low)
    if(dis=="uniform")    
      res[,r+1]<-runif(iterations,low,up)
    if(dis=="lognorm")    
      res[,r+1]<-rlnorm(iterations,meanlog=mean(log(up),log(low)),sdlog=(mean(c(log(up),log(low)))-log(low))/1.645) 
    if(dis=="lognorm_lim2") {
      temp<-rlnorm(iterations,meanlog=mean(log(up),log(low)),sdlog=(mean(c(log(up),log(low)))-log(low))/1.645)
      temp[which(temp>2*up)]<-2*up
      res[,r+1]<-temp
    }
    
    #the next few lines apply a curve fitting procedure based on given distributions and specified quantiles
    if(match(dis, c("beta","norm","cauchy","logis","f","t","chisq","exp","gamma",
                    "lnorm","unif","weibull","triang","gompertz"), nomatch = 0) > 0){
      if(dis=="unif") {
        quants<-c(low,up)
        percentiles<-c(0.05,0.95)
      } else {
        quants<-c(low,variable_list[r,"median"],up)
        percentiles<-c(0.05,0.5,0.95)
      }   
      res[,r+1]<-sample_fitted_curve(samples=iterations,quants=quants,distribution=dis,percentiles=percentiles) 
    }   
  }
}
  colnames(res)<-c("iter",as.character(variable_list[,"variable"]))  
  cols<-colnames(res)[2:ncol(res)]
  varnames<-cols
  tt<-table(varnames)
  for (t in 1:length(tt))
    assign(names(tt[t]),as.numeric(res[,which(varnames==names(tt[t]))+1]), envir = .GlobalEnv)   
  
  #MAKE CORRELATED VARIABLES
  
  if(length(correlated_variables>0)){
    tabl<-read.csv(paste(input_file,"_correlations.csv",sep=""),header=FALSE)
    correlation_starts<-which(tabl[,1]=="variable")
    if(length(correlation_starts)==1) correlation_ends<-nrow(tabl) else
      correlation_ends<-c(correlation_starts[2:length(correlation_starts)]-1,nrow(tabl))
    
    res_corr<-data.frame(iter=1:iterations)
    for(i in 1:length(correlation_starts)){
      col_names<-as.character(unlist(tabl[correlation_starts[i],]))
      col_names<-sapply(tabl[correlation_starts[i],],as.character)
      correl<-tabl[(correlation_starts[i]+1):correlation_ends[i],]
      colnames(correl)<-as.character(col_names)
      correl[,"lower"]<-as.numeric(as.numeric(gsub(",","", as.character(correl[,"lower"]))))
      correl[,"upper"]<-as.numeric(as.numeric(gsub(",","", as.character(correl[,"upper"]))))
      correl<-correl[which(!is.na(correl[,"upper"])),]
      
      var_names<-correl$variable
      dis_types<-correl$distribution
      r_coefficients<-list()
      for(i in 1:(length(var_names)-1))
        r_coefficients[[i]]<-as.numeric(as.character(correl[(i+1):length(var_names),as.character(var_names[i])]))
      
      means<-(correl[,"lower"]+correl[,"upper"])/2
      CVs<-((means-correl[,"lower"])/1.645)/means*100
      
      tt<-make_correlated_variables(iterations,r_coefficients,means,CVs,var_names,dis_types)
      for (t in 1:length(var_names))
        assign(as.character(var_names[t]),as.numeric(tt[,t]), envir = .GlobalEnv)   
      for(j in 1:length(var_names)) {res_corr<-cbind(res_corr,tt[,j])
                                     colnames(res_corr)[ncol(res_corr)]<-as.character(var_names[j])
      }
    }
    res<-cbind(res,res_corr[,2:ncol(res_corr)]) 
  }
  return(res)
}




