MonteCarlo<-function(result_path,input_file,fun,iterations,write_table=TRUE,indicators=FALSE,log_scales=FALSE){
  require(pls)
  require(chillR)
  
  variable_list<-read.csv(input_file)
  if(length(which(variable_list$indicator=="only")>0)) {
    indi_only<-as.character(variable_list[which(variable_list$indicator=="only"),1])
    variable_list<-variable_list[which(!variable_list$indicator=="only"),]
  }
  
  res<-make_variables(input_file,iterations)
  
  results<-res
  cols<-colnames(res)[2:ncol(res)]
  function_results<-apply(t(sapply(apply(results,1,fun,varnames=cols),c)),2,as.numeric)
  results<-cbind(results[,2:ncol(results)],function_results)
  resultnames<-names(fun(res[1,],varnames=cols))
  if(indicators==TRUE) 
    resultnames<-resultnames[which(!resultnames%in%indi_only)]
  colnames(results)[(ncol(results)-length(resultnames)+1):ncol(results)]<-resultnames
  full_results<-results
  nresultcols<-length(resultnames)
  #colnames(results)[(ncol(results)-nresultcols+1):ncol(results)]<-resultnames
  
  if(indicators==TRUE){
    results_for_standard_PLS<-results[,which(!colnames(results)%in%indi_only)]
    indicator_list<-c(as.character(variable_list[which(variable_list$indicator=='yes'),1]),indi_only)
    results_for_indicator_PLS<-results[,which(colnames(results)%in%c(resultnames,indicator_list))]
  } else
    results_for_standard_PLS<-results
  
  for (ress in 1:length(resultnames)){  
    #make PLS plots for the full list of inputs
    dep<-unlist(results_for_standard_PLS[,resultnames[ress]])
    
    resu<-results_for_standard_PLS[,which(apply(results_for_standard_PLS,2,sd)>0)]
    dat<-resu #[,2:ncol(resu)]
    indep<-as.matrix(dat[,1:(ncol(dat)-nresultcols)])
    ccc<-colnames(indep) 
    indep<-apply(indep,c(1,2),as.numeric) 
    colnames(indep)<-ccc 
    dat$indep<-indep
    pls_out<-plsr(dep~indep,data=dat,method="oscorespls",scale=TRUE,ncomp=2)  
    vip<-VIP(pls_out)["Comp 1",]
    coef<-pls_out$coefficients[,,1]
    color_bars<-color_bar_maker(vip,coef,threshold=0.8,col1="RED",col2="DARK GREEN",col3="DARK GREY")
    
    #barplot(coef,horiz=TRUE,las=1,col=color_bars,cex.names=4,cex.axis=3,main="Model coefficient",cex.main=5,axes=FALSE)
    
    vip_select<-vip[order(vip,decreasing=TRUE)[1:50]]
    if(length(vip)<50) vip_select<-vip_select[1:length(vip)]
    col_select<-color_bars[order(vip,decreasing=TRUE)[1:50]]
    if(length(vip)<50) col_select<-col_select[1:length(vip)]
    #par(mar=c(5.1,20,4.1,4.1))
    
    png(paste(result_path,resultnames[ress],"_PLS_VIP.png",sep=""),height=1400,width=1400)
    par(mar=c(5.1,55,4.1,2.1))
    barplot(rev(vip_select),horiz=TRUE,las=1,col=rev(col_select),cex.names=2,cex.axis=1,main="VIP for most important variables",cex.main=2,axes=FALSE)
    axis(side=1,cex.axis=2,lwd=5,padj=0.7)
    abline(v=0.8,lwd=3)
    dev.off()
    
    pls_tab<-cbind(vip,coef)
    colnames(pls_tab)<-c("VIP","Coefficient")
    if (write_table) write.csv(pls_tab,paste(result_path,resultnames[ress],"_pls_results.csv",sep=""))
    #par(mar=c(5.1,4.1,4.1,2.1))
    
    #PLS plots for the indicators only
    if(indicators==TRUE){
      dep<-unlist(results_for_indicator_PLS[,resultnames[ress]])
      
      resu<-results_for_indicator_PLS[,which(apply(results_for_indicator_PLS,2,sd)>0)]
      dat<-resu #[,2:ncol(resu)]
      indep<-as.matrix(dat[,1:(ncol(dat)-nresultcols)])
      ccc<-colnames(indep) 
      indep<-apply(indep,c(1,2),as.numeric) 
      colnames(indep)<-ccc 
      dat$indep<-indep
      pls_out<-try(plsr(dep~indep,data=dat,method="oscorespls",scale=TRUE,ncomp=2),silent=TRUE)
      if(strsplit(pls_out[1]," ")[[1]][1]=="Error") 
        Error<-TRUE 
      else 
        Error<-FALSE
      if(!Error){
        vip<-VIP(pls_out)["Comp 1",]
        coef<-pls_out$coefficients[,,1]
        color_bars<-color_bar_maker(vip,coef,threshold=0.8,col1="RED",col2="DARK GREEN",col3="DARK GREY")
        
        #barplot(coef,horiz=TRUE,las=1,col=color_bars,cex.names=4,cex.axis=3,main="Model coefficient",cex.main=5,axes=FALSE)
        
        vip_select<-vip[order(vip,decreasing=TRUE)[1:50]]
        if(length(vip) < 50) 
          vip_select<-vip_select[1:length(vip)]
        col_select<-color_bars[order(vip,decreasing=TRUE)[1:50]]
        if(length(vip) < 50) 
          col_select<-col_select[1:length(vip)]
        #par(mar=c(5.1,20,4.1,4.1))
        
        png(paste(result_path,resultnames[ress],"_PLS_VIP_indicators.png",sep=""),height=1400,width=1400)
        par(mar=c(5.1,55,4.1,2.1))
        barplot(rev(vip_select),horiz=TRUE,las=1,col=rev(col_select),cex.names=2,cex.axis=1,main="VIP for most important variables",cex.main=2,axes=FALSE)
        axis(side=1,cex.axis=2,lwd=5,padj=0.7)
        abline(v=0.8,lwd=3)
        dev.off()
        
        pls_tab<-cbind(vip,coef)
        colnames(pls_tab)<-c("VIP","Coefficient")
      } else  
        pls_tab<-data.frame(Error=c("Error in PLS analysis",pls_out))
      if (write_table) 
        write.csv(pls_tab,paste(result_path,resultnames[ress],"_pls_results_indicators.csv",sep=""))    
    }
    
    #now make distribution plots
    results<-results_for_standard_PLS
    
    png(paste(result_path,resultnames[ress],"_distribution.png",sep=""),width=1000,height=500)
    par(mar=c(5.1,5.1,4.1,2.1))
    restemp<-results[,resultnames[ress]]
    
    if(log_scales==TRUE){
      if(length(which(restemp<min(restemp+(max(restemp)-min(restemp))/2)))/length(restemp)>0.9) {
        histo<-hist(log(results[,resultnames[ress]],10),nclass=100,xlab=resultnames[ress],main=resultnames[ress])} 
      else      
        histo<-hist(results[,resultnames[ress]],nclass=100,xlab=resultnames[ress],main=resultnames[ress])
    } else
      histo<-hist(results[,resultnames[ress]],nclass=100,xlab=resultnames[ress],main=resultnames[ress]) 
    cumfreq<-c()
    histcol<-c()
    lh<-length(histo$breaks)
    for(bb in 1:lh){ 
      if(log_scales==TRUE){
        if(length(which(restemp<min(restemp+(max(restemp)-min(restemp))/2)))/length(restemp)>0.9)
          cumfreq[bb]<-length(which(log(results[,resultnames[ress]],10)>histo$breaks[bb]))
        else
          cumfreq[bb]<-length(which(results[,resultnames[ress]]>histo$breaks[bb]))
      } else
        cumfreq[bb]<-length(which(results[,resultnames[ress]]>histo$breaks[bb]))
      histcol[bb]<-"GREY"
      if(cumfreq[bb]<0.95*nrow(res)) histcol[bb]<-"YELLOW"
      if(cumfreq[bb]<0.75*nrow(res)) histcol[bb]<-"ORANGE"   
      if(cumfreq[bb]<0.55*nrow(res)) histcol[bb]<-"DARK GREEN"
      if(cumfreq[bb]<0.45*nrow(res)) histcol[bb]<-"ORANGE"
      if(cumfreq[bb]<0.25*nrow(res)) histcol[bb]<-"YELLOW"
      if(cumfreq[bb]<0.05*nrow(res)) histcol[bb]<-"GREY"
    }
    
    if(log_scales==TRUE){
      if(length(which(restemp<min(restemp+(max(restemp)-min(restemp))/2)))/length(restemp)>0.9) {
        #make log scale
        hist(log(results[,resultnames[ress]],10),nclass=100,xlab=paste(resultnames[ress],"(log scale)"),main=resultnames[ress],col=histcol,lwd=3,cex.lab=2,cex.axis=2,axes=FALSE,prob=TRUE)
        lll<-as.character(10^(as.numeric(axis(1))) )
        ats<-as.numeric(axis(1))
        hist(log(results[,resultnames[ress]],10),nclass=100,xlab=paste(resultnames[ress],"(log scale)"),main=resultnames[ress],col=histcol,lwd=3,cex.lab=2,cex.axis=2,axes=FALSE,prob=TRUE)
        axis(2)
        axis(1,labels=lll,at=ats)
      } else
        hist(results[,resultnames[ress]],nclass=100,xlab=resultnames[ress],main=resultnames[ress],col=histcol,lwd=3,cex.lab=2,cex.axis=2,prob=TRUE)
    } else
      hist(results[,resultnames[ress]],nclass=100,xlab=resultnames[ress],main=resultnames[ress],col=histcol,lwd=3,cex.lab=2,cex.axis=2,prob=TRUE)     
    dev.off()
  } 
  if (write_table) write.csv(full_results,paste(result_path,"output_table.csv",sep=""))
}


