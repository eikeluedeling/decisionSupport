summarize_distributions<-function(table,variables){
  tab<-read.csv(table)
  results<-data.frame(matrix(ncol=12,nrow=length(variables)))
  colnames(results)<-c("outcome","q0%","q10%","q25%","q50%","q75%","q90%","q100%","mean","chance_loss","chance_zero","chance_gain")
  results[,1]<-variables
  for (var in variables) {
    results[which(var==variables),c("q0%","q10%","q25%","q50%","q75%","q90%","q100%")]<-quantile(tab[,var],probs=c(0,0.1,0.25,0.5,0.75,0.9,1))
    results[which(var==variables),"mean"]<-mean(tab[,var])
    results[which(var==variables),"chance_loss"]<-length(which(tab[,var]<0))
    results[which(var==variables),"chance_zero"]<-length(which(tab[,var]==0))
    results[which(var==variables),"chance_gain"]<-length(which(tab[,var]>0))
  }
  return(results)
}



variables<-c("GOK_ORIO_bottomline","Wajir_bottomline","Habaswein_bottomline","Pipeline_communities_bottomline",
             "Water_company_bottomline","downstream_bottomline","upstream_bottomline","TOTAL_bottomline",
             "TOTAL_minus_donor_bottomline","project_success","benefit_scaling_factor")

table_risks<-"D:/DECISIONS/UPGRO/model/results_risks/output_table.csv"
table_coop<-"D:/DECISIONS/UPGRO/model/results_cooperation/output_table.csv"

risks<-summarize_distributions(table_risks,variables)
write.csv(risks,"D:/DECISIONS/UPGRO/model/results_risks/summary_risks.csv",row.names=FALSE)
cooperate<-summarize_distributions(table_coop,variables)
write.csv(cooperate,"D:/DECISIONS/UPGRO/model/results_cooperation/summary_cooperation.csv",row.names=FALSE)
