#' Make Conditional Probability tables using the likelihood method
#' 
#' This function creates Conditional Probability Tables for
#' Bayesian Network nodes from parameters that (for complex nodes) can
#' be more easily elicited from experts than the full table. The function
#' uses the Likelihood method. The function combines the make_CPT and sample_CPT
#' functions, but only offers limited flexibility. Refer to the make_CPR and
#' sample_CPT descriptions for details.
#' 
#' @param parent_list named list of parameters for the parent nodes containing a
#' name and a vector of two elements: c(number_of_states,parent_weight).
#' @param child_states_n number of states for the child node.
#' @param child_prior prior distribution for the states of the child node.
#' @param b parameter for the strength of the parent's influence on
#' the child node. A value of 1 causes no response; 3 is quite strong. Defaults to 2.
#' @param obs_states optional vector of observed states for all parents. This has to
#' be complete and names have to correspond exactly with the names of states of the parent
#' nodes. It's also important that the name are given in the exact same sequence as the
#' parents are listed in parent_list.
#' @return list of two data.frames: 1) Conditional Probability Table (CPT); 2) legend table
#' specifying which states of the parent nodes belong to which column in the CPT. If obs_states
#' are given, an additional attribute $sampled specified one random draw, according to the
#' CPT and the obs_states provided.
#' @importFrom chillR interpolate_gaps
#' @author Eike Luedeling
#' @keywords ~kwd1 ~kwd2
#' 
#' @examples
#' 
#' parent_list<-list(pare1=c(5,3),parent2=c(3,2),PARE3=c(4,5))
#' sample_simple_CPT(parent_list,5)
#' sample_simple_CPT(parent_list,5,obs_states=c("very high","medium","high"))
#' 
#' sample_simple_CPT(parent_list=list(management_intensity=c(5,2),inputs=c(5,1)),5,
#'      obs_states=c("medium","very high"))$sampled
#' 
#' @export
sample_simple_CPT<-function(parent_list,
                   child_states_n,
                   child_prior=NULL,
                   b=2,
                   obs_states=NULL)
{

  parent_names<-names(parent_list)
  parent_n_states<-sapply(parent_list,function(x) x[1])
  parent_weights<-sapply(parent_list,function(x) x[2])
  
  if(is.null(child_prior)) child_prior<-rep(1/length(child_states_n),length(child_states_n))
  
  ranking_child<-interpolate_gaps(c(-1,rep(NA,child_states_n-2),1))$interp

gamma<-list()

for(a in 1:length(parent_names))
{gamma[[a]]<-matrix(nrow=child_states_n,ncol=parent_n_states[a])
for(c in 1:child_states_n)
  gamma[[a]][c,]<-ranking_child[c]*interpolate_gaps(c(-1,rep(NA,parent_n_states[a]-2),1))$interp}


vecs <- mapply(seq, 1, rev(sapply(gamma,ncol)))

if(is.matrix(vecs)) vecs<-lapply(seq_len(ncol(vecs)), function(i) vecs[,i])

tmp <- t(do.call(expand.grid, vecs))
row.names(tmp)<-rev(row.names(tmp))
tmp<-tmp[nrow(tmp):1,]

CPT<-matrix(0,nrow=child_states_n,ncol=prod(parent_n_states))


if(length(gamma)==1)
  for(i in 1:length(gamma))  CPT<-CPT+gamma[[i]][,tmp[i]] else
   for(i in 1:length(gamma))  CPT<-CPT+gamma[[i]][,tmp[i,]]
CPT<-b^CPT


CPT<-CPT*child_prior

for(i in 1:ncol(CPT))
  CPT[,i]<-CPT[,i]/sum(CPT[,i])

assign_states<-function(x)
{if(x==1) out<-"normal"
 if(x==2) out<-c("low","high")
 if(x==3) out<-c("low","medium","high")
 if(x==4) out<-c("low","medium low","medium high","high")
 if(x==5) out<-c("very low","low","medium","high","very high")
 if(x==6) out<-c("very low", "low","medium low","medium high","high","very high")
 if(x==7) out<-c("very low", "low", "medium low","medium","medium high","high","very high")
 if(x>7) out<-paste("state",1:x)
 return(out)
}

row.names(CPT)<-assign_states(child_states_n)
colnames(CPT)<-paste("col_",1:ncol(CPT),sep="")

parent_states<-sapply(parent_n_states,assign_states)

if(is.matrix(parent_states))
  parent_states<-lapply(seq_len(ncol(parent_states)), function(i) parent_states[,i])


leg_tab<-tmp

if(length(gamma)==1)
  leg_tab<-data.frame(matrix(parent_states[[1]],nrow=1)) else
    {for(i in 1:nrow(leg_tab))
        leg_tab[i,]<-parent_states[[i]][tmp[i,]]}
     colnames(leg_tab)<-paste("col_",1:ncol(leg_tab),sep="")
     row.names(leg_tab)<-parent_names



if(!is.null(obs_states))
  {sampled<-sample_CPT(list(CPT,leg_tab),obs_states)
   return(list(CPT=CPT,column_legend=leg_tab,sampled=sampled))} else
    return(list(CPT=CPT,column_legend=leg_tab))
}


