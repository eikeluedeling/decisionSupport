#' Make Conditional Probability tables using the likelihood method
#' 
#' This function creates Conditional Probability Tables for
#' Bayesian Network nodes from parameters that (for complex nodes) can
#' be more easily elicited from experts than the full table. The function
#' uses the Likelihood method, as described by Sjoekvist S & Hansson F, 2013.
#' Tables are created from three the relative weights of all parents, rankings
#' for all parents, a parameter (b) for the sensitivity of the child node and
#' a prior distribution (for the child node).
#' 
#' @param parent_effects list of vectors describing the effects of all parent node
#' states on the value of the child variable. For example, if parent 1 has
#' four states, the respective vector might look like this: c(3,1,0,0). This
#' would imply that the first state of the parent is strongly associated with
#' high values for the child, the second less strongly, and the 3rd and
#' 4th value are associated with equally low values.
#' @param parent_weights weight factors for the parent nodes
#' @param b parameter for the strength of the parent's influence on
#' the child node. A value of 1 causes no response; 3 is quite strong.
#' @param child_prior prior distribution for the states of the child node.
#' @param ranking_child vector of length length(child_prior) containing rankings for
#' the child node states on a -1..1 scale. If this is null, evenly spaced rankings on
#' this -1..1 scale are assigned automatically.
#' @param child_states optional vector specifying the names of the child states.
#' @param parent_names optional vector specifying parent node names.
#' @param parent_states list of the same structure as parent_effects containing
#' names for all states of all parents.
#' @return list of two data.frames: 1) Conditional Probability Table (CPT); 2) legend table
#' specifying which states of the parent nodes belong to which column in the CPT.
#' @author Eike Luedeling
#' @references Sjoekvist S & Hansson F, 2013. Modelling expert judgement into a Bayesian
#' Belief Network - a method for consistent and robust determination of conditional
#' probability tables. Master's thesis, Faculty of Engineering, Lund University;
#' http://lup.lub.lu.se/luur/download?func=downloadFile&recordOId=3866733&fileOId=3866740
#' @keywords CPT
#' 
#' @examples
#' 
#' make_CPT(parent_effects=list(c(-1,1),c(-0.5,0,0.5)),
#'   parent_weights=c(3,1),b=1.5,child_prior=c(.2,.6,.2),child_states=c("a","b","c"))
#'
#'test_CPT<-make_CPT(parent_effects=list(c(-1,3),c(-4,2),c(-2,3,4),c(1,2,3)),
#'                   parent_weights=c(1,1,1,1),b=2,child_prior=c(1,2,3,4,5),
#'                   child_states=c("a","b","c","d","e"),
#'                   parent_states=list(c("low","high"),c("A","B"),c(1,2,3),c("Hi","Lunch","Bye")))
#'
#' 
#' @export make_CPT
make_CPT<-function(parent_effects,parent_weights,b,child_prior,
                   ranking_child=NULL,child_states=NULL,parent_names=NULL,
                   parent_states=NULL)
{

  child_n_states<-length(child_prior)

  if(is.na(child_prior[1])) child_prior<-rep(1/length(child_prior),length(child_prior))
  
  if(is.null(ranking_child))
    ranking_child<-interpolate_gaps(c(-1,rep(NA,child_n_states-2),1))$interp else
      if(!length(ranking_child)==child_n_states)
        ranking_child<-interpolate_gaps(c(-1,rep(NA,child_n_states-2),1))$interp

for (i in 1:length(parent_effects))
{al<-(parent_effects[[i]]-min(parent_effects[[i]]))
 al<-al/max(al)*2  # (2/al[length(al)])
 parent_effects[[i]]<-(al-1)*parent_weights[i]}

gamma<-list()

for(a in 1:length(parent_effects))
{gamma[[a]]<-matrix(nrow=child_n_states,ncol=length(parent_effects[[a]]))
for(c in 1:child_n_states)
  gamma[[a]][c,]<-ranking_child[c]*parent_effects[[a]]}


vecs <- mapply(seq, 1, rev(sapply(gamma,ncol)))

if(is.matrix(vecs)) vecs<-lapply(seq_len(ncol(vecs)), function(i) vecs[,i])

tmp <- t(do.call(expand.grid, vecs))
row.names(tmp)<-rev(row.names(tmp))
tmp<-tmp[nrow(tmp):1,]

CPT<-matrix(0,nrow=child_n_states,ncol=prod(sapply(parent_effects,length)))

if(length(gamma)==1)
  for(i in 1:length(gamma))  CPT<-CPT+gamma[[i]][,tmp] else
    for(i in 1:length(gamma))  CPT<-CPT+gamma[[i]][,tmp[i,]]
CPT<-b^CPT


CPT<-CPT*child_prior

for(i in 1:ncol(CPT))
  CPT[,i]<-CPT[,i]/sum(CPT[,i])

if(is.null(child_states))
  child_states<-paste("state_",1:child_n_states,sep="")
  
row.names(CPT)<-child_states
colnames(CPT)<-paste("col_",1:ncol(CPT),sep="")

if(is.null(parent_states))
  {parent_states<-parent_effects
   parent_states<-sapply(parent_states,function(x) paste("state_",1:length(x),sep=""))
   if(is.matrix(parent_states))
     parent_states<-lapply(seq_len(ncol(parent_states)), function(i) parent_states[,i])
}

leg_tab<-tmp
if(length(gamma)==1)
  leg_tab<-data.frame(matrix(parent_states[[1]],nrow=1)) else
  {for(i in 1:nrow(leg_tab))
    leg_tab[i,]<-parent_states[[i]][tmp[i,]]}
colnames(leg_tab)<-paste("col_",1:ncol(leg_tab),sep="")
row.names(leg_tab)<-parent_names

if(!is.null(parent_names)) row.names(leg_tab)<-parent_names

return(list(CPT=CPT,column_legend=leg_tab))
}

