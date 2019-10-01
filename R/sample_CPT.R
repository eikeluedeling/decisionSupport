#' Sample a Conditional Probability Table
#' 
#' This function randomly chooses a state of a categorical variable, based on a Conditional
#' Probability Table (CPT; a component of Bayesian Network models) that expresses the probability
#' of each possible state in relation to the states of other categorical variables. Given
#' information on the state of all parent variables, the function uses the appropriate
#' probability distribution to draw a random sample for the state of the variable of interest.
#' 
#' @param CPT list of two data.frames: 1) Conditional Probability Table (CPT); 2) legend table
#' specifying which states of the parent nodes belong to which column in the CPT. This can
#' be generated with the make_CPT function, or specified manually (which can be cumbersome).
#' @param states character vector containing (in the right sequence) state values for all
#' parent variables.
#' @return one of the states of the child node belonging to the CPT.
#' @author Eike Luedeling
#' @keywords CPT sampling
#' @importFrom stats runif
#' 
#' @examples
#'
#'test_CPT<-make_CPT(parent_effects=list(c(-1,3),c(-4,2),c(-2,3,4),c(1,2,3)),
#'                   parent_weights=c(1,1,1,1),b=2,child_prior=c(1,2,3,4,5),
#'                   child_states=c("a","b","c","d","e"),
#'                   parent_states=list(c("low","high"),c("A","B"),c(1,2,3),
#'                   c("Left","Right","Center")))
#'
#'sample_CPT(CPT=test_CPT,states=c("low","A","2","Left"))
#' 
#' @export
sample_CPT<-function(CPT,states)
{
  probs<-1:ncol(CPT[[2]])
  for(i in 1:length(states))
    probs<-probs[-(which(!CPT[[2]][i,]==states[i]))]
  probs<-cumsum(CPT[[1]][,probs])
  rando<-runif(1)
  if(length(probs)==0)
    {warning("State combination ",states," doesn't exist in CPT.")
    return(NA)}
  state<-names(probs)[min(which(probs>rando))]
  return(state)
}


