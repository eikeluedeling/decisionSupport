#' Draw a random state for a categorical variable
#' 
#' This function draws a sample from a user-defined frequency distribution for a categorical
#' variable. 
#' 
#' @param states character vector containing state names.
#' @param probs numeric vector containing probabilities for the states. If these do
#' not add up to 1, they are automatically normalized.
#' @return one of the states, drawn randomly according to the specified probabilities.
#' @author Eike Luedeling
#' @keywords ~kwd1 ~kwd2
#' @importFrom stats runif
#' 
#' @examples
#'
#'random_state(states=c("very low","low","medium","high","very high"),
#'   probs=c(1,1,2,1,1))
#' 
#' @export 
random_state<-function(states,probs)
{
  probabs<-cumsum(probs)
  probabs<-probabs/max(probabs)
  rando<-runif(1)
  state<-states[min(which(probabs>rando))]
  return(state)
}

random_state(states=c("very low","low","medium","high","very high"),
             probs=c(1,1,2,1,1))

