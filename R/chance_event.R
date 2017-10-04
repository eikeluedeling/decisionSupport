#' simulate occurrence of random events
#' 
#' In many simulations, certain events can either occur or not, and values for
#' dependent variables can depend on which of the cases occurs. This function
#' randomly simulates whether events occur and returns output values
#' accordingly. The outputs can be single values or series of values, with the
#' option of introducing artificial variation into this dataset.
#' 
#' 
#' @param chance probability that the risky event will occur (between 0 and 1)
#' @param value_if output value in case the event occurs. This can be either a
#' single numeric value or a numeric vector. Defaults to 1.
#' @param value_if_not output value in case the event does not occur. This can
#' be either a single numeric value or a numeric vector. If it is a vector, it
#' must have the same length as value_if
#' @param n number of times the risky event is simulated. This is ignored if
#' length(value_if)>1.
#' @param CV_if coefficient of variation for introducing randomness into the
#' value_if data set. This defaults to 0 for no artificial variation. See
#' documentation for the vv function for details.
#' @param CV_if_not coefficient of variation for introducing randomness into
#' the value_if_not data set. This defaults to the value for CV_if. See
#' documentation for the vv function for details.
#' @param one_draw boolean coefficient indicating if event occurrence is
#' determined only once (TRUE) with results applying to all elements of the
#' results vector, or if event occurrence is determined independently for each
#' element (FALSE; the default)
#' @return numeric vector of the same length as value_if or, if
#' length(value_if)==1 of length n, containing outputs of a probabilistic
#' simulation that assigns value_if if the event occurs, or value_if_not if is
#' does not occur (both optionally with artificial variation)
#' @author Eike Luedeling
#' @importFrom stats rbinom
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' chance_event(0.6,6)
#'  
#' chance_event(.5,c(0,5),c(5,6))
#' 
#' chance_event(chance=0.5,
#'              value_if=1,
#'              value_if_not=5,
#'              n=10,
#'              CV_if=20)
#' 
#' @export chance_event
chance_event <- function(chance,value_if=1,value_if_not=0,n=1,CV_if=0,CV_if_not=CV_if,one_draw=FALSE)
  {
  if(!(length(value_if)==length(value_if_not))) stop("value_if and value_if_not are not of the same length")
  
  if(length(value_if)==1)
    if(n>1) {value_if<-vv(value_if,CV_if,n)
              value_if_not<-vv(value_if_not,CV_if_not,n)}
  if(!one_draw) occurrence<-rbinom(length(value_if),1,chance) else occurrence<-rbinom(1,1,chance)
  return(occurrence*value_if+(1-occurrence)*value_if_not)}

