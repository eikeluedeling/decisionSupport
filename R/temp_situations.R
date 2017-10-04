#' Situation occurrence and resolution
#' 
#' This function simulates a situation, e.g. a conflict, that arises with a certain probability,
#' generates an impact as long as it persists, and has a certain chance of being resolved.
#' 
#' @param n integer; number of values to produce
#' @param p_occurrence chance that a situation (e.g. conflict) occurs (probability btw. 0 and 1)
#' @param p_resolution chance that the situation disappears
#' (e.g. the conflict gets resolved) (probability btw. 0 and 1)
#' @param normal_outcome output value for vector elements that aren't affected by the 
#' situation (can be subject to random variation, if var_CV_normal is specified).
#' Defaults to 1.
#' @param situation_outcome output value for vector elements that are affected by the 
#' situation (can be subject to random variation, if var_CV_situation is specified).
#' Defaults to 0.
#' @param var_CV_normal desired coefficient of variation for 'normal' vector elements (in percent).
#' Defaults to 0.
#' @param var_CV_situation desired coefficient of variation for elements of the vector
#' that are affected by the situation (in percent). Defaults to 0.

#' @return vector of n numeric values, representing a variable time series, which simulates
#' the effects of a situation that arises with a probability p_occurrence and disappears again
#' with a probability p_resolution
#' @author Eike Luedeling
#' @keywords ~kwd1 ~kwd2
#' 
#' @examples
#' 
#' temp_situations(n=30,p_occurrence=0.2,p_resolution=0.5)
#'
#' temp_situations(n=30,p_occurrence=0.2,p_resolution=0.5,
#' normal_outcome=10,situation_outcome=100,var_CV_normal=10,
#' var_CV_situation=40)
#' 
#' @export temp_situations
temp_situations<-
  function(n,p_occurrence,p_resolution,normal_outcome=1,situation_outcome=0,
           var_CV_normal=0,
           var_CV_situation=0)
  {occurrence<-rbinom(n,1,p_occurrence)
  resolved<-rbinom(n,1,p_resolution)
  output<-occurrence
  for(i in 2:length(output))
    if(occurrence[i]==0) if(occurrence[i-1]==1)
      if(!resolved[i]==1) occurrence[i]<-0 else occurrence[i]<-1
  
  output[which(occurrence==0)]<-
    vv(normal_outcome,var_CV_normal,n=length(output[which(occurrence==0)]))
  output[which(occurrence==1)]<-
    vv(situation_outcome,var_CV_situation,n=length(output[which(occurrence==1)]))

  return(output)}
