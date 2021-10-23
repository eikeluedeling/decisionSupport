#' @include estimate.R
NULL
##############################################################################################
# scenario_mc(base_estimate, scenarios, model_function, ...) 
##############################################################################################
#' Perform a Monte Carlo simulation for predefined scenarios.
#' 
#' This function is a wrapper around the \code{\link[decisionSupport:decisionSupport]{mc_Simulation}} function that facilitates
#' implementation of scenarios. The standard \code{\link[decisionSupport:decisionSupport]{mc_Simulation}} function only allows
#' specifying one set of estimates (i.e. distribution, lower and upper bounds) for each random
#' variable. This is inconvenient when we want to run simulations for heterogeneous populations
#' that include subsets with particular characteristics, e.g. small and large farms. It may
#' then make sense to specify separate distributions for input variables for each of the subsets.
#' The \code{scenario_mc} function facilitates this.
#' 
#' @param base_estimate \code{estimate}: this object corresponds to the \code{estimate} object
#'   in \code{\link[decisionSupport:decisionSupport]{mc_Simulation}}. The distributions that are specified through \code{base_estimate}
#'   are used as default distributions in the simulation, but their parameters can be overridden by
#'   information in the \code{scenarios} data.frame. In brief, this is an estimate of the joint probability distribution of
#'   the input variables. This can be read from a csv file and calculated with the
#'   \code{\link[decisionSupport:decisionSupport]{estimate_read_csv}} function. It can also be generated with the 
#'   \code{\link[decisionSupport:decisionSupport]{as.estimate}} function.
#' @param scenarios \code{data.frame}: Specifies values that should be adjusted in each scenario.
#'   Must contain columns \code{Variable} and \code{param} and one column for each scenario. The
#'   \code{Variable} column can only contain variable names that appear in \code{base_estimate}, as well 
#'   as a \code{Runs} element. The \code{param} column can only contain the strings \code{distribution},
#'   \code{lower} and \code{upper}, except for the row corresponding to \code{Runs} in the \code{Variable}
#'   column (for this the entry doesn't matter). For each scenario column (whose name is the scenario
#'   name), the scenario-specific values must be specified. If the value in the \code{Runs} row is NA,
#'   the \code{numberOfModelRuns} object will be used instead (if that's also NA, you get an error). \code{param}
#'   can also be "both", in which case both lower and upper bounds are set to the respective number, and
#'   the distribution is set to "const".
#' @param model_function \code{function}: The function that transforms the input distribution. It 
#'   has to return a single \code{numeric} value or a \code{list} with named \code{numeric} values.
#' @param  ... Optional arguments of \code{model_function}.
#' @param numberOfModelRuns The number of times running the model function. This doesn't need to be
#'   provided when the the \code{scenarios} data.frame contains a \code{Runs} line that specifies a
#'   particular number of runs for each scenario.
#' @param randomMethod \code{character}: The method to be used to sample the distribution
#'   representing the input estimate. For details see option \code{method} in 
#'   \code{\link{random.estimate}}.
#' @param functionSyntax \code{character}: The syntax which has to be used to implement the model
#'   function. Possible values are \code{"data.frameNames"},
#'   \code{"matrixNames"} or \code{"plainNames"}. Details are given below.
#' @param relativeTolerance \code{numeric}: the relative tolerance level of deviation of the
#'   generated confidence interval from the specified interval. If this deviation is greater than
#'   \code{relativeTolerance} a warning is given.
#' @param verbosity \code{integer}: if \code{0} the function is silent; the larger the value the
#'   more verbose is output information.
#' @details 
#'   See documentation of the \code{\link[decisionSupport:decisionSupport]{mc_Simulation}} function.
#' 
#' @return An object of \code{class mcSimulation}, which is a \code{list} with elements:
#'   \describe{
#'      \item{\code{$x}}{
#'         \code{data.frame} containing the sampled \eqn{x -} (input) values which are generated 
#'         from \code{base_estimate} and possibly modified by \code{scenarios}. To identify the scenario, the scenario name is provided in the
#'         \code{scenario} column.
#'      }
#'      \item{\code{$y}}{
#'        \code{data.frame} containing the simulated \eqn{y -} (output) values, i.e. the model 
#'        function values for \code{x}.The return of the decision model function may include the
#'        assignment of names for the output variables, e.g. like this:
#'          \preformatted{
#'            profit <- function(x){
#'             revenue - costs
#'             return(list(Revenue = revenue,
#'                    Costs = cost))
#'          }
#'        }
#'        
#'      }
#'   } 
#' @examples
#'  
#'  ### define a model_function
#'  
#'  profit<-function(x)
#'  {profit<-benefit_1+benefit_2-cost_1-cost_2
#'    return(Profit=profit)}
#'    
#'  ### define a base_estimate, to be used when no other information is provided
#'  # through the scenario data.frame
#'    
#'  base_estimate<-as.estimate(variable=c("cost_1","cost_2","benefit_1","benefit_2"),
#'                               distribution=c("norm","posnorm","norm","posnorm"),
#'                               lower=c(40,10,50,30),
#'                               upper=c(100,200,300,100))
#'             
#'  ### define a scenario data.frame, which will override values in the base_estimate
#'                                      
#'  scenarios<-data.frame(Variable=c("Runs","cost_1","cost_1","cost_1","cost_2","cost_2",
#'                                   "benefit_1","benefit_1","benefit_2"),
#'                        param=c("x","lower","upper","distribution","lower","upper",
#'                                "lower","upper","lower"),
#'                        Scenario_1=c(100,40,70,"posnorm",30,90,20,35,10),
#'                        Scenario_2=c(50,100,200,"norm",10,40,35,75,5),
#'                        Scenario_3=c(10,400,750,"norm",400,600,30,70,60))
#'                        
#'  ### run a simulation
#'               
#'  results<-scenario_mc(base_estimate, scenarios, model_function=profit,
#'                       functionSyntax="plainNames")
#'                       
#'  ### plot and inspect results
#'  
#'  hist(results)
#'  summary(results)
#'  print(results)
#'  
#'  
#'  
#' @seealso \code{\link{mcSimulation}}, \code{\link{print.mcSimulation}}, \code{\link{summary.mcSimulation}}, \code{\link{hist.mcSimulation}}, \code{\link{estimate}}, \code{\link{random.estimate}}
#' @export
scenario_mc<-function(base_estimate, scenarios, model_function, ..., numberOfModelRuns=NA, randomMethod = "calculate", functionSyntax="data.frameNames",
                      relativeTolerance = 0.05,
                      verbosity = 0)
{
  scenario_names<-colnames(scenarios)[which(!colnames(scenarios)%in% c("Variable","param"))]

  for(scens in scenario_names)
  {
    
    n_Runs<-NA
    if("Runs" %in% scenarios$Variable)
      n_Runs<-as.numeric(scenarios[which(scenarios[,1]=="Runs"),scens])
    if(is.na(n_Runs))
      n_Runs<-numberOfModelRuns
    if(is.na(n_Runs))
      stop(paste("Not clear how many times scenario", scens, "should be run"))
    if(!(n_Runs==round(n_Runs))&(n_Runs>0))
      stop(paste("Number of runs for scenario", scens, "is not a positive integer"))
    
    estim<-base_estimate
    
    if(!("Runs" %in% scenarios$Variable)) parameter_lines<-1:nrow(scenarios) else
      parameter_lines<-which(!(1:nrow(scenarios)==which(scenarios$Variable=="Runs")))
    
    for (i in parameter_lines)
    {
      if(!scenarios$Variable[i] %in% rownames(estim$marginal))
        stop(paste("Estimate object doesn't contain a parameter called",scenarios$Variable[i]))
      if(!is.na(scenarios[i,scens]))
        {if(scenarios$param[i]=="both")
        {estim$marginal[scenarios[i,1],"lower"]<-as.numeric(scenarios[i,scens])
          estim$marginal[scenarios[i,1],"upper"]<-as.numeric(scenarios[i,scens])
          estim$marginal[scenarios[i,1],"distribution"]<-"const"}
        if(scenarios$param[i]=="lower")
          if(!is.na(as.numeric(scenarios[i,scens])))
            estim$marginal[scenarios[i,1],"lower"]<-as.numeric(scenarios[i,scens])
        if(scenarios$param[i]=="upper")
          if(!is.na(as.numeric(scenarios[i,scens])))
            estim$marginal[scenarios[i,1],"upper"]<-as.numeric(scenarios[i,scens])
        if(scenarios$param[i]=="distribution")
          if(!is.na(scenarios[i,scens]))
            estim$marginal[scenarios[i,1],"distribution"]<-scenarios[i,scens]
      }
      }
    
     scen_results<-mcSimulation(
      estimate = estim,
      model_function = model_function,
      ...,
      numberOfModelRuns = n_Runs,
      functionSyntax = functionSyntax)
     
    scen_results$x[,"Scenario"]<-scens
    
    if(scens==scenario_names[1])
      end_results<-scen_results
    if(!scens==scenario_names[1])
      {end_results$x<-rbind(end_results$x,scen_results$x)
      end_results$y<-rbind(end_results$y,scen_results$y)
      end_results$call<-"multi-scenario call"
      }
  
  }
  class(end_results) <- cbind("mcSimulation", class(end_results))
  return(end_results)
}



