#' @include estimate.R
NULL
##############################################################################################
# mcSimulation(estimate, model_function, numberOfModelRuns, ...) 
##############################################################################################
#' Perform a Monte Carlo simulation.
#' 
#' This function generates a random sample of an output distribution defined as the transformation 
#' of an input distribution by a mathematical model, i.e. a mathematical function. This is called a
#' Monte Carlo simulation. For details cf. below.
#' @param estimate \code{estimate}: estimate of the joint probability distribution of
#'   the input variables. This can be read from a csv file and calculated with the \code{\link[decisionSupport]{estimate_read_csv}} function.
#' @param model_function \code{function}: The function that transforms the input distribution. It 
#'   has to return a single \code{numeric} value or a \code{list} with named \code{numeric} values.
#' @param  ... Optional arguments of \code{model_function}.
#' @param numberOfModelRuns The number of times running the model function.
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
# @param ... Optional arguments to be passed to \code{\link{random}.
#' @details 
#' This method solves the following problem. Given a multivariate random variable \eqn{x =
#' (x_1,\ldots,x_k)} with joint probability distribution \eqn{P}, i.e. 
#'   \deqn{x \sim P.}{x ~ P} 
#' Then the continuous function 
#'   \deqn{f:R^k \rightarrow R^l, y = f(x)}{f:R^k --> R^l, y = f(x)} 
#' defines another random variable with distribution 
#'   \deqn{y \sim f(P).}{y ~ f(P).}
#' Given a probability density \eqn{\rho} of x that defines \eqn{P} the problem is the determination 
#' of the probability density \eqn{\phi} that defines \eqn{f(P)}. This method samples the 
#' probability density \eqn{\phi} of \eqn{y} as follows: The input distribution \eqn{P} is provided 
#' as \code{estimate}. From \code{estimate} a sample \code{x} with \code{numberOfModelRuns} is  
#' generated using \code{\link{random.estimate}}. Then the function values \eqn{y=f(x)} are 
#' calculated, where \eqn{f} is \code{model_function}.
#' 
#' \code{functionSyntax} defines the syntax of \code{model_function}, which has to be used, as 
#' follows:
#' \describe{
#   \item{\code{"plainNamesDeprecated"}}{
#     This option requires the package \pkg{\code{\link[plyr]{plyr}}}.
#     \code{model_function} is constructed, e.g. like this:
#        \preformatted{
#          profit<-function(x){
#            # Assign the variable names to the function environement:
#            for(i in names(x)) assign(i, as.numeric(x[i]))
#              
#            revenue-costs
#          }
#        }
#        \dfn{Note}: this is the slowest of the possibilities for \code{functionSyntax}.
#   }
#'   \item{\code{"data.frameNames"}}{
#'      The model function is constructed, e.g. like this:
#'        \preformatted{
#'          profit<-function(x){
#'            x[["revenue"]]-x[["costs"]]
#'          }
#'        }
#'        or like this:
#'        \preformatted{
#'          profit<-function(x){
#'            x$revenue-x$costs
#'          }
#'        }
#'      }
#'      \item{\code{"matrixNames"}}{
#'         The model function is constructed, e.g. like this:
#'         \preformatted{
#'            profit<-function(x){
#'              x[,"revenue"]-x[,"costs"]
#'            }
#'         }     
#'      }
#'   \item{\code{"plainNames"}}{
#     This option requires the package \pkg{\code{\link[plyr]{plyr}}}.
#'     \code{model_function} is constructed, e.g. like this:
#'        \preformatted{
#'          profit<-function(x){
#'            revenue-costs
#'          }
#'        }
#'        \dfn{Note}: this is the slowest of the possibilities for \code{functionSyntax}.
#'    }     
#'  }
#' @return An object of \code{class mcSimulation}, which is a \code{list} with elements:
#'   \describe{
#'      \item{\code{$x}}{
#'         \code{data.frame} containing the sampled \eqn{x -} (input) values which are generated 
#'         from \code{estimate}.
#'      }
#'      \item{\code{$y}}{
#'        \code{data.frame} containing the simulated \eqn{y -} (output) values, i.e. the model 
#'        function values for \code{x}.
#'      }
#'   } 
#' @examples
#'  #############################################################
#'  # Example 1 (Creating the estimate from the command line):
#'  #############################################################
#'  # Create the estimate object:
#'  variable=c("revenue","costs")
#'  distribution=c("norm","norm")
#'  lower=c(10000,  5000)
#'  upper=c(100000, 50000)
#'  costBenefitEstimate<-as.estimate(variable, distribution, lower, upper)
#'  # (a) Define the model function without name for the return value:
#'  profit1<-function(x){
#'    x$revenue-x$costs
#'  }
#'  # Perform the Monte Carlo simulation:
#'  predictionProfit1<-mcSimulation( estimate=costBenefitEstimate, 
#'                                  model_function=profit1, 
#'                                  numberOfModelRuns=10000,
#'                                  functionSyntax="data.frameNames")
#'  # Show the simulation results:
#'  print(summary(predictionProfit1))
#'  hist(predictionProfit1,xlab="Profit")
#'  #############################################################
#'  # (b) Define the model function with a name for the return value:
#'  profit1<-function(x){
#'    list(Profit=x$revenue-x$costs)
#'  } 
#'  # Perform the Monte Carlo simulation:
#'  predictionProfit1<-mcSimulation( estimate=costBenefitEstimate, 
#'                                  model_function=profit1, 
#'                                  numberOfModelRuns=10000,
#'                                  functionSyntax="data.frameNames")
#'  # Show the simulation results:
#'  print(summary(predictionProfit1, classicView=TRUE))
#'  hist(predictionProfit1) 
#'  #########################################################
#  # (c) Using plain names (deprecated) in the model function syntax
#  profit1<-function(x){
#   # Assign the variable names to the function environement:
#   for(i in names(x)) assign(i, as.numeric(x[i]))
#              
#    list(Profit=revenue-costs)
#  } 
#  # Perform the Monte Carlo simulation:
#  predictionProfit1<-mcSimulation( estimate=costBenefitEstimate, 
#                                  model_function=profit1, 
#                                  numberOfModelRuns=1000,
#                                  functionSyntax="plainNamesDeprecated")
#  # Show the simulation results:
#  print(summary(predictionProfit1, probs=c(0.05,0.50,0.95)))
#  hist(predictionProfit1) 
#  #########################################################
#  # (d) Using plain names (deprecated) in the model function syntax and
#  #     define the model function without name for the return value:
#  profit1<-function(x){
#   # Assign the variable names to the function environement:
#   for(i in names(x)) assign(i, as.numeric(x[i])) 
#    
#    revenue-costs
#  }
#  # Perform the Monte Carlo simulation:
#  predictionProfit1<-mcSimulation( estimate=costBenefitEstimate,
#                                   model_function=profit1,
#                                   numberOfModelRuns=1000,
#                                   functionSyntax="plainNamesDeprecated")
#  # Show the simulation results:
#  print(summary(predictionProfit1, probs=c(0.05,0.50,0.95)))
#  hist(predictionProfit1, xlab="Profit")
#  #########################################################
#'  # (c) Using plain names in the model function syntax
#'  profit1<-function(){
#'    list(Profit=revenue-costs)
#'  } 
#'  # Perform the Monte Carlo simulation:
#'  predictionProfit1<-mcSimulation( estimate=costBenefitEstimate, 
#'                                  model_function=profit1, 
#'                                  numberOfModelRuns=1000,
#'                                  functionSyntax="plainNames")
#'  # Show the simulation results:
#'  print(summary(predictionProfit1, probs=c(0.05,0.50,0.95)))
#'  hist(predictionProfit1) 
#'  #########################################################
#'  # (d) Using plain names in the model function syntax and
#'  #     define the model function without name for the return value:
#'  profit1<-function() revenue-costs
#'  # Perform the Monte Carlo simulation:
#'  predictionProfit1<-mcSimulation( estimate=costBenefitEstimate,
#'                                   model_function=profit1,
#'                                   numberOfModelRuns=1000,
#'                                   functionSyntax="plainNames")
#'  # Show the simulation results:
#'  print(summary(predictionProfit1, probs=c(0.05,0.50,0.95)))
#'  hist(predictionProfit1, xlab="Profit")
#'  #############################################################
#'  # Example 2(Reading the estimate from file):
#'  #############################################################
#'  # Define the model function:
#'  profit2<-function(x){
#'    Profit<-x[["sales"]]*(x[["productprice"]] - x[["costprice"]])
#'    list(Profit=Profit)
#'  }  
#'  # Read the estimate of sales, productprice and costprice from file:
#'  inputFileName=system.file("extdata","profit-4.csv",package="decisionSupport")
#'  parameterEstimate<-estimate_read_csv(fileName=inputFileName)
#'  print(parameterEstimate)
#'  # Perform the Monte Carlo simulation:
#'  predictionProfit2<-mcSimulation( estimate=parameterEstimate, 
#'                                  model_function=profit2, 
#'                                  numberOfModelRuns=10000,
#'                                  functionSyntax="data.frameNames")
#'  # Show the simulation results:
#'  print(summary(predictionProfit2))
#'  hist(predictionProfit2) 
#' @seealso \code{\link{print.mcSimulation}}, \code{\link{summary.mcSimulation}}, \code{\link{hist.mcSimulation}}, \code{\link{estimate}}, \code{\link{random.estimate}}
#' @export
mcSimulation <- function(estimate, model_function, ..., numberOfModelRuns, 
                         randomMethod="calculate", 
                         functionSyntax="data.frameNames",
                         relativeTolerance=0.05,
                         verbosity=0){
  #ToDo: (i) review code and (ii) test
  x<-random(rho=estimate, n=numberOfModelRuns, 
            method=randomMethod,
            relativeTolerance=relativeTolerance)
  if (functionSyntax=="data.frameNames"){
    y<-model_function(as.data.frame(x), ...)
  } else if (functionSyntax=="matrixNames"){
    y<-model_function(as.matrix(x),...)
  } else if (functionSyntax=="plainNamesDeprecated"){
    warning("functionSyntax=\"plainNamesDeprecated\" is deprecated. Please use 
             functionSyntax=\"plainNames\" instead.")
    y<-do.call(what=rbind,
               args=lapply(X=apply(X=x,
                                   MARGIN=1,
                                   FUN=model_function#,
    #                               varnames=row.names(estimate)
                                   ),
                           FUN=unlist))
    
    #     if( !requireNamespace("plyr", quietly = TRUE)) 
    #       stop("Package \"plyr\" needed. Please install it.",
    #            call. = FALSE)
    #     if(verbosity > 0)
    #       .progress="text"
    #     else
    #       .progress="none"
    #     y<-plyr::aaply(.data=x,
    #                    .margins=1,
    #                    .fun=function(x) unlist(model_function(x=x,varnames=row.names(estimate))),
    #                    .drop=FALSE,
    #                    .progress=.progress)
    
    
    #     ## Case: 1d model function without name needs to be treated separately, s.t. output syntax is 
    #     ## consistent with other "functionSyntax":
    #     if(colnames(y)[[1]]=="1" && dim(y)[[2]]==1) 
    #       (y<-as.vector(y))
    # Construct names for the output components if not supplied:
    if(any(colnames(y)==as.character(1:ncol(y))))
      colnames(y)<-paste("output_",c(1:ncol(y)),sep="")
  } else if (functionSyntax=="plainNames"){
    # Auxiliary model function:
    #  CRAN does not allow the use of assign:
    #     model_function_ <- function (x) {
    #       sapply(X=row.names(estimate), 
    #              FUN=function(i) assign(i, as.numeric(x[i]), pos=1)
    #       )
    #       model_function()
    #     }
    model_function_ <- function (x) {
      # Construct a named list where each variable name indicates its value:
      e<-as.list(sapply(X=row.names(estimate), 
                        FUN=function(i) as.numeric(x[i])
      ))
      # Execute the user defined model function in an environment defined by the above constructed list:
      eval(expr=body(model_function), 
           envir=e)
    }
    # Run the actual Monte Carlo simulation:
    y<-do.call(what=rbind,
               args=lapply(X=apply(X=x,
                                   MARGIN=1,
                                   FUN=model_function_),
                           FUN=unlist))
    # Construct names for the output components if not supplied:
    if(any(colnames(y)==as.character(1:ncol(y))))
      colnames(y)<-paste("output_",c(1:ncol(y)),sep="")
  } else 
    stop("functionSyntax=",functionSyntax, "is not defined!") 
  if( is.null(names(y)) && is.null(colnames(y)) ) {
    if(is.null(ncol(y))){
      y<-data.frame(y)
      colnames(y)<-paste("output_",c(1:ncol(y)),sep="")
    } else{
      colnames(y)<-paste("output_",c(1:ncol(y)),sep="")
    }
  }
  # Return object:
  returnObject<-list(y=data.frame(y), x=data.frame(x))
  returnObject$call<-match.call()
  class(returnObject)<-cbind("mcSimulation", class(returnObject))
  
  return(returnObject)
}

