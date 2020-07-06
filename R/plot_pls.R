#' Visualizing Projection to Latent Structures (PLS) regression outputs for various types of Monte Carlo simulation results
#' 
#' Several plotting options for distribution outputs
#' 
#' @param plsrResults is an object of Projection to Latent Structures (PLS) regression outputs from the \code{\link[decisionSupport:plsr.mcSimulation]{plsr.mcSimulation}} function
#' 
#' @keywords Monte-Carlo decisionSupport decision-analysis net-present-value NPV risk uncertainty
#' 
#'  
#' @references 
#' Do, Hoa, Eike Luedeling, and Cory Whitney. “Decision Analysis of Agroforestry Options Reveals Adoption Risks for Resource-Poor Farmers.” Agronomy for Sustainable Development 40, no. 3 (June 2020): 20. \url{https://doi.org/10.1007/s13593-020-00624-5}
#' 
#' @examples 
#' # Create the estimate object:
#' 
#' variable = c("labor_cost", "investment_cost", "yield", "market_price")
#' distribution = c("posnorm", "posnorm", "posnorm", "posnorm")
#' lower = c(200, 20000, 5000, 10)
#' upper = c(10000, 100000, 20000, 200)
#' 
#' costBenefitEstimate <- as.estimate(variable, distribution, lower, upper)
#' 
#' # Define the model function without name for the return value:
#' 
#' profit1 <- function(x) {
#'   income <- x$yield * x$market_price
#'   costs <- x$labor_cost + x$investment_cost
#'   profit <- income - costs
#'   return(list(Revenues = profit))
#' }
#' 
#' # Perform the Monte Carlo simulation:
#' 
#' predictionProfit1 <- mcSimulation(estimate = costBenefitEstimate,
#'                                   model_function = profit1,
#'                                   numberOfModelRuns = 10000,
#'                                   functionSyntax = "data.frameNames")
#' 
#' 
#' # Run the PLS analysis
#' 
#' pls <- plsr.mcSimulation(object = predictionProfit1,
#' resultName = names(predictionProfit1$y))
#'  
#'  # Plot PLS results 
#'  
#'  plot_pls(pls)
#' 
#' @export plot_pls
#'
plot_pls <- function(plsrResults){
  
  
  # Define the VIP function to avoid referring to chillR::VIP
  
  VIP <- function(object) {
    
    if (object$method != "oscorespls")
      stop(
        "Only implemented for orthogonal scores algorithm.  Refit with 'method = \"oscorespls\"'"
      )
    if (nrow(object$Yloadings) > 1)
      stop("Only implemented for single-response models")
    
    SS <- c(object$Yloadings) ^ 2 * colSums(object$scores ^ 2)
    Wnorm2 <- colSums(object$loading.weights ^ 2)
    SSW <- sweep(object$loading.weights ^ 2, 2, SS / Wnorm2, "*")
    sqrt(nrow(SSW) * apply(SSW, 1, cumsum) / cumsum(SS))
  }
  
  # Extract the VIP scores with VIP() from chillR. Not sure now what is "Comp 1" (component we guess)
  
  vipResult <- VIP(plsrResults)["Comp 1",]
  
  # The same for the pls coefficients. The use of ,, is new to us
  
  coef <- plsrResults$coefficients[, , 1]
  
  # Create a df for further plotting
  
  pls_outputs <- data.frame(Variable = names(vipResult),
                            VIP = vipResult,
                            Coef = coef)
  
  # Remove the ugly rownames
  
  rownames(pls_outputs) <- NULL
  
  # PLS plot
  
  ggplot(dplyr::filter(pls_outputs, VIP > 0.8),
         aes(VIP, reorder(Variable, VIP), fill = Coef > 0)) +
    geom_col() +
    geom_vline(aes(xintercept = 1)) +
    scale_fill_manual(breaks = c(TRUE, FALSE), values = c("cadetblue", "firebrick")) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.01))) +
    scale_y_discrete(expand = expansion(add = 0.5)) +
    labs(x = "Variable Importance in Projection",
         y = NULL) +
    theme_bw() +
    theme(legend.position = "none")
  
  
  }
  
