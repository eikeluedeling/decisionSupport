#' Visualizing Projection to Latent Structures (PLS) regression outputs for various types of Monte Carlo simulation results
#' 
#' Plotting the Variable Importance in the Projection (VIP) statistic and coefficients of a PLS model of Monte Carlo outputs
#' 
#' @param plsrResults is an object of Projection to Latent Structures (PLS) regression outputs from the \code{\link[decisionSupport:plsr.mcSimulation]{plsr.mcSimulation}} function
#' @param input_table is a data frame with at least two columns named 'variable' and 'label'. The 'variable column should have one entry for the name of each variable contained in any of the plots. In preparing the figure, the function will replace the variable names with the labels. If the label is missing then the plot will show 'NA' in the place of the variable name. Default is NULL and uses the original variable names.
#' @param cut_off_line is the vertical line for the VIP variable selection. The default is 1 on the x-axis, which is a standard cut-off for VIP used for variable selection 
#' @param threshold is the filter for reducing the number of variables shown in the plot. With this set to 0 all variables with a VIP > 0 will be shown (often a very long list). In the default setting the overall plot only shows those variables with a VIP > 0.8, which is a common cut-off for variable selection.
#' @param pos_color is the color to be used for positive coefficient values, default is "cadetblue"
#' @param neg_color is the color to be used for negative coefficient values, default is "firebrick"
#' @param ... accepts arguments to be passed to \code{\link[ggplot2:theme]{ggplot::theme}}
#' 
#' @keywords Monte-Carlo decisionSupport decision-analysis net-present-value NPV risk uncertainty
#' 
#'  
#' @references 
#' Do, Hoa, Eike Luedeling, and Cory Whitney. “Decision Analysis of Agroforestry Options Reveals Adoption Risks for Resource-Poor Farmers.” Agronomy for Sustainable Development 40, no. 3 (June 2020): 20. \url{https://doi.org/10.1007/s13593-020-00624-5}
#' Lanzanova, Denis, Cory Whitney, Keith Shepherd, and Eike Luedeling. “Improving Development Efficiency through Decision Analysis: Reservoir Protection in Burkina Faso.” Environmental Modelling & Software 115 (May 1, 2019): 164–75. \url{https://doi.org/10.1016/j.envsoft.2019.01.016}
#' Luedeling, Eike, and Keith Shepherd. “Decision-Focused Agricultural Research.” Solutions 7, no. 5 (2016): 46–54. \url{https://www.thesolutionsjournal.com/article/decision-focused-agricultural-research/}
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
plot_pls <- function(plsrResults, input_table = NULL, cut_off_line = 1,  
                     threshold = 0.8, pos_color = "cadetblue", neg_color = "firebrick", ...){
  
  
  # Check if plsrResults is class mvr
  
  assertthat::assert_that(class(plsrResults) == "mvr",
                          msg = "plsrResults is not class 'mvr', please provide a valid object. This does not appear to have been generated with the 'plsr.mcSimulation' function.")
  
  
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
  
  # Extract the VIP scores with VIP() from chillR. 
  #User can choose 'ncomp' in the 'plsr.mcSimulation'
  
  
  if (plsrResults$ncomp == 1) 
    
    #For 1 ncomp no need to do any subsetting
    vipResult <- VIP(plsrResults) else 
      
      # For >2 ncomp we subset to "Comp 1" (component one) to exclude others
      vipResult <- VIP(plsrResults)["Comp 1",]
  
  # The same for the pls coefficients (select the only or first component). 
  coef <- plsrResults$coefficients[, , 1]
  
  # Create a df for further plotting
  
  pls_outputs <- data.frame(Variable = names(vipResult),
                            VIP = vipResult,
                            Coefficient = coef)
  
  # Remove the ugly rownames
  
  rownames(pls_outputs) <- NULL
  
  # Add own variable names from 'label' in the input table
  
  # check that the input table is available
  if (!(is.null(input_table)))
    
    # join data frames (use 'by = ' to match columns from both data frames)
    combined_table <- dplyr::left_join(pls_outputs, input_table, by = c( "Variable" = "variable")) else
      combined_table <- pls_outputs
  
  # Filter the data according to the threshold defined by the user
  
  filtered_table <- dplyr::filter(combined_table, VIP > threshold)
  
  # Order the variable or labels according VIP
  
  if ("label" %in% colnames(filtered_table))
    
    ordered_vars <- stats::reorder(filtered_table$label, filtered_table$VIP) else
      
      ordered_vars <- stats::reorder(filtered_table$Variable, filtered_table$VIP)
      
  
  # PLS plot
  
  ggplot2::ggplot(filtered_table,
                  ggplot2::aes(VIP, ordered_vars, fill = Coefficient > 0)) +
    ggplot2::geom_col() +
    ggplot2::geom_vline(ggplot2::aes(xintercept = cut_off_line)) +
    ggplot2::scale_fill_manual(breaks = c(TRUE, FALSE), values = c(pos_color, neg_color), 
                               labels = c("Positive", "Negative")) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.01))) +
    ggplot2::scale_y_discrete(expand = ggplot2::expansion(add = 0.5)) +
    ggplot2::labs(x = "Variable Importance in Projection",
                  y = NULL, fill = "Coefficient") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::theme(...)
  
  
  }
  
