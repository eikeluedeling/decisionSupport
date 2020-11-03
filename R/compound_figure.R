#' Compound figure for decision support
#' 
#' Simple compound figure of model results and analyses of a binary decision (do or do not do). The figure includes the distribution of the expected outcome, the expected cashflow, as well as the variable importance and the value of information
#' 
#' @param model is a user defined model function see the \code{\link[decisionSupport:mcSimulation]{mcSimulation}} function
#' @param input_table is a data frame with at least two columns named 'variable' and 'label'. The 'variable column should have one entry for the name of each variable contained in any of the plots. In preparing the figure, the function will replace the variable names with the labels. If the label is missing then the plot will show 'NA' in the place of the variable name. Default is NULL and uses the original variable names.
#' @param decision_var_name is the name of the decision outcome named in the \code{return()} of 'model'. This will be used in all plots and analyses except for the cashflow plot. For now this is just one decision option
#' @param cashflow_var_name is the name of the cashflow variable named in the \code{return()} of 'model'. This will be used in the cashflow plot
#' @param model_runs is the number of time that the model should run. The default is 10,000
#' @param distribution_method is the method used in the distribution plot see the \code{\link[decisionSupport:plot_distribution]{plot_distribution}} function
#' 
#' 
#' 
#' 
compound_figure <- function(model, 
                            input_table, 
                            decision_var_name,
                            cashflow_var_name,
                            model_runs = 1e4, 
                            distribution_method = 'smooth_simple_overlay') {
  
  monte_carlo <- decisionSupport::mcSimulation(
    estimate =  decisionSupport::as.estimate(input_table),
    model_function = model,
    numberOfModelRuns = model_runs,
    functionSyntax = "plainNames")

#### Plot Net Present Value (NPV) distributions 
distribution_plot <- decisionSupport::plot_distributions(mcSimulation_object = monte_carlo, 
                                    vars = decision_var_name,
                                    method = distribution_method, 
                                    legend.position = "none")

#### Cashflow analysis
cashflow_plot <- plot_cashflow(mcSimulation_object = monte_carlo, 
                               cashflow_var_name = cashflow_var_name)

#### Projection to Latent Structures (PLS) analysis
pls_result <- plsr.mcSimulation(object = monte_carlo,
                                resultName = decision_var_name, ncomp = 1)

# Run the `plot_pls()` on the results from `plsr.mcSimulation()` 
pls_plot <- plot_pls(pls_result, input_table = input_table, threshold = 0.8)

#### Value of Information (VoI) analysis
# subset the outputs from the mcSimulation function (y) by selecting the correct variables
mcSimulation_table <- data.frame(monte_carlo$x, monte_carlo$y[decision_var_name])
evpi <- multi_EVPI(mc = mcSimulation_table, first_out_var = decision_var_name)

# run the `plot_evpi()` on the results from `multi_EVPI()` with standard settings. The length of the bars is equal to EVPI.
evpi_plot <- plot_evpi(evpi, decision_vars = decision_var_name, input_table = input_table)

# use the patchwork library to create the compound figure
((distribution_plot + cashflow_plot) / (pls_plot + evpi_plot)) #& 
  #ggplot2::theme(strip.text.x = ggplot2::element_blank())
  #future iterations of this function may remove the axis title
}
