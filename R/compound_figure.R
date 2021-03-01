#' Compound figure for decision support
#' 
#' Simple compound figure of model results and analyses of a binary decision 
#' (do or do not do). The figure includes the distribution of the expected outcome, 
#' the expected cashflow, as well as the variable importance and the value of information
#' 
#' @author Eduardo Fernandez (\email{efernand@@uni-bonn.de})
#' @author Cory Whitney (\email{cory.whitney@@uni-bonn.de})
#' 
#' @param model is a user defined model function see the \code{\link[decisionSupport:mcSimulation]{mcSimulation}} function. Either 'model' or 'mcSimulation_object' must be provided. If both are given then 'model' has precedence and the model will be calculated for the figure
#' @param input_table is a data frame with at least two columns named 'variable' and 'label'. The 'variable column should have one entry for the name of each variable contained in any of the plots. In preparing the figure, the function will replace the variable names with the labels. If the label is missing then the plot will show 'NA' in the place of the variable name. Default is NULL and uses the original variable names.
#' @param decision_var_name is the name of the decision outcome named in the \code{return()} of 'model'. This will be used in all plots and analyses except for the cashflow plot. For now this is just one decision option
#' @param cashflow_var_name is the name of the cashflow variable named in the \code{return()} of 'model'. This will be used in the cashflow plot
#' @param model_runs is the number of time that the model should run. The default is 10,000
#' @param distribution_method is the method used in the distribution plot see the \code{\link[decisionSupport:plot_distributions]{plot_distributions}} function
#' @param mcSimulation_object is an object of Monte Carlo simulation outputs from the \code{\link[decisionSupport:mcSimulation]{mcSimulation}} function
#' @param plsrResults is an object of Projection to Latent Structures (PLS) regression outputs from the \code{\link[decisionSupport:plsr.mcSimulation]{plsr.mcSimulation}} function
#' @param EVPIresults are the results of the \code{\link[decisionSupport:multi_EVPI]{multi_EVPI}} function
#' @param x_axis_name_distribution,y_axis_name_distribution,x_axis_name_cashflow,y_axis_name_cashflow,x_axis_name_pls,y_axis_name_pls,x_axis_name_evpi,y_axis_name_evpi are the names (character strings) to pass to the axis titles for the respective plots (distribution, cashflow, pls, evpi)
#' @param legend_name_cashflow,legend_name_pls are the names (character strings) representing the title of the legend
#' @param legend_labels_cashflow,legend_labels_pls are the names (character strings) representing the labels of the legend 
#' @param base_size is the base text size to be used for the plot. The default is 11, this is the \code{\link[ggplot2:ggtheme]{ggplot2::ggtheme}} default
#' 
#' @return This function returns a plot of classes \code{'patchwork'}, \code{'gg'}, 
#' and \code{'ggplot'}. This allows the user to
#' continue editing some features of the plots through the syntax (i.e. \code{'&'},
#' and \code{'+'}) from both libraries.
#'
#' @keywords Monte-Carlo decisionSupport decision-analysis net-present-value NPV risk uncertainty
#'
#' @references 
#' Do, Hoa, Eike Luedeling, and Cory Whitney. “Decision Analysis of Agroforestry Options Reveals Adoption Risks for Resource-Poor Farmers.” Agronomy for Sustainable Development 40, no. 3 (June 2020): 20. https://doi.org/10.1007/s13593-020-00624-5.
#' Lanzanova, Denis, Cory Whitney, Keith Shepherd, and Eike Luedeling. “Improving Development Efficiency through Decision Analysis: Reservoir Protection in Burkina Faso.” Environmental Modelling & Software 115 (May 1, 2019): 164–75. https://doi.org/10.1016/j.envsoft.2019.01.016.
#' Ruett, Marius, Cory Whitney, and Eike Luedeling. “Model-Based Evaluation of Management Options in Ornamental Plant Nurseries.” Journal of Cleaner Production 271 (June 2020): 122653. https://doi.org/10.1016/j.jclepro.2020.122653.
#' 
#' @examples 
#' ##############################################################
#' # Example 1 (Creating the estimate from the command line):
#' #############################################################
#' # Create the estimate object:
#' 
#' cost_benefit_table <- data.frame(label = c("Revenue", "Costs"),
#'                                   variable = c("revenue", "costs"),
#'                                   distribution = c("norm", "norm"),
#'                                   lower = c(100,  500),
#'                                   median = c(NA, NA),
#'                                   upper = c(10000, 5000))
#' 
#' # (a) Define the model function without name for the return value:
#' 
#' profit1 <- function() {
#'   Decision <- revenue - costs
#'   cashflow <- rnorm(rep(revenue, 20))
#'   return(list(Revenues = revenue,
#'               Costs = costs, 
#'               cashflow = cashflow, 
#'               Decision = Decision))
#' }
#' 
#' compound_figure(model = profit1, 
#' input_table = cost_benefit_table, 
#' decision_var_name = "Decision",
#' cashflow_var_name = "cashflow",
#' model_runs = 1e2, 
#' distribution_method = 'smooth_simple_overlay')
#' 
#' @import patchwork
#' @export compound_figure
#' 
compound_figure <- function(model = NULL, 
                            input_table, 
                            decision_var_name,
                            cashflow_var_name,
                            model_runs = 1e4, 
                            distribution_method = 'smooth_simple_overlay',
                            mcSimulation_object = NULL, 
                            plsrResults = NULL, 
                            EVPIresults = NULL, 
                            
                            x_axis_name_distribution = "Outcome distribution",
                            y_axis_name_distribution = NULL,
                            
                            x_axis_name_cashflow = "Timeline of intervention", 
                            y_axis_name_cashflow = "Cashflow", 
                            legend_name_cashflow = "Quantiles (%)",
                            legend_labels_cashflow = c("5 to 95", "25 to 75", "median"),
                            
                            x_axis_name_pls = "Variable Importance in Projection",
                            y_axis_name_pls = NULL, 
                            legend_name_pls = "Coefficient",
                            legend_labels_pls = c("Positive", "Negative"),
                            
                            x_axis_name_evpi = "Expected Value of Perfect Information",
                            y_axis_name_evpi = NULL,
                            
                            base_size = 11) {

  if(is.null(mcSimulation_object) & is.null(model))
    stop("Please provide either 'model' or the mcSimulation_object", call. = FALSE)
    
  if(!is.null(mcSimulation_object) & 
     class(mcSimulation_object)[[1]] == "mcSimulation") {
      monte_carlo <- mcSimulation_object} else {
  
      monte_carlo <- decisionSupport::mcSimulation(
      estimate =  decisionSupport::as.estimate(input_table),
      model_function = model,
      numberOfModelRuns = model_runs,
      functionSyntax = "plainNames") }

#### Plot Net Present Value (NPV) distributions 
distribution_plot <- decisionSupport::plot_distributions(mcSimulation_object = monte_carlo, 
                                    vars = decision_var_name,
                                    method = distribution_method, 
                                    legend.position = "none", 
                                    x_axis_name = x_axis_name_distribution,
                                    y_axis_name = y_axis_name_distribution,
                                    base_size = base_size)

#### Cashflow analysis
cashflow_plot <- plot_cashflow(mcSimulation_object = monte_carlo, 
                               cashflow_var_name = cashflow_var_name,
                               x_axis_name = x_axis_name_cashflow,
                               y_axis_name = y_axis_name_cashflow,
                               legend_name = legend_name_cashflow,
                               legend_labels = legend_labels_cashflow,
                               facet_labels = "", base_size = base_size)

#### Projection to Latent Structures (PLS) analysis

if(!is.null(plsrResults) & 
   class(plsrResults) == "mvr") {
    pls_result <- plsrResults} else {
      
      pls_result <- plsr.mcSimulation(object = monte_carlo,
                                      resultName = decision_var_name, ncomp = 1) }

# Run the `plot_pls()` on the results from `plsr.mcSimulation()` 
pls_plot <- plot_pls(pls_result, input_table = input_table, threshold = 0.8,
                     x_axis_name = x_axis_name_pls,
                     y_axis_name = y_axis_name_pls,
                     legend_name = legend_name_pls,
                     legend_labels = legend_labels_pls,
                     base_size = base_size)

#### Value of Information (VoI) analysis
# subset the outputs from the mcSimulation function (y) by selecting the correct variables
mcSimulation_table <- data.frame(monte_carlo$x, monte_carlo$y[decision_var_name])

# Run the multi_EVPI() or supply result
if(!is.null(EVPIresults) & 
   "EVPI_outputs" %in% class(EVPIresults)) {
    evpi <- EVPIresults} else {
      
      evpi <- multi_EVPI(mc = mcSimulation_table, first_out_var = decision_var_name) }

# run the `plot_evpi()` on the results from `multi_EVPI()` with standard settings. 
# The length of the bars is equal to EVPI.
evpi_plot <- plot_evpi(evpi, decision_vars = decision_var_name, input_table = input_table, 
                       new_names = "", 
                       x_axis_name = x_axis_name_evpi,
                       y_axis_name = y_axis_name_evpi,
                       base_size = base_size)

# use the patchwork library to create the compound figure
if(is.null(evpi_plot))
   figure <- ((distribution_plot + cashflow_plot) / (pls_plot + patchwork::plot_spacer()))
  
else
  
   figure <- ((distribution_plot + cashflow_plot) / (pls_plot + evpi_plot)) 

return(figure)
}
