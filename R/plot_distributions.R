#' Probability distribution plots for various types of Monte Carlo simulation results
#' 
#' Several plotting options for distribution outputs
#' 
#' @author Eduardo Fernandez (\email{efernand@@uni-bonn.de})
#' @author Cory Whitney (\email{cory.whitney@@uni-bonn.de})
#' 
#' @param mcSimulation_object is an object of Monte Carlo simulation outputs from the \code{\link[decisionSupport:mcSimulation]{mcSimulation}} function
#' @param vars is a vector containing variable names from the \code{mcSimulation_object}. This can also be a single variable name
#' @param method is the plot option to be used in \code{link{ggplot2}}: "smooth_simple_overlay" creates a density plot with \code{\link[ggplot2:geom_density]{geom_density}}, "hist_simple_overlay" creates a histogram with \code{\link[ggplot2:geom_histogram]{geom_histogram}}, "boxplot" creates a boxplot with \code{\link[ggplot2:geom_boxplot]{geom_boxplot}} and "boxplot_density" creates a density plot with a boxplot using \code{\link[ggplot2:geom_density]{geom_density}} and \code{\link[ggplot2:geom_boxplot]{geom_boxplot}}
#' @param bins are the number of bins to use for the \code{\link[ggplot2:geom_histogram]{geom_histogram}}. Default number of bins is 150
#' @param binwidth is the width of the bins to use for the \code{\link[ggplot2:geom_histogram]{geom_histogram}}. Default number is 1000. When both \code{bins} and \code{binwidth} are defined, the later overrides \code{bins}
#' @param old_names are the variable names from the MC simulation outputs that refer to the distribution values. This should be a vector of character strings. This is set to NULL with the assumption that the existing names for variables are preferred 
#' @param new_names are the variable names to replace the MC simulation outputs that refer to the distribution values. This should be a vector of character strings. This is set to NULL with the assumption that the existing names for variables are preferred
#' @param colors is the color palette to be used for the fill of distribution shapes and boxplots. The default is c("#009999", "#0000FF", "#56B4E9", "#009E73","#F0E442", "#0072B2", "#D55E00", "#CC79A7") assuming a maximum of eight variables to be compared
#' @param outlier_shape is the optional shape to replace the outliers in the boxplot. To show no outliers use NA. See \code{\link[ggplot2:aes_linetype_size_shape]{shape}} for shape options
#' @param x_axis_name is the name (character string) to be passed to the x-axis title. Default is "Outcome distribution" and allows allow the user to add a customized axis title
#' @param y_axis_name is the name (character string) to be passed to the y-axis title. Default is NULL to allow the user to add a customized axis title. If a name is not provided the title will be "Number of points in bin" for the \code{hist_simple_overlay} method and "Density estimate" for all other plot options
#' @param base_size is the base text size to be used for the plot. The default is 11, this is the \code{\link[ggplot2:ggtheme]{ggplot2::ggtheme}} default
#' @param ... accepts arguments to be passed to \code{\link[ggplot2:ggtheme]{ggplot2::ggtheme}}
#'
#' @return This function returns a plot of classes \code{'gg'}, 
#' and \code{'ggplot'}. This allows the user to
#' continue editing some features of the plots through the syntax 
#' \code{'+'}.
#' 
#' @keywords Monte-Carlo decisionSupport decision-analysis net-present-value NPV risk uncertainty
#' 
#' @references 
#' Do, Hoa, Eike Luedeling, and Cory Whitney. “Decision Analysis of Agroforestry Options Reveals Adoption Risks for Resource-Poor Farmers.” Agronomy for Sustainable Development 40, no. 3 (June 2020): 20. \doi{10.1007/s13593-020-00624-5}.
#' Lanzanova, Denis, Cory Whitney, Keith Shepherd, and Eike Luedeling. “Improving Development Efficiency through Decision Analysis: Reservoir Protection in Burkina Faso.” Environmental Modelling & Software 115 (May 1, 2019): 164–75. \doi{10.1016/j.envsoft.2019.01.016}.
#' Ruett, Marius, Cory Whitney, and Eike Luedeling. “Model-Based Evaluation of Management Options in Ornamental Plant Nurseries.” Journal of Cleaner Production 271 (June 2020): 122653. \doi{10.1016/j.jclepro.2020.122653}.
#' 
#' 
#' @examples 
#' ##############################################################
#' # Example 1 (Creating the estimate from the command line):
#' #############################################################
#' # Create the estimate object:
#' 
#' variable = c("revenue", "costs")
#' distribution = c("norm", "norm")
#' lower = c(10000,  5000)
#' upper = c(100000, 50000)
#' costBenefitEstimate <- as.estimate(variable, distribution, lower, upper)
#' 
#' # (a) Define the model function without name for the return value:
#' 
#' profit1 <- function(x) {
#'   x$revenue - x$costs
#'   return(list(Revenues = x$revenue,
#'               Costs = x$costs))
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
#' # Plot the distributions
#' 
#' plot_distributions(mcSimulation_object = predictionProfit1, vars = c("Revenues", "Costs"),
#'          method = "smooth_simple_overlay")
#' 
#' plot_distributions(mcSimulation_object = predictionProfit1, vars = c("Revenues", "Costs"),
#'          method = "hist_simple_overlay", bins = 30)
#' 
#' plot_distributions(mcSimulation_object = predictionProfit1, vars = c("Costs"),
#'          method = "hist_simple_overlay", binwidth = 1000)
#' 
#' plot_distributions(mcSimulation_object = predictionProfit1, vars = c("Revenues", "Costs"),
#'          method = "boxplot_density", outlier_shape = 3)
#'  
#' 
#' @export plot_distributions
#' @import class
#'
plot_distributions <- function(mcSimulation_object, 
                               vars, 
                               method = "smooth_simple_overlay", 
                               bins = 150,
                               binwidth = NULL,
                               old_names = NULL, 
                               new_names = NULL, 
                               colors = NULL, 
                               outlier_shape = ".",
                               x_axis_name = "Outcome distribution", 
                               y_axis_name = NULL, 
                               base_size = 11, 
                               ...) {
  
  
  # Check if mcSimulation_object is class mcSimulation
  assertthat::assert_that(class(mcSimulation_object)[[1]] == "mcSimulation",
                          msg = "mcSimulation_object is not class 'mcSimulation', please provide a valid object. This does not appear to have been generated with the 'mcSimulation' function.")
  
  
  # Create a dataframe from the mcSimulation_object
  data <- data.frame(mcSimulation_object$y,
                     mcSimulation_object$x)
  
  
  # subset NPV variables
  data <- dplyr::select(data, tidyselect::all_of(vars))
  
  # define the names
  if (is.null(new_names) | is.null(old_names)){
    new_names <- names(data)
    old_names <- names(data)}
  
  #rename the data and add all_of() to overcome the ambiguity of 'external vector'
  data <- dplyr::rename_at(data, dplyr::vars(tidyselect::all_of(old_names)), ~ new_names)
  
  #assign data
  standard_plot_data <- tidyr::pivot_longer(data, tidyselect::all_of(new_names))
  
  # define the colors
  if (is.null(colors)){
    colors <- c("#009999", "#0000FF", "#56B4E9", "#009E73",
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")}
  
  # define the y_axis_name name
  if (!is.null(y_axis_name)) y_axis_name <- y_axis_name else
    if (method == "hist_simple_overlay") y_axis_name <- "Number of points in bin" else
      if (method == "boxplot") y_axis_name <- "Decision option" else
        y_axis_name <- "Density estimate"
  
  # define the standard plot to be used as baseline plot
  standard_plot <-  ggplot2::ggplot(standard_plot_data,
                                    ggplot2::aes(x = value, group = name, fill = name)) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.01), labels = scales::comma) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.01), labels = scales::comma) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::labs(x = x_axis_name, y = y_axis_name , fill = "Decision\noption") +
    ggplot2::theme_bw(base_size = base_size) 
  
  if (method == "smooth_simple_overlay") {
    return(standard_plot + ggplot2::geom_density(color = NA, alpha = 0.5) +
             ggplot2::theme(...))
  }
  
  if (method == "hist_simple_overlay") {
    return(standard_plot + ggplot2::geom_histogram(color = NA, alpha = 0.5, bins = bins,
                                                   position = "identity", binwidth = binwidth) +
             ggplot2::theme(...)) 
  }
  
  if (method == "boxplot") {
    return(ggplot2::ggplot(standard_plot_data, ggplot2::aes(x = value, y = stats::reorder(name, value, FUN = stats::median), fill = name)) + 
             ggplot2::geom_boxplot(outlier.alpha = 0.3) + 
             ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.01), labels = scales::comma) +
             ggplot2::scale_fill_manual(values = colors) +
             ggplot2::labs(x = x_axis_name, y = y_axis_name , fill = "Decision\noption") +
             ggplot2::theme_bw(base_size = base_size) + 
             ggplot2::theme(legend.position = "none") +
             ggplot2::theme(...))
  }
  
  
  if (method == "boxplot_density") {
    
    # Since the width of boxploth will change depending on the difference between options, we
    # implemented a small regression to allow the function define the correct value for the
    # width parameter.
    
    # Here we make an assumption to represent a wide range of distribution differences...
    # Percentage is the most "adequate" value for boxploth width according to the difference between
    # options (visual selection)
    
    data <- data.frame(percentage = c(0.1, 0.15, 0.5, 4),
                       options_difference = c(100, 11229249, 58838895, 507997898))
    
    #options_difference = c(11229249, 58838895, 507997898)
    
    # Compute a linear regression between percentage and option differences
    regression <- stats::lm(percentage ~ options_difference, data = data)
    
    # Estimate the difference between options provided by the user
    options_difference <- max(standard_plot_data$value) - min(standard_plot_data$value)
    
    # Compute the boxploth_width parameter using the linear regression coefficients
    boxploth_width_correction <- stats::coefficients(regression)[[1]] + (stats::coefficients(regression)[[2]] * options_difference)
    
    return(
      
      ggplot2::ggplot(standard_plot_data, ggplot2::aes(x = value, fill = name)) +
        ggplot2::geom_density(alpha = 0.5, color = NA) +
          ggplot2::geom_boxplot(ggplot2::aes(x = value, y = 0),
                                #place the boxplot consistently at the bottom of the graph
                                width = max(stats::density(standard_plot_data$value)$y *
                                              boxploth_width_correction),
                                varwidth = TRUE,
                                alpha = 0.5,
                                size = 0.3, 
                                outlier.shape = outlier_shape) +
        ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.01), labels = scales::comma) +
        ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.01), labels = scales::comma) +
        ggplot2::scale_fill_manual(values = colors) +
        ggplot2::labs(x = x_axis_name, y = y_axis_name) +
        ggplot2::facet_wrap(. ~ name, scales = 'free_y') +
        ggplot2::theme_bw(base_size = base_size) +
        ggplot2::theme(strip.background = ggplot2::element_blank(),
                       legend.position = "none",
                       axis.text.x = ggplot2::element_text(
                         angle = 45,
                         hjust = 1,
                         vjust = 1),
                       ...)
    )
    
  }
  
}
