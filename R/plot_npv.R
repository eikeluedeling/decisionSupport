#' Net Present Value plots for Monte Carlo simulation results
#' 
#' Several plotting options for NPV outputs
#' 
#' @param mcSimulationResults is a data frame of Monte Carlo simulation outputs from the \code{\link[decisionSupport:decisionSupport]{decisionSupport}} function
#' @param method is the type of plot to be performed in \code\{link{ggplot2}} using \code{\link[ggplot2:geom_density]{geom_histogram}} or \code{\link[ggplot2:geom_histogram]{geom_histogram}} 
#' @param bins are the number of bins to use for the \code{\link[ggplot2:geom_histogram]{geom_histogram}}. Default number of bins is 150
#' @param old_names are the variable names from the MC simulation outputs that refer to the NPV values. This should be a vector of character strings. This is set to NULL with the assumption that the existing names for NPV variables are preferred 
#' @param new_names are the variable names to replace the MC simulation outputs that refer to the NPV values. This should be a vector of character strings. This is set to NULL with the assumption that the existing names for NPV variables are preferred
#' @param colors is the color palette to be used for the fill of distribution shapes and boxplots. The default is c("#009999", "#0000FF", "#56B4E9", "#009E73","#F0E442", "#0072B2", "#D55E00", "#CC79A7") assuming a maximum of eight NPV variables
#' @param outlier_shape is the optional shape to replace the outliers in the boxplot. To show no oultiers use NA. See \code{\link[ggplot2:aes_linetype_size_shape]{shape}} for shape options
#' 
#' @keywords Monte-Carlo decisionSupport decision-analysis net-present-value NPV risk uncertainty
#' 
#' 
#' @import tidyr dplyr ggplot2 tidyselect
#'  
#' @references 
#' Lanzanova Denis, Cory Whitney, Keith Shepherd, and Eike Luedeling. “Improving Development Efficiency through Decision Analysis: Reservoir Protection in Burkina Faso.” Environmental Modelling & Software 115 (May 1, 2019): 164–75. \url{https://doi.org/10.1016/j.envsoft.2019.01.016}.
#' 
#' @examples 
#' 
#' #Generate random dataset of 2000 Monte Carlo simulations for NPV
#' 
#' test_data <- data.frame(replicate(3, sample(-10000:10000, 2000, rep = TRUE)))
#' 
#' #Make a name for the NPV variables with time append (as is done in the decisionSupport function)  
#' names(test_data) <-  gsub(x = names(test_data),
#'                           pattern = "X",
#'                           replacement = "NPV")
#' 
#' 
#' plot_npv(mcSimulationResults = test_data, method = "smooth_simple_overlay")
#' plot_npv(mcSimulationResults = test_data, method = "hist_simple_overlay", bins = 30)
#' plot_npv(mcSimulationResults = test_data,"boxplot_density", outlier_shape = 3)
#' 
#' @export plot_npv
#'
plot_npv <- function(mcSimulationResults, method = "smooth_simple_overlay", bins = 150,
                     old_names = NULL, new_names = NULL, colors = NULL, outlier_shape = ".") {
  
  # subset NPV variables
  data <- dplyr::select(mcSimulationResults, tidyselect::starts_with("NPV"))
  
  # define the names
  if (is.null(new_names) | is.null(old_names)){
    new_names <- names(data)
    old_names <- names(data)}
    
  #remane the data and add all_of() to overcome the ambiguity of 'external vector'
  data <- dplyr::rename_at(data, dplyr::vars(tidyselect::all_of(old_names)), ~ new_names)
    
    #assign data
    standard_plot_data <- tidyr::pivot_longer(data, tidyselect::starts_with("NPV"))
    
    # define the colors
    if (is.null(colors)){
      colors <- c("#009999", "#0000FF", "#56B4E9", "#009E73",
              "#F0E442", "#0072B2", "#D55E00", "#CC79A7")}
    
    standard_plot <-  ggplot2::ggplot(standard_plot_data, aes(x = value, group = name, fill = name)) +
      scale_x_continuous(expand = expansion(mult = 0.01), labels = scales::comma) +
      scale_y_continuous(expand = expansion(mult = 0.01), labels = scales::comma) +
      scale_fill_manual(values = colors) +
      labs(x = "Net Present Value", y = "Probability density", fill = "Decision\noption") +
    theme_bw() 
    
    if (method == "smooth_simple_overlay") {
      return(standard_plot + ggplot2::geom_density(color = NA, alpha = 0.5) )
    }
  
    if (method == "hist_simple_overlay") {
      return(standard_plot + ggplot2::geom_histogram(color = NA, alpha = 0.5, bins = 150) ) 
    }
    
    if (method == "boxplot_density") {
      
      return(
        ggplot2::ggplot(standard_plot_data, aes(x = value, y = name, fill = name)) +
               geom_density(aes(y = ..count..), alpha = 0.5, color = NA) +
               ggstance::geom_boxploth(aes(x = value, y = 0),
                                       #place the boxplot consistently at the bottom of the graph
                                       width = .0025,
                                       alpha = 0.5,
                                       size = 0.3, 
                                       outlier.shape = outlier_shape) +
               scale_x_continuous(expand = expansion(mult = 0.01), labels = scales::comma) +
               scale_y_continuous(expand = expansion(mult = 0.01), labels = scales::comma) +
               scale_fill_manual(values = colors) +
               labs(x = "Net Present Value", y = "Frequency of model runs", fill = "Decision\noption") +
               facet_wrap(. ~ name) +
               theme_bw() +
               theme(strip.background = element_blank(),
                 legend.position = "none",
                 axis.text.x = element_text(
                   angle = 45,
                   hjust = 1,
                   vjust = 1))
        )
    
      }
    
}
