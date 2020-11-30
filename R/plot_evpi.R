#' Visualizing the results of Expected Value of Perfect Information (EVPI) analysis for various types of Monte Carlo simulation results
#' 
#' Plotting the Expected Value of Perfect Information (EVPI) of Monte Carlo outputs
#'
#' @param EVPIresults are the results of the \code{\link[decisionSupport:multi_EVPI]{multi_EVPI}} function
#' @param input_table is a data frame with at least two columns named 'variable' and 'label'. The 'variable column should have one entry for the name of each variable contained in any of the plots. In preparing the figure, the function will replace the variable names with the labels. If the label is missing then the plot will show 'NA' in the place of the variable name. Default is NULL and uses the original variable names.
#' @param decision_vars are the names of the decision variables in the output of the \code{\link[decisionSupport:mcSimulation]{mcSimulation}} function
#' @param new_names are the reformatted replacement names of the decision variables in the output of the \code{\link[decisionSupport:mcSimulation]{mcSimulation}} function
#' @param unit is the symbol to display before the evpi value on the x axis. It accepts text or (many) unicode formatted symbol text
#' @param x_axis_name is the name (character string) to be passed to the x-axis title. Default is "Expected Value of Perfect Information" and allows allow the user to add a customized axis title
#' @param y_axis_name is the name (character string) to be passed to the y-axis title. Default is NULL to allow the user to add a customized axis title
#' @param bar_color is the color to be used for the EVPI barplot. Default is "cadetblue"
#' @param base_size is the base text size to be used for the plot. The default is 11, this is the \code{\link[ggplot2:theme_bw]{ggplot::theme_bw}} default
#' @param ... accepts arguments to be passed to \code{\link[ggplot2:theme]{ggplot::theme}}
#' 
#' 
#' @keywords Monte-Carlo decisionSupport decision-analysis net-present-value EVPI perfect-information risk uncertainty
#' 
#'  
#' @references 
#' Do, Hoa, Eike Luedeling, and Cory Whitney. “Decision Analysis of Agroforestry Options Reveals Adoption Risks for Resource-Poor Farmers.” Agronomy for Sustainable Development 40, no. 3 (June 2020): 20. \url{https://doi.org/10.1007/s13593-020-00624-5}
#' Lanzanova, Denis, Cory Whitney, Keith Shepherd, and Eike Luedeling. “Improving Development Efficiency through Decision Analysis: Reservoir Protection in Burkina Faso.” Environmental Modelling & Software 115 (May 1, 2019): 164–75. \url{https://doi.org/10.1016/j.envsoft.2019.01.016}
#' Luedeling, Eike, and Keith Shepherd. “Decision-Focused Agricultural Research.” Solutions 7, no. 5 (2016): 46–54. \url{https://www.thesolutionsjournal.com/article/decision-focused-agricultural-research/}
#' 
#' @examples 
#' 
#' # Create a data.frame
#'
#' montecarlo <- data.frame(indep1 = rnorm(1000, sd = 50, mean = 100), 
#'                          indep2 = rnorm(1000, sd = 100, mean = 100))
#' montecarlo[, 'output1'] <- montecarlo[, 'indep1'] * montecarlo[, 'indep2']
#' montecarlo[, 'output2'] <- (montecarlo[, 'indep1'] * (montecarlo[, 'indep2']) + 10)
#' 
#' # Run the multi_EVPI function 
#'  
#' results_all <- multi_EVPI(montecarlo,"output1")
#' 
#' plot_evpi(results_all, decision_vars = c("output1", "output2"), 
#' new_names = c("Decision option 1", "Decision option 2"))
#' 
#' @export plot_evpi
#' 
plot_evpi <- function(EVPIresults, 
                      decision_vars, 
                      input_table = NULL, 
                      new_names = NULL, 
                      unit = NULL, 
                      x_axis_name = "Expected Value of Perfect Information",
                      y_axis_name = NULL,
                      bar_color = "cadetblue", 
                      base_size = 11, 
                      ...){
  
  # Check if EVPIresults is class mvr
  assertthat::assert_that("EVPI_outputs" %in% class(EVPIresults),
                          msg = "EVPIresults is not class 'EVPI_outputs', please provide a valid object. This does not appear to have been generated with the 'multi_EVPI' function.")

  # Check that input table is a data frame (or not)
  if (!is.null(input_table))
  assertthat::assert_that(any(class(input_table) %in% c("tbl_df", "tbl", "data.frame")), 
                          msg = "The input_table is not a data.frame or tibble (tbl, tbl_df) class, please provide a valid object.")
    
  # use the result of multi_EVPI() to create a full data frame
  full_evpi_data <- NULL

  # use a for loop to select the names of the data frames to use as a new column
  for (i in 1:length(EVPIresults)) {
    
    data <- EVPIresults[[i]]
    
    data["output_variable"] <- names(EVPIresults)[i]
    
    #merge the data frames
    if (is.null(full_evpi_data))
      full_evpi_data <- data else
        
      full_evpi_data <- dplyr::bind_rows(full_evpi_data, data)
  }
  
  # remove the names of the rows for the full evpi data
  rownames(full_evpi_data) <- NULL

  # check that the input table is available
  if (!(is.null(input_table)))
  
  # join data frames (use 'by = ' to match columns from both data frames)
  combined_table <- dplyr::left_join(full_evpi_data, 
                                     input_table, by = c( "variable" = "variable")) else
    combined_table <- full_evpi_data

  # Filter the data to only show positive EVPI
  filtered_table <- dplyr::filter(combined_table, EVPI > 0)
  
  # add a stop for cases where there are no positive EVPI
  if(nrow(filtered_table) == 0) {
    warning("There are no variables with a positive EVPI. You probably do not need a plot for that.", 
         call. = FALSE)
    return(invisible(NULL)) }
  
  
  # Check that the decision_vars are in the evpi data set
  assertthat::assert_that(any(decision_vars %in% filtered_table$output_variable), 
                          msg = "The names provided for decision_vars do not match the names in the EVPIresults. Make sure that they are in the EVPIresults and are spelled correctly.")
  
  # subset the data according to the user-defined decision variables
  data <- dplyr::filter(filtered_table, output_variable %in% decision_vars)
  
  if (is.null(new_names))
    decision_labels <- decision_vars else
      decision_labels <- new_names
    
  data$output_variable <- factor(data$output_variable,
                                 levels = decision_vars, labels = decision_labels)
  # safety check
  if (!is.null(input_table))
    y_axis <- "label" else
    y_axis <- "variable"
  
  # safety check
  if (is.null(unit))
    unit <- ""
  
# with one output variable and facet
ggplot2::ggplot(data, 
                ggplot2::aes(x = EVPI, y = stats::reorder(!!ggplot2::ensym(y_axis), EVPI))) +
  ggplot2::geom_col(fill = bar_color) +
  ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.01)), 
                              labels = scales::dollar_format(prefix = unit)) +
  ggplot2::labs(y = y_axis_name, x = x_axis_name) + 
  ggplot2::facet_wrap( ~ output_variable, scales = "free") +
  ggplot2::theme_bw(base_size = base_size) +
  ggplot2::theme(strip.background = ggplot2::element_blank()) + 
  ggplot2::theme(...)

}
