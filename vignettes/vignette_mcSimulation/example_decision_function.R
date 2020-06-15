library(tidyverse)
library(decisionSupport)
library(ggplot2)

# A function to create variables ####
# This is used for running parts of the model line by line within the function 
# before running the whole MC

make_variables <- function(est, n = 1)
{
  x <- random(rho = est, n = n)
  for (i in colnames(x))
    assign(i, as.numeric(x[1, i]), envir = .GlobalEnv)
}

make_variables(estimate_read_csv("vignettes/vignette_mcSimulation/example_input_table.csv"))

example_decision_function <- function(x, varnames){
  # ex-post risk: impacts the benefits ####
  # None included yet
  # ex-ante risk: impacts the implementation of interventions ####
  intervention_NonPopInvolvEvent <-
    chance_event(intervention_NonPopInvolv, 1, 0, n = 1)
  
  # calculation of common random draws for all intervention model runs ####
  
  TLU <- vv(TLU_no_intervention, var_CV, n_years)
  TLU_profit <- vv(profit_per_TLU, var_CV, n_years)
  
  precalc_intervention_fruit_benefits <-
    vv(intervention_fruit_area_ha, var_CV, n_years) *
    vv(intervention_fruit_yield_t_ha, var_CV, n_years) *
    vv(intervention_fruit_profit_USD_t, var_CV, n_years)
  
  precalc_intervention_vegetable_benefits <-
    vv(intervention_vegetable_area_ha, var_CV, n_years) *
    vv(intervention_vegetable_yield_t_ha, var_CV, n_years) *
    vv(intervention_vegetable_profit_USD_t, var_CV, n_years)
  
  precalc_intervention_rainfed_crop_benefits <-
    vv(intervention_rainfed_crop_area_ha, var_CV, n_years) *
    vv(intervention_rainfed_crop_yield_t_ha, var_CV, n_years) *
    vv(intervention_rainfed_crop_profit_USD_t, var_CV, n_years)
  
  #  Intervention ####
  
  for (decision_intervention_strips in c(FALSE,TRUE))
      {
      
  if (decision_intervention_strips)
  {
    intervention_strips <- TRUE
    intervention_strips_PlanningCost <- TRUE
    intervention_strips_cost <- TRUE
  } else
  {
    intervention_strips <- FALSE
    intervention_strips_PlanningCost <- FALSE
    intervention_strips_cost <- FALSE
  }
  
  if (intervention_NonPopInvolvEvent) {
    intervention_strips <- FALSE
    intervention_strips_cost <- FALSE
  }
  
  # Costs ####
  if (intervention_strips_cost) {
    cost_intervention_strips <-
      intervention_adaptation_cost + intervention_tech_devices_cost + intervention_nursery_cost +
      intervention_wells_cost +
      intervention_training_cost + intervention_mngmt_oprt_cost + intervention_mngmt_follow_cost +
      intervention_mngmt_audit_cost
  } else
    cost_intervention_strips <- 0
  
  if (intervention_strips_PlanningCost) {
    plan_cost_intervention_strips <-
      intervention_communication_cost + intervention_zoning_cost
  } else
    plan_cost_intervention_strips <- 0
  
  maintenance_cost <- rep(0, n_years)
  
  if (intervention_strips)
    maintenance_cost <-
    maintenance_cost + vv(maintenance_intervention_strips, var_CV, n_years)
  
  intervention_cost <- maintenance_cost
  intervention_cost[1] <-
    intervention_cost[1] + cost_intervention_strips + plan_cost_intervention_strips

  
  # Benefits from  cultivation in the intervention strips ####
  
  intervention_fruit_benefits <-
    as.numeric(intervention_strips) * precalc_intervention_fruit_benefits
  intervention_vegetable_benefits <-
    as.numeric(intervention_strips) * precalc_intervention_vegetable_benefits
  intervention_rainfed_crop_benefits <-
    as.numeric(intervention_strips) * precalc_intervention_rainfed_crop_benefits
  
  # Total benefits from crop production (agricultural development and riparian zone) ####
  crop_production <-
    intervention_fruit_benefits +
    intervention_vegetable_benefits +
    intervention_rainfed_crop_benefits
  
  # Benefits from livestock ####
  # The following allows considering that intervention strips may
  # restrict access to the reservoir for livestock.
  
  if (intervention_strips)
    TLU_intervention <-
    TLU * (1 + change_TLU_intervention_perc / 100)
  else
    TLU_intervention <- TLU
  
  if (decision_intervention_strips){
  
    livestock_benefits <- TLU_intervention * TLU_profit
    
    total_benefits <- crop_production + livestock_benefits
    
    net_benefits <- total_benefits - intervention_cost
    
    result_interv <- net_benefits}
  
  
  if (!decision_intervention_strips){
    
    livestock_benefits <- TLU_no_intervention * TLU_profit
    
    total_benefits <- livestock_benefits
    
    net_benefits <- total_benefits - intervention_cost
    
    result_n_interv <- net_benefits}
  
    } #close intervention loop bracket

NPV_interv <-
  discount(result_interv, discount_rate, calculate_NPV = TRUE)

NPV_n_interv <-
  discount(result_n_interv, discount_rate, calculate_NPV = TRUE)

return(list(Interv_NPV = NPV_interv,
       NO_Interv_NPV = NPV_n_interv,
       Cashflow_interv = result_interv,
       Cashflow_n_interv = result_n_interv))
}

# Running the model ####

# This uses mcSimulation function instead of decisionSupport.
# Be aware of using 'estimate_read_csv' for importing the estimates.
# 'p', the output of the function, is a list having the elements y, x and call. We do not know
# why the colnames in y are output_1 and _2 respectively.
# It seems that the functionSyntax argument is not working well.


test_mcSimulation_function <- decisionSupport::mcSimulation(
  estimate = decisionSupport::estimate_read_csv("vignettes/vignette_mcSimulation/example_input_table.csv"),
  model_function = example_decision_function,
  numberOfModelRuns = 1e4, #run 10,000 times
  functionSyntax = "plainNames"
)

plot_distributions(mcSimulation_object = test_mcSimulation_function, vars = c("Interv_NPV", "NO_Interv_NPV"),
                   method = 'boxplot_density',
                   y_axis_name = "Hi",
                   axis.title.x = ggplot2::element_text(size = 15, family = "serif"))

plot_cashflow(mcSimulation_object = test_mcSimulation_function, cashflow_var_name = c("Cashflow_interv",
                                                                                      "Cashflow_n_interv"),
              axis.title.x = ggplot2::element_text(size = 15, family = "serif"))


# Look for the class of the mcSimulation output

class(test_mcSimulation_function)

str(test_mcSimulation_function)






# Here we used the welfare function


test_welfare_function <- decisionSupport::welfareDecisionAnalysis(
  estimate = decisionSupport::estimate_read_csv("vignettes/vignette_mcSimulation/example_input_table.csv"),
  welfare = example_decision_function,
  numberOfModelRuns = 1e4, #run 10,000 times
  functionSyntax = "plainNames"
)


# Here the decision support function

decisionSupport::decisionSupport(
  inputFilePath = "vignettes/vignette_mcSimulation/example_input_table.csv",
  outputPath = "vignettes/vignette_mcSimulation/test_results",
  welfareFunction = example_decision_function,
  numberOfModelRuns = 1e4, #run 10,000 times
  functionSyntax = "plainNames",
  write_table = TRUE
)



mcSimulationResults <- read.csv("vignettes/vignette_mcSimulation/test_results/mcSimulationResults.csv")

names(mcSimulationResults)










test_mcSimulation_function_2 <- mcSimulation(
  estimate = decisionSupport::estimate_read_csv("vignettes/vignette_mcSimulation/example_input_table.csv"),
  model_function = example_decision_function,
  numberOfModelRuns = 1e4, #run 10,000 times
  functionSyntax = "plainNames"
)


class(test_mcSimulation_function_2) == "mcSimulation"





mcSimulation_DF <- data.frame(test_mcSimulation_function$y,
                              test_mcSimulation_function$x)


names(mcSimulation_DF)






plot_distributions(mcSimulation_object = test_mcSimulation_function, vars = c("Interv_NPV", "NO_Interv_NPV"),
                   method = 'boxplot_density',
                   y_axis_name = "Hi",
                   axis.title.x = ggplot2::element_text(size = 15, family = "serif"))




class(test_mcSimulation_function)[[1]]


plot_cashflow(mcSimulation_object = test_mcSimulation_function, cashflow_var_name = c("Cashflow_interv",
                                                                                      "Cashflow_n_interv"))


