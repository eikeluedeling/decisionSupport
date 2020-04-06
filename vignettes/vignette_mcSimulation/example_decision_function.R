
library(decisionSupport)

# A function to create variables ####
# This is used for running parts of the model line by line within the function 
# before running the whole MC
make_variables <- function(est, n = 1)
{
  x <- random(rho = est, n = n)
  for (i in colnames(x))
    assign(i, as.numeric(x[1, i]), envir = .GlobalEnv)
}

make_variables(estimate_read_csv("Example_Decision_Support/example_input_table.csv"))


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
  
  livestock_benefits <- TLU_intervention * TLU_profit
  
  total_benefits <- crop_production + livestock_benefits
  
  net_benefits <- total_benefits - intervention_cost
  
  if (decision_intervention_strips)
    result_interv <- net_benefits
  
  if (!decision_intervention_strips)
    result_n_interv <- net_benefits
  
    } #close intervention loop bracket

NPV_interv <-
  discount(result_interv, discount_rate, calculate_NPV = TRUE)

NPV_n_interv <-
  discount(result_n_interv, discount_rate, calculate_NPV = TRUE)

return(NPV_DO_minus_DONT=NPV_interv - NPV_n_interv)
}

# Running the model ####

decisionSupport::decisionSupport(
  "Example_Decision_Support/example_input_table.csv",
  outputPath = 'Example_Decision_Support/results',
  welfareFunction = example_decision_function,
  numberOfModelRuns = 1e4, #run 10,000 times
  functionSyntax = "plainNames"
)
