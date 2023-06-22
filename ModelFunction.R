# Imports
library(decisionSupport)


#Building the model
dripline_estimates <- data.frame(variable = c("Yield","Marketvalue", "Management", "Watersource", "Nutrition", "Powergeneration", "Establishmentcost", 
                                              "Managementcost"),
                                 lower = c(25, 0.5, 10, 6, 3, 5, 18, 8),
                                 median = NA,
                                 upper = c(90, 5, 39, 20, 13, 17, 81, 34),
                                 distribution = c("posnorm", "posnorm", "posnorm", "posnorm", "posnorm", "posnorm", "posnorm", "posnorm"),
                                 label = c("Yield (kg/ha)", "Price (ksh/kg)", "management_savings(ksh)", "Basinprofits(ksh)", "Nutrientgain(ksh)",
                                           "Electricity(ksh/Kw)", "instalationcost(ksh", "managementcost(Ksh)"),
                                 Description = c("Yield of the maize crop", "Market price per KG of maize grain", "Reduced cost of weed management",
                                                 "Benefits generated from efficient water use from basin", "Value of food nutrient generated", 
                                                 "Kw of electricity increased by increased water levels", "Cost of generating installing the project", 
                                                 "Cost of project maintainance" ))

dripline_mc_simulation <- mcSimulation(estimate = as.estimate(dripline_estimates),
                                       model_function = dripmodel_function,
                                       numberOfModelRuns = 800,
                                       functionSyntax = "plainNames")

dripmodel_function <- function(){
  #The model_function
  # Estimate the income in a normal season
  AgriIncome <- (Yield*Marketvalue) + Management 
  ExtIncome <- Watersource + Nutrition + Powergeneration
  TotalProfits <- AgriIncome + ExtIncome
  
  # Estimate the overall costs
  Overallcosts<- Establishmentcost + Managementcost
  
  # Estimate the final results from the model
  final_result <- TotalProfits - Overallcosts
  
  # Generate the list of outputs from the Monte Carlo simulation
  return(list(final_result = final_result))
}

# Monte-Carlo simulation
dripline_mc_simulation <- mcSimulation(estimate = as.estimate(dripline_estimates),
                                       model_function = dripmodel_function,
                                       numberOfModelRuns = 800,
                                       functionSyntax = "plainNames")
dripline_mc_simulation

#Plot distribution
plot_distributions(mcSimulation_object = dripline_mc_simulation,
                   vars = "final_result",
                   method = "boxplot_density",
                   old_names = "final_result",
                   new_names = "Outcome distribution for profits")







