# Imports
library(decisionSupport)
library(igraph)

#### DRIP IRRIGATION ####
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



#### SURFACE IRRIGATION ####
#surface irrigation pathway
SurfaceIrr_path<-graph.formula(SurfaceIrr-+Yield,
                               SurfaceIrr-+Incomes,
                               SurfaceIrr-+Management,
                               MaintenanceCost-+SurfaceIrr,
                               MaintenanceCost-+TotalCost,
                               Incomes-+TotalBenefits,
                               Management-+TotalBenefits,
                               Yields-+TotalBenefits,
                               TotalBenefits-+CurrentSituationOutcome,
                               TotalCost-+CurrentSituationOutcome)
plot(SurfaceIrr_path)

SurfaceIrr_Path<-graph.formula()

#surface Irrigation estimates
SurfaceIrr_estimates <- data.frame(variable = c("Yield","Management","Incomes","Maintenancecost"),
                                   lower = c(14, 2, 20, 4),
                                   median = NA,
                                   upper = c(20, 10, 23, 10),
                                   distribution = c("posnorm","posnorm","posnorm","posnorm"),
                                   label = c("Yield (kg/ha)","Savings","Incomes (Ksh)","managementcost(Ksh)"),
                                   Description = c("Total Yields (Ksh","Savings from Management (Ksh)","All incomes earned (Ksh)","Cost of project maintainance"))

SurfaceIrr_estimates

#Surface Irrigation Function
SurfaceIrr_function <- function(){
  TotalBenefits <-(Yield+Management+Incomes)
  TotalCost<-Maintenancecost
  final_result<-TotalBenefits-TotalCost
  return(list(final_result = final_result))
}

# Monte-Carlo simulation
SurfaceIrr_mc_simulation <- mcSimulation(estimate = as.estimate(SurfaceIrr_estimates),
                                       model_function = SurfaceIrr_function,
                                       numberOfModelRuns = 800,
                                       functionSyntax = "plainNames")


#Plot distribution
plot_distributions(mcSimulation_object = SurfaceIrr_mc_simulation,
                   vars = "final_result",
                   method = "boxplot_density",
                   old_names = "final_result",
                   new_names = "Outcome distribution for profits")




