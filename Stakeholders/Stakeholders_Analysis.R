# Imports ####
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(readxl)
library(dplyr)


#Ploting stakeholder analysis
watermanagement_stakeholder <- read.csv("Stakeholders/Stakeholder Management data.csv")
View(watermanagement_stakeholder)

#ploting the stakeholders data
ggplot(watermanagement_stakeholder, aes(x = Expertise,
                                        y = Capital_Availability,
                                        label = Stakeholders,
<<<<<<< HEAD:Water_managementproject_coding.R
                                        color = influence)) +
  geom_point(aes(shape = Entity)) +
=======
                                        color = Influence)) +
  geom_point() +
>>>>>>> 0dece34a90de301283d8fbf29d98de4804ac16ef:Stakeholders/Stakeholders_Analysis.R
  xlab("Adoption Influence") +
  
  
  #labeling the stakeholders names and showing in full names
  scale_x_continuous(labels = paste(seq(0, 5, by = 1)),
                     breaks = seq(0, 5, by = 1),
                     limits = c(0, 5),
                     expand = c(0, 1)) +
  
  scale_y_continuous(labels = paste(seq(0, 5, by = 1)), 
                     breaks = seq(0, 5, by = 1), 
                     limits = c(0, 5), 
                     expand = c(0, 1)) +
                       
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +

#create line to categorize stakeholders
  geom_hline(yintercept=2.5, color="yellow", linewidth=1) +
  geom_vline(xintercept=2.5, color="yellow", linewidth=1) +
    
# Show all names of overlapped values
  geom_text_repel(box.padding = 0.3, max.overlaps = Inf, size = 3) +
  annotate("text", label = "Priority Stakeholders", 
           x = 4.5, y = 2.5, size = 7, color = "grey48") +
  annotate("text", label = "Resource persons", 
           x = 2.5, y = 4.5, size = 7, color = "grey48")


mermaid("graph LR
        Y(Yield)-->I(Income); linkStyle 0 stroke:green, stroke-width:1.5px
        M(Market price)-->I; linkStyle 1 stroke: green, stroke-width:1.5px
        I-->F(Final result); linkStyle 2 stroke: green, stroke-width:1.5px
        CL(Labor cost)-->F; linkStyle 3 stroke: red, stroke-width:1.5px")                     
#Development of the impact model
#This is generated from the previous conceptual model
#recall the marmeid

library(igraph)
impact_path <- graph.formula(Yield -+ AgricIncome,
                             Management -+ AgricIncome,
                             Watersource -+ ExternalIncome,
                             AgricIncome -+ TotalProfit,
                             ExternalIncome -+ TotalProfit,
                             InstallCost -+ TotalCost,
                             ManagementCost -+ TotalCost,
                             TotalCost -+ HouseholdNetProfit,
                             TotalProfit -+ HouseholdNetProfit)
                             
plot(impact_path)


library(DiagrammeR)
mermaid(diagram = "graph LR
    Y(Yield)-->Ag(AgricIncome); linkStyle 0 stroke: blue, stroke-width:1.5px
    Mg(Management)-->Ag; linkStyle 1 stroke: blue, stroke-width:1.5px
    WB(Watersource)-->Ex(ExternalIncome); linkStyle 2 stroke: blue, stroke-width:1.5px
    Ag-->TP(TotalProfit); linkStyle 3 stroke:blue, stroke-width: 1.5px
    Ex-->TP; linkStyle 4 stroke: blue, stroke-width: 1.5px
    In(InstallCost)-->TC(Total_Cost); linkStyle 5 stroke: red, stroke-width:1.5px
    MngtC(ManagementCost)-->TC; linkStyle 6 stroke: red, stroke-width: 1.5px
    TP-->NP(NetProfit); linkStyle 7 stroke: blue, stroke-width: 1.5px
    TC-->NP; linkStyle 8 stroke: red, stroke-width: 1.5px")


#

#Building the model
library(decisionSupport)
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
                                              "Cost of project maintainance"))
dripline_estimates

install.packages("readr")

#view the estimate table
View(dripline_estimates)



dripline_mc_simulation <- mcSimulation(estimate = as.estimate(dripline_estimates),
                                      model_function = dripmodel_function,
                                      numberOfModelRuns = 500,
                                      functionSyntax = "plainNames")
library(decisionSupport)
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

dripline_mc_simulation <- mcSimulation(estimate = as.estimate(dripline_estimates),
                                       model_function = dripmodel_function,
                                       numberOfModelRuns = 500,
                                       functionSyntax = "plainNames")
dripline_mc_simulation

plot_distributions(mcSimulation_object = dripline_mc_simulation,
                   vars = "final_result",
                   method = "boxplot_density",
                   old_names = "final_result",
                   new_names = "Outcome distribution for profits")


#### SURFACE IRRIGATION ####
#surface irrigation pathway
library(igraph)
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
library(decisionSupport)
SurfaceIrr_estimates <- data.frame(variable = c("Yield","Management","Incomes","Maintenancecost"),
                                   lower = c(14, 2, 20, 4),
                                   median = NA,
                                   upper = c(20, 10, 23, 10),
                                   distribution = c("posnorm","posnorm","posnorm","posnorm"),
                                   label = c("Yield (kg/ha)","Savings","Incomes (Ksh)","managementcost(Ksh)"),
                                   Description = c("Total Yields (Ksh","Savings from Management (Ksh)","All incomes earned (Ksh)","Cost of project maintainance"))

View(SurfaceIrr_estimates)

#Surface Irrigation Function
SurfaceIrr_function <- function(){
  TotalBenefits <-(Yield+Management+Incomes)
  TotalCost<-Maintenancecost
  final_result0<-TotalBenefits-TotalCost
  return(list(final_result0 = final_result0))
}

# Monte-Carlo simulation
SurfaceIrr_mc_simulation <- mcSimulation(estimate = as.estimate(SurfaceIrr_estimates),
                                         model_function = SurfaceIrr_function,
                                         numberOfModelRuns = 600,
                                         functionSyntax = "plainNames")


#Plot distribution
plot_distributions(mcSimulation_object = SurfaceIrr_mc_simulation,
                   vars = "final_result","final_result0",
                   method = "boxplot_density",
                   old_names = "final_result",
                   new_names = "Outcome distribution for profits")

###########################################################################

# Imports ####
library(readr)
library(decisionSupport)

# Estimates table
input_table<-read.csv("Model Function/Estimates.csv")
View(input_table)

# Model function WITHOUT DROUGHT ####
irrigation_model_function_withoutDrought<-function(x){
  
  # calculate drought risks: impact the implementation of drought ####
  droughtEvent <-chance_event(Drought_Event, 0, 0, n=0)
  
  
  #  Intervention ####
  for (decision_drip_irrigation in c(FALSE,TRUE)){
    
    if (decision_drip_irrigation){
      
      # Profits ####
      Profits<-vv(Drip_Yield,Var_CV,n_years)*Marketvalue+Drip_Management+Drip_All_other_incomes
      
      # Costs ####
      Drip_Establishmentcost<-c(Drip_Establishmentcost,0,0,0,0)
      Overallcosts<- Drip_Establishmentcost+Drip_MaintenanceCost
      
      # Results ####
      net_benefits <- Profits - Overallcosts
      result_drip <- net_benefits
    }
    
    else{
      
      # Profits ####
      # Drought
      if (droughtEvent){
        Profits<-vv(Surface_Yield,Var_CV,n_years)*
          (1-vv(Drought_Discount, Var_CV,n_years))*
          Marketvalue+Surface_Management+Surface_All_other_incomes
      }
      
      # No drought
      else{
        Profits<-vv(Surface_Yield,Var_CV,n_years)*Marketvalue
        +Surface_Management+Surface_All_other_incomes}
      
      # Costs ####
      Surface_Establishmentcost<-c(Surface_Establishmentcost,0,0,0,0)
      Overallcosts<- Surface_MaintenanceCost+Surface_Establishmentcost
      
      # Results ####
      net_benefits <- Profits - Overallcosts
      result_surface <- net_benefits}
    
  }   #close intervention loop bracket
  NPV_interv <-
    discount(result_drip, discount_rate, calculate_NPV = TRUE)
  
  NPV_n_interv <-
    discount(result_surface, discount_rate, calculate_NPV = TRUE)
  
  return(list(Drip_NPV = NPV_interv,
              Surf_NPV = NPV_n_interv,
              NPV_decision_do = NPV_interv - NPV_n_interv,
              Cashflow_decision_drip = result_drip,
              Cashflow_decision_surface = result_surface))}

mcSimulation_results_withoutDrought <- decisionSupport::mcSimulation(
  estimate = decisionSupport::estimate_read_csv("Estimates.csv"),
  model_function = irrigation_model_function_withoutDrought,
  numberOfModelRuns = 200,
  functionSyntax = "plainNames"
)

# Plot distrbution ####
decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results_withoutDrought, 
                                    vars = c("Drip_NPV","Surf_NPV"),
                                    method = 'smooth_simple_overlay', 
                                    base_size = 7)+
  decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results_withoutDrought, 
                                      vars = c("Drip_NPV",
                                               "Surf_NPV"),
                                      method = 'boxplot')+
  
  # Plot cashflow ####
plot_cashflow(mcSimulation_object = mcSimulation_results_withoutDrought, 
              cashflow_var_name = c("Cashflow_decision_drip", "Cashflow_decision_surface"),
              x_axis_name = "Years with intervention",
              y_axis_name = "Annual cashflow in KSh",
              color_25_75 = "purple4", color_5_95 ="purple2",
              color_median = "red", 
              facet_labels = c("Drip irrigation", "Surface irrigation"))


# Compound for surface irrigation
compound_figure(model = irrigation_model_function_withoutDrought, 
                input_table = input_table, 
                decision_var_name = "Surf_NPV",
                cashflow_var_name = "Cashflow_decision_surface",
                model_runs = 1e2, 
                distribution_method = 'smooth_simple_overlay')+
  # compound for drip irrigation
  compound_figure(model = irrigation_model_function_withoutDrought, 
                  input_table = input_table, 
                  decision_var_name = "Drip_NPV",
                  cashflow_var_name = "Cashflow_decision_drip",
                  model_runs = 1e2, 
                  distribution_method = 'smooth_simple_overlay')

# Model function WITH DROUGHT ####
irrigation_model_function_withDrought<-function(x){
  
  # calculate drought risks: impact the implementation of drought ####
  droughtEvent <-chance_event(Drought_Event, 1, 0, n = 1)
  
  
  #  Intervention ####
  for (decision_drip_irrigation in c(FALSE,TRUE)){
    
    if (decision_drip_irrigation){
      
      # Profits ####
      Profits<-vv(Drip_Yield,Var_CV,n_years)*Marketvalue+Drip_Management+Drip_All_other_incomes
      
      # Costs ####
      Drip_Establishmentcost<-c(Drip_Establishmentcost,0,0,0,0)
      Overallcosts<- Drip_Establishmentcost+Drip_MaintenanceCost
      
      # Results ####
      net_benefits <- Profits - Overallcosts
      result_drip <- net_benefits
    }
    
    else{
      
      # Profits ####
      # Drought
      if (droughtEvent){
        Profits<-vv(Surface_Yield,Var_CV,n_years)*
          (1-vv(Drought_Discount, Var_CV,n_years))*
          Marketvalue+Surface_Management+Surface_All_other_incomes
      }
      
      # No drought
      else{
        Profits<-vv(Surface_Yield,Var_CV,n_years)*Marketvalue
        +Surface_Management+Surface_All_other_incomes}
      
      # Costs ####
      Surface_Establishmentcost<-c(Surface_Establishmentcost,0,0,0,0)
      Overallcosts<- Surface_MaintenanceCost+Surface_Establishmentcost
      
      # Results ####
      net_benefits <- Profits - Overallcosts
      result_surface <- net_benefits}
    
  }   #close intervention loop bracket
  NPV_interv <-
    discount(result_drip, discount_rate, calculate_NPV = TRUE)
  
  NPV_n_interv <-
    discount(result_surface, discount_rate, calculate_NPV = TRUE)
  
  return(list(Drip_NPV = NPV_interv,
              Surf_NPV = NPV_n_interv,
              NPV_decision_do = NPV_interv - NPV_n_interv,
              Cashflow_decision_drip = result_drip,
              Cashflow_decision_surface = result_surface))}

mcSimulation_results_withDrought <- decisionSupport::mcSimulation(
  estimate = decisionSupport::estimate_read_csv("Estimates.csv"),
  model_function = irrigation_model_function_withDrought,
  numberOfModelRuns = 200,
  functionSyntax = "plainNames"
)

# Plot distrbution ####
decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results_withDrought, 
                                    vars = c("Drip_NPV","Surf_NPV"),
                                    method = 'smooth_simple_overlay', 
                                    base_size = 7)+
  decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results_withDrought, 
                                      vars = c("Drip_NPV",
                                               "Surf_NPV"),
                                      method = 'boxplot')+
  
  # Plot cashflow ####
plot_cashflow(mcSimulation_object = mcSimulation_results_withDrought, 
              cashflow_var_name = c("Cashflow_decision_drip", "Cashflow_decision_surface"),
              x_axis_name = "Years with intervention",
              y_axis_name = "Annual cashflow in Ksh",
              color_25_75 = "purple4", color_5_95 ="purple2",
              color_median = "red", 
              facet_labels = c("Drip irrigation", "Surface irrigation"))


# Compound drip irrigation
compound_figure(model = irrigation_model_function_withDrought, 
                input_table = input_table, 
                decision_var_name = "Drip_NPV",
                cashflow_var_name = "Cashflow_decision_drip",
                model_runs = 1e2, 
                distribution_method = 'smooth_simple_overlay')+
  #compound surface irrigation
  compound_figure(model = irrigation_model_function_withDrought, 
                  input_table = input_table, 
                  decision_var_name = "Surf_NPV",
                  cashflow_var_name = "Cashflow_decision_surface",
                  model_runs = 1e2, 
                  distribution_method = 'smooth_simple_overlay')