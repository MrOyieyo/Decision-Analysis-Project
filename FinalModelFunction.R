# Imports ####
library(readr)
library(decisionSupport)

# Estimates table
input_table<-read.csv("Estimates.csv")
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
      Overallcosts<- Surface_MaintenanceCost
      
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


# Compound 
compound_figure(model = irrigation_model_function_withoutDrought, 
                input_table = input_table, 
                decision_var_name = "Surf_NPV",
                cashflow_var_name = "Cashflow_decision_surface",
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
      Overallcosts<- Surface_MaintenanceCost
      
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
              Cashfolw_decision_surface = result_surface))}

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
              cashflow_var_name = c("Cashflow_decision_drip", "Cashfolw_decision_surface"),
              x_axis_name = "Years with intervention",
              y_axis_name = "Annual cashflow in Ksh",
              color_25_75 = "purple4", color_5_95 ="purple2",
              color_median = "red", 
              facet_labels = c("Drip irrigation", "Surface irrigation"))


# Compound 
compound_figure(model = irrigation_model_function_withDrought, 
                input_table = input_table, 
                decision_var_name = "Drip_NPV",
                cashflow_var_name = "Cashflow_decision_drip",
                model_runs = 1e2, 
                distribution_method = 'smooth_simple_overlay')
