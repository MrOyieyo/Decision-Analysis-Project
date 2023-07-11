# Imports ####
library(readr)
library(decisionSupport)

# Model function WITH DROUGHT ####
irrigation_model_function_withDrought<-function(x){
  
  # calculate drought risks: impact the implementation of drought ####
  droughtEvent <-chance_event(Drought_Event, 1, 0, n = 5)
  
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
      Profits<-vv(Surface_Yield,Var_CV,n_years)*
          (1-vv(Drought_Discount, Var_CV,n_years)*droughtEvent)*
          Marketvalue+Surface_Management+Surface_All_other_incomes
      
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
  
  