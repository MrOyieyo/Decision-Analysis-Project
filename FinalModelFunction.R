# Imports ####
library(readr)
library(decisionSupport)

# Estimates table
input_table<-read.csv("Estimates.csv")
View(input_table)

# Model function ####
irrigation_model_function<-function(x){
  
  # calculate drought risks: impact the implementation of drought ####
  droughtEvent <-chance_event(Drought_Event, 1, 0, n = 1)
  
  
  #  Intervention ####
  for (decision_drip_irrigation in c(FALSE,TRUE)){
    
    if (decision_drip_irrigation){
      
      # Profits ####
      Profits<-vv(Drip_Yield,Var_CV,n_years)*Marketvalue+Drip_Management+Drip_All_other_incomes
      
      # Costs ####
      Overallcosts<- Drip_Establishmentcost
      
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

mcSimulation_results <- decisionSupport::mcSimulation(
  estimate = decisionSupport::estimate_read_csv("Estimates.csv"),
  model_function = irrigation_model_function,
  numberOfModelRuns = 200,
  functionSyntax = "plainNames"
)

# Plot distrbution ####
decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = c("Drip_NPV","Surf_NPV"),
                                    method = 'smooth_simple_overlay', 
                                    base_size = 7)+
decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = c("Drip_NPV",
                                             "Surf_NPV"),
                                    method = 'boxplot')


plot_cashflow(mcSimulation_object = mcSimulation_results, cashflow_var_name = "Cashflow_decision_surface",
              x_axis_name = "Years with intervention",
              y_axis_name = "Annual cashflow in Ksh",
              color_25_75 = "green4", color_5_95 = "green1",
              color_median = "red")

plot_cashflow(mcSimulation_object = mcSimulation_results, 
              cashflow_var_name = c("Cashflow_decision_drip", "Cashfolw_decision_surface"),
              x_axis_name = "Years with intervention",
              y_axis_name = "Annual cashflow in USD",
              color_25_75 = "purple4", color_5_95 ="purple2",
              color_median = "red", 
              facet_labels = c("Drip irrigation", "Surface irrigation"))
