#hoping for the best

#Drip Irrigation Project
#Necesary packages when necesity arise
# install.packages("decisionSupport")
# install.packages("dplyr")
# install.packages("ggplot2")

library(decisionSupport)
library(dplyr)
library(ggplot2)
library(readr)


#benefits of the project to fermers
KSH<-142.50 #1 Euro to kenya curency (ksh) 

yield<-80330 #at 14% MC, kg/ha
fertil<- 200*KSH*10 #amount fertilizer saved by the technology for 10ha

water_manage<-200*KSH #benefit of water management for a season
seed<-40*KSH #proper seed germination
weed_manag <- 200*KSH #savings from weed management by the technology
bn_farmers <- yield+fertil+water_manage+seed+weed_manag # benefit to farmers per season

#benefit to public
food_n <- 2000*KSH #benefit for nutrition per season
electric<- 5230*KSH #benefit kplc get from saving the water in a season
tourism <- 1320*KSH # benefit to toursim sector per season
bn_public <- food_n+electric+tourism


precalc_total_benefit<- (bn_farmers+bn_public)*2 #profit from two seasons in a year

print(precalc_total_benefit)

# cost analysis for instalation services provided by irrigation exparts (to be paid by farmer´s corpearatives)
#declaration of user input variables:

###land leveling and general labour

labour_cost <- 2*KSH*8*30 #cost per hour, per day for 30 workable days

###equipment 
area_covered<- 10 #in ha
pipe_needed <- 6000 #pipes
pipeprice<-1*KSH #cost per pipe
tractors <- 2 #tractor used to fit the drip pipes and leveling the ground

###cost of mainainance and operations
tractor_p<-4000*2*KSH #purchase price 
tractorgas <- 200*3*KSH #cost of litters of gas used per yr
pipestock<- 200*KSH #pipes to be in stock for a year
tractor_maintenance<-tractor_p*0.05 #per year
labor<-2*KSH*4*360 #per year
ocm <- tractorgas+pipestock+tractor_maintenance+labor+labour_cost

total_var_cost<- ocm

##fixed costs
fixc = pipeprice*pipe_needed + tractor_p


precalc_total_cost<-total_var_cost+fixc #total cost per year

print(precalc_total_cost)
print(precalc_total_benefit)

#benefit-cost ratio
BC_ratio<- precalc_total_benefit/precalc_total_cost
print(BC_ratio)

BC_ratio <- precalc_total_benefit / precalc_total_cost



# Create a bar plot
barplot(rbind(precalc_total_benefit, precalc_total_cost), beside = TRUE, names.arg = c("Benefit", "Cost"), 
        main = "Drip Irrigation Cost_Benefit check", xlab = "Category", ylab = "Amount",
        col = c("blue", "red"), legend.text = TRUE)

tinytex::install_tinytex()
      
