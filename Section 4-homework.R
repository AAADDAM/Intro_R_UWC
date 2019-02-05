#Section 4
# 2 feb 2019
#Aadam Rawoot 

# Make use of any two built in datasets:
# Make use of the summarise, select, group_by functions
# Create at least two visualisations that were not done in the Intro R workshop use geom_plot

#Load libraries

library(tidyverse)

co2 <- datasets::CO2#rename dataset to add it to environment

summ_co2 <- co2 %>%#rename again and pipe
  group_by(Type, Treatment, conc) %>% #group by certain variables, used in code
  summarise(mn.up = mean(uptake),#summarise the mean and standard deviation uptake
            sd.up = sd(uptake))

ggplot(data = summ_co2, aes(x = Type, y = mn.up, fill = Treatment)) +#ggplot used to plot a graph, data used from the summarised data. aes is asthetics used to plot x and y entries on axis, fill used by treatment variable
  geom_violin() +#shape given
  labs(x = "Place", y = "Mean uptake") +#labs used to rename x and y axis
  ggtitle("Average uptake rate of CO in 2 sites") +#ggtitle used to give a title
  theme()#theme added to graph

 datasets::ToothGrowth#loaded dataset toothgrowth

  mn.sd <- ToothGrowth%>% #renamed and added to environment as mn.sd then piped
   group_by(supp, dose) %>%#group by used to group variables and then
   summarise(mn.ln = mean(len),#summarised average length and stdv length
            sd.ln = sd(len))
 
 
teeth_plot <- ToothGrowth#renamed teeth_plot
 
teeth_plot <- ggplot(mn.sd, aes(x = dose, y = mn.ln, fill = supp)) +#ggplot mn.sd data used
   geom_col(aes(fill = supp), position = "dodge", colour = "black") + #geom_col used to create columns
   geom_errorbar(aes(ymin = mn.ln - sd.ln,#geom_errorbar creates error bar
                     ymax = mn.ln + sd.ln), 
                 position = "dodge") +
    labs(x = "Dose (mg/d)", y = "Tooth length (mm)") +#labs used to rename x and y axis
  ggtitle("Dosage of supplements in relation to tooth growth")#creates title



#end of section 4!

