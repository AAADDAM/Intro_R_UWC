#section2
#2feb 2019
#Aadam Rawoot

# Section 2: 
# Make use of the ecklonia.csv dataset:
# Explore the data (Hint* head, tail, glimpse functions)
# Demonstrate the dimensions of the dataset
# Create three graphs; bargraph, line graph and boxplot: Write hypothesis for each of the graphs and answer these hypotheses
# Make use of the ggarrange function and arrange these three graphs created above into 1 plot
# All graphs must have labels as well as titles
# Calculate the mean,max,min,median and variance for the stipe_length, stipe_diameter for each of the sites (Hint* group_by site) - use summarize
# Calculate standard error -sd and var gives error
# Determine the min and maximum frond length and stipe length
# Determine the overall summary of the dataset - use summary
library(tidyverse)#loading packages in r with functions
library(lubridate)
library(dplyr)
library(ggpubr)
library(readr)
ecklonia <- read_csv("data/ecklonia.csv")#read in eckolnia dataset

ecko <- ecklonia#assign new name to ecklonia
rm(ecklonia)#remove ecklonia dataset from environment

head(ecko)#r auto does 6 rows
tail(ecko)
head(feb_data, n = 7)#head lets you see top 7 rows, n is number of rows
tail(feb_data, n = 7)#tails lets you see bottom 7 rows
glimpse(ecko)#more thorough summary
names(ecko)#names of all columns

ecko_data_select <- ecko %>% #pipe function, created by shift, control and M
  select(-stipe_length, -stipe_mass) #select function uses data selected, while select- removes data from dataset 

ecko_data_select <- ecko %>% 
  select(stipe_length, stipe_mass) %>%
  filter(stipe_length >= "500")#filter extracts info, '>=' means greater or equal to

dim(ecko)#get dimensions of the data

#hypothesis- the longer the stipe of ecklonia maxima, the heavier the stipe
#proven by graph

ggplot(ecko, aes( x = stipe_length, y = stipe_mass, colour = site)) + #ggplot creates a graph while aesthetics is a function that utilizes the x and y components to sketch your graph while colour is assigned to "site" so that we see a colour variation
  geom_point() + #geom_point gives us the geometric points of data 
  geom_line(aes (group = site)) +#geom_line is the type of graph we create and we group this data by site
  labs(x = "Stipe Length (mm)", y = "Stipe Mass (g)") +#labs allows us to change the axis titles of our graph
  ggtitle("Stipe Length of Eckolina maxima in relation to its mass") +#ggtitle allows us to add a title to the graph
  theme_classic()#theme allows a background theme to be added to the graph

Ecklonia_g_1 <- ggplot(ecko, aes( x = stipe_length, y = stipe_mass, colour = site)) + #ggplot creates a graph while aesthetics is a function that utilizes the x and y components to sketch your graph while colour is assigned to "site" so that we see a colour variation
  geom_point() + #geom_point gives us the geometric points of data 
  geom_line(aes (group = site)) +#geom_line is the type of graph we create and we group this data by site
  labs(x = "Stipe Length (mm)", y = "Stipe Mass (kg)") +#labs allows us to change the axis titles of our graph
  ggtitle("Stipe Length of Ecklonia maxima in relation to its Mass") +#ggtitle allows us to add a title to the graph
  theme_classic()#theme allows a background theme to be added to the graph

#hypothesis- the heavier the frond, the longer the frond of ecklonia maxima
#proven by graph

Ecklonia_g_2<- ggplot(data = ecko, aes(x = frond_length, y = frond_mass)) +
  geom_boxplot(aes(fill = site)) +
  labs(x = "Frond Length(mm)", y = "Frond Mass (kg)") +
  ggtitle("Frond Length of Ecklonia maxima in relation to its Mass") +
  theme()

#Hypotheis- Ecklonia generally grow much bigger in Batsata Rock than in Boulders Beach
#hypothesis proven by graph _ Ecklonia_g_3

Ecklonia_g_3 <- ggplot(ecko, aes( x = stipe_length, width = 7, y = stipe_mass, colour = site)) + #ggplot creates a graph while aesthetics is a function that utilizes the x and y components to sketch your graph while colour is assigned to "site" so that we see a colour variation
   
  geom_col(aes (group = site)) +#geom_col is the type of graph we create and we group this data by site, creates bars
  labs(x = "Stipe Length (mm)", y = "Stipe Mass (kg)") +#labs allows us to change the axis titles of our graph
  ggtitle("Stipe Length of Ecklonia maxima in relation to its Mass") +#ggtitle allows us to add a title to the graph
  theme_classic()#theme allows a background theme to be added to the graph

ggarrange(Ecklonia_g_1, Ecklonia_g_2, Ecklonia_g_3, #ggarrange used to arrange graphs in a certain order on one plot
          ncol = 2, nrow = 2,#set number of rows and columns
          labels = c("A", "B", "C", "D"),#give a label each figure
          common.legend = TRUE)#create common legend

Final_graph <- ggarrange(Ecklonia_g_1, Ecklonia_g_2, Ecklonia_g_3,#final graph name to save plot to environment 
                         ncol = 2, nrow = 2, 
                         labels = c("A", "B", "C", "D"), 
                         common.legend = TRUE) 


ecko_bar <- ecko %>%
  select(stipe_length, frond_length, primary_blade_length,site)



ecko_summary<- ecko %>% 
  summarise(mean_stipe_length = mean(stipe_length, na.rm = TRUE),#summarise, give a name to each first, mean of stipe length
            sd_stipe_length = sd(stipe_length, na.rm = TRUE),#standard dev of stipe length removing na.omits
            min_stipe_length = min(stipe_length, na.rm = TRUE),#minimum of stipe length
            max_stipe_length = max(stipe_length, na.rm = TRUE),#max of stipe length
            var_stipe_length = var(stipe_length, na.rm = TRUE),#variance of stipe length
            median_stipe_length = median(stipe_length, na.rm = TRUE))#median of stipe length


ecko_summary2<- ecko %>% 
  summarise(mean_stipe_diameter = mean(stipe_diameter, na.rm = TRUE),#summarise, give a name to each first, mean of stipe diameter
            sd_stipe_diameter = sd(stipe_diameter, na.rm = TRUE),#standard dev of stipe diameter removing na.omits
            min_stipe_diameter = min(stipe_diameter, na.rm = TRUE),#minimum of stipe diameter
            max_stipe_diameter = max(stipe_diameter, na.rm = TRUE),#max of stipe diameter
            var_stipe_diameter = var(stipe_diameter, na.rm = TRUE),#variance of stipe diameter
            median_stipe_diameter = median(stipe_diameter, na.rm = TRUE))#median of stipe diameter


ecko %>% # Select 'ecko'
  group_by(site) %>% # Group the dataframe by site
  summarise(var_sl = var(stipe_length), # Calculate variance
            n_sl = n()) %>%  # Count number of values
  mutate(se_sl = sqrt(var_sl / n_sl)) # Calculate se of stipe length


ecko_frond <- ecko %>% #renaming and assigning new name to ecko, save in environment
  summarise(min_frond_length = min(frond_length, na.rm = TRUE) ,#summarise function used to get min and max frond length excluding na's
            max_frond_length = max(frond_length, na.rm = TRUE))

ecko_stipe <- ecko %>% #renaming and assigning new name to ecko, save in environment
  summarise(min_stipe_length = min(stipe_length, na.rm = TRUE) ,#summarise function used to get min and max stipe length excluding na's
            max_stipe_length = max(stipe_length, na.rm = TRUE))

summary(ecko)#gives overall summary of the data

#end of section 2