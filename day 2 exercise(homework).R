#day 2 homework
#Aadam Rawoot
#30TH Jan 2019
#choose 3 datasets and create 2 graphs that are different for each
#calculate the mean of one column in those datasets i.e 3 means
#compose a hypothesis
#plus 3 graphs in lamanaria data


#Exercise 1- Hypothesis, graphs and means for 3 types of data sets

#Hypothesis- the temperature readings of day 2 will reach a higher maximum than that of day 1.
#- based on mammal_graph1 and mammal_graph2, hypothesis accepted

mammals_G1<- datasets::beaver1 #mammals_G1 is the name assigned to dataset beaver1
ggplot(mammals, aes( x = time, y = temp, colour = day)) + #ggplot creates a graph while aesthetics is a function that utilizes the x and y components to sketch your graph while colour is assigned to "day" so that we see a colour variation
  geom_point() + #geom_point gives us the geometric points of data 
  geom_line(aes (group = day)) +#geom_line is the type of graph we create and we group this data by day
  labs(x = "time (minutes)", y = "temp (℃)") +#labs allows us to change the axis titles of our graph
  ggtitle("Temperature(℃) readings across time (minutes)") +#ggtitle allows us to add a title to the graph
  theme_classic()#theme allows a background theme to be added to the graph

mammal_graph1 <- ggplot(mammals, aes( x = time, y = temp, colour = day)) +#here mammal_graph1 is assigned to the entire code above in order to make mammal_graph1 the outlet to depict the graph when clicked on
  geom_point() +
  geom_line(aes (group = day)) +
  labs(x = "time (minutes)", y = "temp (℃)") +
  ggtitle("Temperature(℃) readings across time (minutes)") +
  theme_classic()



mammals_G2<- datasets::beaver1#mammals_G2 is the name assigned to dataset beaver1 
ggplot(mammals, aes( x = time, y = temp, colour = day)) +#ggplot creates a graph while aesthetics is a function that utilizes the x and y components to sketch your graph while colour is assigned to "day" so that we see a colour variation
  geom_point(aes (group = day)) +#geom_point gives us the geometric points of data 
  geom_smooth(method = "lm") +#geom_smooth is the type of graph we create which gives us the best fit line for the graph and lm means linear model
  labs(x = "time (minutes)", y = "temp (℃)") +#labs allows us to change the axis titles of our graph
  ggtitle("Temperature(℃) readings across time (minutes)") +#ggtitle allows us to add a title to the graph
  theme_classic()#theme allows a background theme to be added to the graph


mammal_graph2 <- ggplot(mammals, aes( x = time, y = temp, colour = day)) +#here mammal_graph2 is assigned to the entire code above in order to make mammal_graph2 the outlet to depict the graph when clicked on
  geom_point(aes (group = day)) +
  geom_smooth(method = "lm") +
  labs(x = "time (minutes)", y = "temp (℃)") +
  ggtitle("Temperature(℃) readings across time (minutes)") +
  theme_classic()  

#mean calculation
mammals_G1 %>% #mammals_G1 is the data set selected and then
  summarise(avrg_temp = mean(temp))#this data is summarised using the function summarise from tidyverse and average is used to get the mean

#Hypothesis:guinea pig teeth will grow faster if there is a dosage increase in supplements
#based on Tooth_growth_graph3, hypothesis accepted
Tooth_growth_g3<- datasets::ToothGrowth#Tooth_growth_g3 is the name assigned to ToothGrowth dataset
ggplot(Tooth_growth_g3, aes( x = dose, y = len, colour = supp)) +#ggplot creates a graph while aesthetics is a function that utilizes the x and y components to sketch your graph while colour is given to supplememnts to group data
  geom_point() +#geom_point gives us the geometric points of data 
  geom_smooth(method = "lm") +#geom_smooth is the type of graph we create which gives us the best fit line for the graph and lm means linear model
  labs(x = "dose (mg)", y = "length (mm)") +#labs allows us to change the axis titles of our graph
  ggtitle("Growth of teeth in guinea pigs relying on dosage of supplement given") +#ggtitle allows us to add a title to the graph
  theme_classic()#theme allows a background theme to be added to the graph

Tooth_growth_graph3 <-ggplot(Tooth_growth_g3, aes( x = dose, y = len, colour = supp)) +#here Tooth_growth_graph3 is assigned to the entire code above in order to make Tooth_growth_graph3 the outlet to depict the graph when clicked on
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "dose (mg)", y = "length (mm)") +
  ggtitle("Growth of teeth in guinea pigs relying on dosage of supplement given") +
  theme_classic() 

#Hypothesis:some guinea pigs that recieved a large dose of the supplement VC will attain a greater tooth length than the guinea pigs who recieve a similar dosage of OJ
#Hypothesis accepted based on Tooth_growth_graph4

Tooth_growth_g4<- datasets::ToothGrowth#Tooth_growth_g3 is the name assigned to ToothGrowth dataset
ggplot(Tooth_growth_g4, aes( x = supp, y = len, colour = dose)) +#ggplot creates a graph while aesthetics is a function that utilizes the x and y components to sketch your graph while colour is given to dose to group data
  geom_point() +#geom_point gives us the geometric points of data
  geom_smooth(method = "lm") +#geom_smooth is the type of graph we create which gives us the best fit line for the graph and lm means linear model
  labs(x = "supplement", y = "length (mm)") +#labs allows us to change the axis titles of our graph
  ggtitle("Growth of teeth in guinea pigs depending on the supplement given") +#ggtitle allows us to add a title to the graph
  theme_classic()#theme allows a background theme to be added to the graph

Tooth_growth_graph4 <- ggplot(Tooth_growth_g4, aes( x = supp, y = len, colour = dose)) +#here Tooth_growth_graph4 is assigned to the entire code above in order to make Tooth_growth_graph4 the outlet to depict the graph when clicked on
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "supplement", y = "length (mm)") +
  ggtitle("Growth of teeth in guinea pigs depending on the supplement given") +
  theme_classic()

#mean calculation
Tooth_growth_g3 %>%#dataset Tooth_growth_g3 is used and then
  summarise(avrg_length = mean(len))#this data is summarised using the function summarise from tidyverse and average is used to get the mean

#Hypothesis: the thicker the tree, the taller it is.
#based on Trees_graph5, hypothesis accepted

  Trees_g5 <- datasets::trees#Trees_g5 is assigned to the dataset trees
ggplot(Trees_g5, aes( x = Girth, y = Height)) +#ggplot creates a graph while aesthetics is a function that utilizes the x and y components to sketch your graph
  geom_point() +#geom_point gives us the geometric points of data
  geom_smooth(method = "lm") +#geom_smooth is the type of graph we create which gives us the best fit line for the graph and lm means linear model
  labs(x = "Girth (cm)", y = "Height (cm)") +#labs allows us to change the axis titles of our graph
  ggtitle("Girth of trees in relation to their height") +#ggtitle allows us to add a title to the graph
  theme_classic()#theme allows a background theme to be added to the graph

Trees_graph5 <- ggplot(Trees_g5, aes( x = Girth, y = Height)) +#here Trees_graph5 is assigned to the entire code above in order to make Trees_graph5 the outlet to depict the graph when clicked on
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Girth (cm)", y = "Height (cm)") +
  ggtitle("Girth of trees in relation to their height") +
  theme_classic()

#Hypothesis: the taller the tree, the more lumber it can produce
#Trees_graph6 shows that the hypothesis is accepted

Trees_g6 <- datasets::trees#Trees_g5 is assigned to the dataset trees
ggplot(Trees_g6, aes( x = Height, y = Volume)) +#ggplot creates a graph while aesthetics is a function that utilizes the x and y components to sketch your graph
  geom_point() +#geom_point gives us the geometric points of data
  geom_smooth(method = "lm") +#geom_smooth is the type of graph we create which gives us the best fit line for the graph and lm means linear
  labs(x = "Height(cm)", y = "Volume (cm3)") +#labs allows us to change the axis titles of our graph
  ggtitle("Volume of Timber in relation to the height of trees") +#ggtitle allows us to add a title to the graph
  theme_classic()#theme allows a background theme to be added to the graph


Trees_graph6 <- ggplot(Trees_g6, aes( x = Height, y = Volume)) +#here Trees_graph6 is assigned to the entire code above in order to make Trees_graph6 the outlet to depict the graph when clicked on
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Height(cm)", y = "Volume (cm3)") +
  ggtitle("Volume of Timber in relation to the height of trees") +
  theme_classic()

#mean calculation
Trees_g5 %>%#dataset used is Trees_g5 and then
  summarise(avrg_height = mean(Height))#this data is summarised using the function summarise from tidyverse and average is used to get the mean
  

#Exercise 2- create 2 or more graphs from lamanaria dataset(be creative)- I tried

lamnew <- laminaria %>% #lamnew is assigned to lamanaria dataset
  group_by(site) %>% #and then grouped by site
  select(-Ind,-blade_thickness,-stipe_mass,-stipe_length, -stipe_diameter,-digits, -thallus_mass, -total_length)#and then select is used to remove various columns and entries
ggplot(lamnew, aes(x = blade_length, y = blade_weight, colour = site)) +#ggplot is then used to graph the columns and data left over.
  geom_point() +# geom_point is used to plot lines of the data
  geom_line(aes (group = site)) +#geom_line is used to connect that data
  labs(x = "blade length (mm)", y = "blade weight (mg)") +#labs is used to rename axis titles on the graph
  ggtitle("length and weight of blades in lamanaria species at different sites") +#ggtitle is used to create a heading for the graph
  theme_grey()#theme is used to create or assign a background to the graph

lam_site <- laminaria %>%#lam_site is assigned to lamanaria dataset and then
  select(site,stipe_mass, stipe_length) %>%#select is used to include certain variables
  filter(site == "Olifantsbos")#filter is used to take out everything else except the selected variable
ggplot(lam_site, aes(x = stipe_length, y = stipe_mass, colour = site)) +#ggplot is used to depict a graph using the components calculated with aesthetics, colour is assigned the site selected
  geom_point() +# geom_point is used to plot lines of the data
  geom_line(aes (group = site)) +#geom_line is used to connect that data
  labs(x = "Stipe length (mm)", y = "Stipe weight (mg)") +#labs is used to rename axis titles on the graph
  ggtitle("length and mass of stipes in lamanaria species at Olifantsbos") +#ggtitle is used to create a heading for the graph
  theme_bw()#theme is used to create or assign a background to the graph

library(ggpubr)
library(tidyverse)  
library(lubridate)  
  
#c concatenate,making your own set of numbers
#geom_plot for tidyverse
#just bar is for base r
