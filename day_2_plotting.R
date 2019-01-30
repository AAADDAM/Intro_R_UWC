# PLOTTING IN R USING GGPLOT2
#DAY 2
#AADAM RAWOOT
#30TH JAN 2019

# LOAD LIBRARIES

library(tidyverse)

chicks <- datasets::ChickWeight
??ChickWeight

ggplot(data = chicks, aes(x = Time, y = weight)) + 
  geom_point() +
  geom_line(aes (group = Chick))

#first line is your parent line
#aes means aesthetics- controls everything within that function
#first line depicts info that r must use to create graph
#can use ggplot2
#use plus sigm for plotting , pipe is only for plain equations
#for plots you only use group not group by




ggplot(chicks, aes( x = Time, y = weight, colour = Diet)) +
  geom_point() +
  geom_line(aes(group = Chick))
#geompoint,line, histo etc
#shows how diet affects chicks in terms of time and weight

ggplot(chicks,aes(x = Time, y = weight, colour = Diet)) +
  geom_point() +
  geom_smooth(method = "lm")

#geom smooth is a linear model.


ggplot(chicks, aes( x = Time, y = weight, colour = Diet)) +
  geom_point(colour = "purple") +
  geom_line(aes(group = Chick))

ggplot(chicks, aes(x = Time, y = weight, colour = Diet)) +
  geom_point(aes(size = weight)) +
  geom_smooth(method = "lm") +
  labs(x = "Days", y = "Weight (kg)") +
  ggtitle("Nandos") +
  theme_bw()

#first assign dataset, then aes, make sure to use plus sign, use geompoints and either line or smooth to pick a linear method
#labs allows you to change or add labels, theme lets you change appearance

#exercise 2
mammals <- datasets::beaver1

ggplot(mammals, aes( x = time, y = temp, colour = day)) +
  geom_point() +
  geom_line(aes (group = day))

#beaver dataset used
#time used as x variable and y used as temp

#faceting- creating multiple figures on one 
#done in ggplot

library(ggpubr)

ggplot(chicks, aes(x = Time, y = weight, colour = Diet)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~Diet, ncol = 4)
#~ - tilda
#specifies diet on seperate panes 
#ncol shows number of columns
#ggpubr adds faceing
#nrow to add number of rows

chicks_2 <- chicks %>%
  filter(Time == 21)

plot_1<- ggplot(chicks, aes(x = Time, y = weight, colour = Diet)) +
  geom_point() +
  geom_line (aes(group = Chick)) +
  labs(x = "Days", y = "Weight") +
  ggtitle("C")
plot_1

plot_2 <- ggplot(chicks,aes(x = Time, y = weight, colour = Diet)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("D")

plot_3 <- ggplot(data = chicks_2,aes(x = weight)) +
  geom_histogram(aes(fill = Diet), position = "dodge", binwidth = 100) +
  labs(x = "Final Mass (g)", y = "Count")
plot_3
#dodge allows the seperation of bars 
#binwidth width of the bars

plot_4 <- ggplot(data = chicks_2, aes(x = Diet, y = weight)) +
  geom_boxplot(aes(fill = Diet)) +
  labs(x = "Diet", y = "Final Mass (g)")
plot_4

#first control enter saves plot to environment
#highlighting the name and control enter makes the plot appear


plot_combined <- ggarrange(plot_1, plot_2, plot_3, plot_4)
#ggarrange found in ggpubr

#choose 3 datasets and create 2 graphs that are different for each
#calculate the mean of one column in those datasets i.e 3 means
#compose a hypothesis


#3rd library
library(boot)


urine <- boot::urine
??urine

urine %>% 
  select(-cond)

