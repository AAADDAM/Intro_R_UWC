#Aadam Rawoot
#7 feb 2019
#newest work
#load libraries

library(tidyverse)
library(ggpubr)
library(lubridate)
library(dplyr)
library(readr)

YearlyEggBreadths2 <- read_delim("exercise/data/YearlyEggBreadths2.csv", 
                                 ";", escape_double = FALSE, trim_ws = TRUE)
View(YearlyEggBreadths2)




ggplot(data = YearlyEggBreadths2, aes(x = Months, y = AveragesMinBreadth1)) +
  geom_line(aes(group = 1, colour = "Average annual minimum egg breadths"),
            colour = "black", size = 1) +
  geom_errorbar(aes(ymin = AveragesMinBreadth1 - as.numeric(StdDevMinBreadth1),
                    ymax = AveragesMinBreadth1 + as.numeric(StdDevMinBreadth1)),
                width = 0.1) +
  geom_line(aes(x = Months, y = AveragesMinBreadth2, group = 1,
                colour = "Adjusted average annual minimum egg breadths"),
            colour = "darkorange3", size = 1) +
  geom_errorbar(aes(ymin = AveragesMinBreadth2 - as.numeric(StdDevMinBreadth2),
                    ymax = AveragesMinBreadth2 + as.numeric(StdDevMinBreadth2)),
                width = 0.1, colour = "red") +
  scale_x_discrete(limits = month.name) +
  scale_y_continuous(breaks = seq(0, 70, 10)) +
  theme_classic() + # change theme of the plot
  labs(x = "Month", y = "Egg breadth (mm)") # add axes labels
  