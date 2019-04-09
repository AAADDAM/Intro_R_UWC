# exercise_7feb.R

# Exercise ----------------------------------------------------------------

library(tidyverse)

egg <- read_delim("Desktop/YearlyEggBreadths.csv", 
           ";", escape_double = FALSE, trim_ws = TRUE) # because my data always reads in weirdly and read_csv doesnt always work for me.

egg_cut <- egg[-c(13:16),] # remove last few empty lines so dataset goes up to only december 


ggplot(data = egg_cut, aes(x = Months, y = AveragesMinBreadth1)) + # set axes using egg_cut dataset
  geom_line(aes(group = 1, colour = "Average annual minimun egg breadths"), # in aes() -- group=1 (just because R wants the group specified), colour="" is supposed to give the line name... but i dont know why its not working now 
            colour = "black", size = 1) + # outside of aes() -- colour="" sets the colour of the line, size= sets the size of the line
  geom_errorbar(aes(ymin = AveragesMinBreadth1 - as.numeric(StdDevMinBreadth1), # as.numeric() has to be added here because the standard deviation was read in as a character not a number 
                    ymax = AveragesMinBreadth1 + as.numeric(StdDevMinBreadth1)), # run egg_cut in the console and see Months is <chr> and StdDevMinBreadth1 is also <chr> therefore has to be specified here as a numeric value otherwise you'll get an error
                width = 0.1, colour = "black") + # error bars will always be specified like this (ymin=mean-sd, ymax=mean+sd). Width= specifies how narrow or wide errorbars will be (the default is quite wide)
  geom_line(aes(y = AveragesMinBreadth2, group = 1, colour = "Adjusted average annual minimum egg breadths"), # for your second line, specify which y values should be used.
            colour = "red", size = 1) + # see notes at lines 10 + 11
  geom_errorbar(aes(ymin = AveragesMinBreadth2 - as.numeric(StdDevMinBreadth2), # as.numeric() is not necessary here because StdDevMinBreadth2 is already a numerical value. But i like to make my lines look similar so i just added it here again. You can leave it out obviously and it wont change anything 
                    ymax = AveragesMinBreadth2 + as.numeric(StdDevMinBreadth2)), 
                width = 0.1, colour = "red") + # see notes at lines 12-14
  scale_x_discrete(limits = month.name) + # this changes the x-axis so that the months appear in month name order, not alphabetically
  scale_y_continuous(breaks = seq(0, 70, 10)) + # set y-axis at min 0 and max 70, going up in steps of 10
  theme_classic() + # change theme of the plot
  labs(x = "Month", y = "Egg breadth (mm)") # add axes labels


