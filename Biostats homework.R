#Bioststs homework
#15 April - due 19th 
#Exericse 6 7 and 9 
#load libraries

library(fitdistrplus)
library(logspline)
library(tidyverse)
library(ggpubr)
library(lubridate)
library(dplyr)
library(readr)

# 6.7

# HYPOTHESIS: males are larger in length compared to females

# Creating a random dataset
random <- data.frame(length = c(rnorm(n = 100, mean = 12, sd = 2),
                                rnorm(n = 100, mean = 8, sd = 2)),
                     sample = c(rep("Male", 100), rep("Female", 100)))

# Visualise the data using a boxplot
ggplot(data = random, aes(x = sample, y = length, fill = sample)) +
  geom_boxplot() +
  coord_flip() +
  labs(y = "length", x = "") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
# Boxplots showing difference in lengths of males and females

# Question: are males larger than females?
# H0: Length of males ARE NOT greater than length of females
# H1: Length of males ARE greater than length of females

# For a t-test, we must meet certain assumptions

# To perform a t-test, data must be normal and homoscedastic
# calculated using shaprio.test and var, respectively
random %>% 
  group_by(sample) %>% 
  summarise(norm_dat = as.numeric(shapiro.test(length)[2]),
            var_dat = var(length)[1])
# for both male and female, p > 0.05 ie data is normal
# homoscedastic because variance of one is not more than two to four times greater than the other

# t-test traditional output
t.test(length ~ sample, data = random, var.equal = TRUE, alternative = "greater")

# dataframe output
compare_means(length ~ sample, data = random, method = "t.test", var.equal = TRUE, alternative = "greater")
# p-value = 2.04e-33 (p < 0.05)
# because p < 0.05, we can reject the null hypothesis and accept the alternative hypothesis

# In conclusion, length of males was found to be significantly greater 
# than the length of females (according to p-value in compare_means) 

# BUT t.test and compare_means give two different p-values...
# t.test: p = 1, t = -14.556, df = 198 (shows no significant difference?)
# RWS: I think it is because compare_means() sees that 
# you want a "greater" than test and so decides for you if either of the groups is greater
# The base t.test() function does not do this.


# 7.4.1 Exercise 1

# enter the mass at the end of the experiment
feed_1 <- c(60.8, 57.0, 65.0, 58.6, 61.7)
feed_2 <- c(68.7, 67.7, 74.0, 66.3, 69.8)
feed_3 <- c(102.6, 102.1, 100.2, 96.5)
feed_4 <- c(87.9, 84.2, 83.1, 85.7, 90.3)

# make a dataframe
bacon <- as.tibble(data.frame(
  feed = c(
    rep("Feed 1", length(feed_1)),
    rep("Feed 2", length(feed_2)),
    rep("Feed 3", length(feed_3)),
    rep("Feed 4", length(feed_4))
  ),
  mass = c(feed_1, feed_2, feed_3, feed_4)
))

# ANOVA 

# QUESTION: Does feed type have an effect on the mass of pigs at the end of the experiment?
# H0: There is NO difference in pig mass at the end of the experiments after being fed one of four diets
# H1: There IS a difference in pig mass at the end of the experiments after being fed one of four diets

pigs.aov <- aov(mass ~ feed, data = bacon)
summary(pigs.aov)
# Pr < 0.05, so we reject the null hypothesis
# so, there is a difference in pig mass at the end of the experiment after being fed one of four diets

# There is a difference, but which feeds have the effect?

# Visualise with boxplots 

ggplot(data = bacon, aes(x = feed, y = mass, fill = feed )) +
  geom_boxplot(notch = TRUE)
# none of the feeds notched overlap... but this graph is just a suggestion

# Tukey 

TukeyHSD(pigs.aov)
# p-adj < 0.05 for all feeds: show all diets are different and significant
# lwr and upr does not go across zero in all cases

# Visualise this 

plot(TukeyHSD(pigs.aov))

# 7.4.2 Exercise 2 

teeth <- datasets::ToothGrowth

# QUESTION: Does difference in doses of Vitamin C have an effect on the length of tooth growth in guinea pigs
# H0: There is NO difference in tooth lengths of guinea pigs receiving one of three doses of vitamin C
# H1: There IS a difference in tooth lengths of guinea pigs receiving one of three doses of vitamin C

# Filter out only Vitamin C doses 

teeth_vc <- ToothGrowth %>% 
  filter(supp == "VC")

# ANOVA 

teeth.aov <- aov(len ~ as.factor(dose), data = teeth_vc)
summary(teeth.aov)
# pr < 0.05, so we reject the null hypothesis
# so, there is a difference in tooth lengths of guinea pigs receiving one of three doses of vitamin C

# So which dosages have the effect?

# Visualise this using a boxplot 

ggplot(data = teeth_vc, aes(x = as.factor(dose), y = len, fill = as.factor(dose))) +
  geom_boxplot(notch = TRUE)
# none of the dose notches overlap... suggesting they are different

# Tukey 

TukeyHSD(teeth.aov)
# p-adj < 0.05, very low for all. so all doses are significant and different 
# lwr and upr does not cross zero

# Visualise this 

plot(TukeyHSD(teeth.aov))

# 7.4.3 Exercise 3 

teeth <- datasets::ToothGrowth

# H0: interactions between supplement and dose have NO effect on length of teeth
# H1: interactions between supplement and dose DO have an effect on length of teeth

# looking at only length by supplement
summary(aov(len ~ supp, data = teeth))

TukeyHSD((aov(len ~ supp, data = teeth)))

plot(TukeyHSD((aov(len ~ supp, data = teeth))))

# dose was done in previous example, but running too many ANOVAS increases error
# therefore

# now to look at interactions BETWEEN factors
summary(aov(len ~ supp * as.factor(dose), data = teeth)) 
# pr < 0.05, so we reject the null hypothesis
# so, interactions between supplement and dose DOES havae an effcet on length of teeth

# So which have the effect?
TukeyHSD((aov(len ~ supp * as.factor(dose), data = teeth)))

plot(TukeyHSD((aov(len ~ supp * as.factor(dose), data = teeth))))
# On plot, all combinations not crossing zero show which combinations of supplement and diet have the most effect on length of teeth  

# Exercise 9.6 

library(reshape2)
melted_eck <- melt(ecklonia_pearson)

ggplot(melted_eck, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  scale_fill_gradient(low = "yellow", high = "indianred", name = "Pearson correlation") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))

#DONE
#hope it works now


# Script runs
# Neat but do need more comments describing what each code does. Remember this shows the marker what you are doing and that you understand what you are doing
# Good hypothesis
# Reember to explore R and try different things or different graphs

