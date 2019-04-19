#Day 2 Biostats
#15 April 2019
#Normal Distributions

library(fitdistrplus)
library(logspline)
library(tidyverse)
library(ggpubr)
library(lubridate)
library(dplyr)
library(readr)
library(ggplot2)
# Generate log-normal data

y <- c(37.50,46.79,48.30,46.04,43.40,39.25,38.49,49.51,40.38,36.98,40.00,
       38.49,37.74,47.92,44.53,44.91,44.91,40.00,41.51,47.92,36.98,43.40,
       42.26,41.89,38.87,43.02,39.25,40.38,42.64,36.98,44.15,44.91,43.40,
       49.81,38.87,40.00,52.45,53.13,47.92,52.45,44.91,29.54,27.13,35.60,
       45.34,43.37,54.15,42.77,42.88,44.26,27.14,39.31,24.80,16.62,30.30,
       36.39,28.60,28.53,35.84,31.10,34.55,52.65,48.81,43.42,52.49,38.00,
       38.65,34.54,37.70,38.11,43.05,29.95,32.48,24.63,35.33,41.34)

summary(y)

par(mfrow = c(2, 2))
plot(x = c(1:length(y)), y = y)
hist(y)
descdist(y, discrete = FALSE, boot = 100)

x <- c(18,9,31,7,47,28,20,300,19,6,19,21,99,85,52,68,69,3,48,116,15,27,51,100,105,99,73,58,1,89,222,56,27,36,300,121,5,42,184,88,24,127,67,93,85,60,92,23,39,140,60,71,333,42,16,51,151,625,624,200,350,4,105,199,88,742)

par(mfrow = c(2, 2))
plot(x = c(1:length(x)), y = x)
hist(x)
descdist(y, discrete = FALSE, boot = 100)

windows()

summary(x)


# normally distributed data
xy <- rnorm(100, 13, 2)
par(mfrow = c(2, 2))
plot(x = c(1:100), y = xy)
hist(xy)
descdist(xy, discrete = FALSE)


xyz <- runif(100)
par(mfrow = c(2, 2))
plot(x = c(1:100), y = xyz)
hist(xyz)
descdist(y, discrete = FALSE)


y <-rnorm(n = 200, m = 13, sd = 2)
par(mfrow = c(2, 2))
# using some basic base graphics as ggplot2 is overkill;
# we can get a histogram using hist() statement
hist(y, main = "Histogram of observed data")
plot(density(y), main = "Density estimate of data")
plot(ecdf(y), main = "Empirical cumulative distribution function")
# standardise the data
z.norm <- (y - mean(y)) / sd(y) 
# make a qqplot
qqnorm(z.norm)
# add a 45-degree reference line
abline(0, 1)




set.seed(666)
r_dat <- data.frame(dat = c(rnorm(n = 1000, mean = 10, sd = 3),
                            rnorm(n = 1000, mean = 8, sd = 2)),
                    sample = c(rep("A", 1000), rep("B", 1000)))

dim(r_dat)

# Create histogram
h <- ggplot(data = r_dat, aes(x = dat, fill = sample)) +
  geom_histogram(position = "dodge", binwidth = 1, alpha = 0.8) +
  geom_density(aes(y = 1*..count.., fill = sample), colour = NA, alpha = 0.4) +
  labs(x = "value")
h
shapiro.test(r_dat$dat)

# we use the square bracket notation to select only the p-value;
# had we used `[1]` we'd have gotten W
r_dat %>% 
  group_by(sample) %>% 
  summarise(norm_dat = as.numeric(shapiro.test(dat)[2]))

str(r_dat)

r_dat %>% 
  group_by(sample) %>% 
  summarise(sample_var = var(dat))


#create a single sample of random normal data
set.seed(666)
r_one <- data.frame(dat = rnorm(n = 20, mean = 20, sd = 5),
                    sample = "A")

# check normality
shapiro.test(r_one$dat)

# No variance to compare
# ...

# compare random data against a population mean of 20
t.test(r_one$dat, mu = 20)#inferential statistic


# compare random data against a population mean of 30
t.test(r_one$dat, mu = 30)

ggplot(data = r_one, aes(y = dat, x = sample)) +
  geom_boxplot(fill = "lightsalmon") +
  # population  mean (mu) = 20
  geom_hline(yintercept = 20, colour = "blue", 
             size = 1, linetype = "dashed") +
  # population  mean (mu) = 30
  geom_hline(yintercept = 30, colour = "red", 
             size = 1, linetype = "dashed") +
  labs(y = "Value", x = NULL) +
  coord_flip()
 
#if p < 0.005, reject null hyp, significance ?

#check against the trailing tail
t.test(r_one$dat, mu = 30, alternative = "greater")

# R assumes we want a to-sided test
t.test(r_one$dat, mu = 30)


# but we can be explicit as we choose
t.test(r_one$dat, mu = 30, alternative = "two.sided")


# random normal data
set.seed(666)
r_two <- data.frame(dat = c(rnorm(n = 20, mean = 4, sd = 1),
                            rnorm(n = 20, mean = 5, sd = 1)),
                    sample = c(rep("A", 20), rep("B", 20)))
# perform t-test
# note how we set the `var.equal` argument to TRUE because we know 
# our data has the same SD (they are simulated as such!)
t.test(dat ~ sample, data = r_two, var.equal = TRUE)

#normal data- bar graph
#not normal- box and whisker plot

#Chapter 7- ANOVA

# First grab the data
chicks <- as_tibble(ChickWeight)

# Then subset out only the sample sets to be compared
chicks_sub <- chicks %>% 
  filter(Diet %in% c(1, 2), Time == 21)
compare_means(weight ~ Diet, data = chicks_sub, method = "t.test")
#ACCEPT NULL HYP, 
t.test(weight ~ Diet, data = chicks_sub)

chicks.aov1 <- aov(weight ~ Diet, data = filter(chicks, Time == 21))
summary(chicks.aov1)



chicks.aov1_g<- ggplot(data = filter(chicks, Time == 21), aes(x = Diet, y = weight)) +
  geom_boxplot( notch = FALSE,aes(fill = Diet)) +
  labs(x = "Diet", y = "weight") +
  ggtitle("Chicksweight") +
  theme()

chicks.aov1_g

TukeyHSD(chicks.aov1)

summary(aov(weight ~ Diet, data = filter(chicks, Time %in% c(0))))
summary(aov(weight ~ Diet, data = filter(chicks, Time %in% c(2))))
chicks.aov2 <- aov(weight ~ as.factor(Time), data = filter(chicks, Time %in% c(0, 2, 10, 21)))
summary(chicks.aov2)
