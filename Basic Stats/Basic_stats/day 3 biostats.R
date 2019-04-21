#day 3 biostats
#Aadam Rawoot
#16 April 2019

library(fitdistrplus)
library(logspline)
library(tidyverse)
library(ggpubr)
library(lubridate)
library(dplyr)
library(readr)


library(readr)
ecklonia <- read_csv("Basic Stats/Basic_stats/ecklonia.csv")
View(ecklonia)

head(faithful)

eruption.lm <- lm(eruptions ~ waiting, data = faithful)
summary(eruption.lm)

erupt<-datasets::faithful


ggplot(erupt, aes(x = waiting, y = eruptions, colour = waiting)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "wainting in minutes", y = "eruption time in minutes") +
  ggtitle("Old Faithful Geyser Data") +
  theme_bw()


slope <- round(eruption.lm$coef[2], 3)
# p.val <- round(coefficients(summary(eruption.lm))[2, 4], 3) # it approx. 0, so...
p.val = 0.001
r2 <- round(summary(eruption.lm)$r.squared, 3)


ggplot(data = faithful, aes(x = waiting, y = eruptions)) +
  geom_point() +
  annotate("text", x = 45, y = 5, label = paste0("slope == ", slope, "~(min/min)"), parse = TRUE, hjust = 0) +
  annotate("text", x = 45, y = 4.75, label = paste0("italic(p) < ", p.val), parse = TRUE, hjust = 0) +
  annotate("text", x = 45, y = 4.5, label = paste0("italic(r)^2 == ", r2), parse = TRUE, hjust = 0) +
  stat_smooth(method = "lm", colour = "salmon") +
  labs(title = "Old Faithful eruption data",
       subtitle = "Linear regression",
       x = "Waiting time (minutes)",
       y = "Eruption duration (minutes)")


summary(eruption.lm)$r.squared


#Chapter 9- Correlations

library(readr)
ecklonia <- read_csv("Basic Stats/Basic_stats/ecklonia.csv")
View(ecklonia)

# Load libraries
library(tidyverse)
library(ggpubr)
library(corrplot)

cor.test(x = ecklonia$stipe_length, ecklonia$frond_length,
         use = "everything", method = "pearson")
#closer to 1  means strong correlation

# Create ordinal data
ecklonia$length <- as.numeric(cut((ecklonia$stipe_length+ecklonia$frond_length), breaks = 3))

# Run test on any variable
cor.test(ecklonia$length, ecklonia$digits)

ecklonia_sub <- ecklonia %>% 
  select(-species, - site, - ID)

cor.test(x = ecklonia$stipe_length, ecklonia$frond_length,
         use = "everything", method = "pearson")

ecklonia_pearson <- cor(ecklonia_sub)
ecklonia_pearson

# Create ordinal data
ecklonia$length <- as.numeric(cut((ecklonia$stipe_length+ecklonia$frond_length), breaks = 3))

# Run test on any variable
cor.test(ecklonia$length, ecklonia$digits)

ecklonia_norm <- ecklonia_sub %>% 
  gather(key = "variable") %>% 
  group_by(variable) %>% 
  summarise(variable_norm = as.numeric(shapiro.test(value)[2]))
ecklonia_norm

cor.test(ecklonia$primary_blade_length, ecklonia$primary_blade_width, method = "kendall")

# Calculate Pearson r beforehand for plotting
r_print <- paste0("r = ", 
                  round(cor(x = ecklonia$stipe_length, ecklonia$frond_length),2))

# Then create a single panel showing one correlation
ggplot(data = ecklonia, aes(x = stipe_length, y = frond_length)) +
  geom_smooth(method = "lm", colour = "grey90", se = F) +
  geom_point(colour = "mediumorchid4") +
  geom_label(x = 300, y = 240, label = r_print) +
  labs(x = "Stipe length (cm)", y = "Frond length (cm)") +
  theme_pubclean()

corrplot(ecklonia_pearson, method = "circle")

# Script runs
# Neat script
# But the same applies here as in script one.
# Add comments
# Describe what you are doing
# State the hypothesis and why you are doing what you are doing with a specific code

