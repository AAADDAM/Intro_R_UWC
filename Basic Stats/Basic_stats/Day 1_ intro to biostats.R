#Aadam Rawoot
#9 April 2019
#Basic biostats
#load libraries

library(tidyverse)
library(ggpubr)
library(lubridate)
library(dplyr)
library(readr)

chicks <- as_tibble(ChickWeight)
ncol(chicks)
nrow(chicks)
glimpse(chicks)


chicks %>%
  filter(Time == 0) %>%
  group_by(Diet) %>%
  summarise(mean_wt = mean(weight),
            sd_wt = sd(weight),
            max_wt = max(weight))
  
#the mean

nums <-  c(666,115,12,18,14,9)
mean(nums)

sum(nums)/ length(nums)

chicks %>% 
  summarise(mean_wt = mean(weight))

nums <-  c(666,115,12,18,14,9, NA)
mean(nums,na.rm = TRUE)

nums2 <- c(1, 2, 5, 6, 9, 13)
mean(nums2)
median(nums2)

quantile(chicks$weight)


chicks %>% 
  summarise(min_wt = min(weight),
            qrt1_wt = quantile(weight, p = 0.25),
            med_wt = median(weight),
            qrt3_wt = quantile(weight, p = 0.75),
            max_wt = max(weight))

range(chicks$weight)
chicks %>% 
  summarise(lower_wt = range(weight)[1],
            upper_wt = range(weight)[2])

summary(chicks$weight)

grp_stat <- chicks %>%
  filter(Time <12) %>% 
  group_by(Diet, Time) %>% 
  summarise(mean_wt = round(mean(weight, na.rm = TRUE), 2),
            med_wt = median(weight, na.rm = TRUE),
            sd_wt = round(sd(weight, na.rm = TRUE), 2),
            sum_wt = sum(weight),
            min_wt = min(weight),
            qrt1_wt = quantile(weight, p = 0.25),
            med_wt = median(weight),
            qrt3_wt = median(weight, p = 0.75),
            max_wt = max(weight),
            n_wt = n())
grp_stat

library(tidyverse)
library(ggpubr)
library(RColorBrewer)
library(ggthemes)

iris.cnt <- iris

iris.cnt <- iris %>%
  count(Species) %>% # automagically creates a column, n, with the counts
  mutate(prop = n / sum(n)) # creates the relative proportion of each species
iris.cnt



plt1 <- ggplot(data = iris.cnt, aes(x = "", y = n, fill = Species)) +
  geom_bar(width = 1, stat = "identity") +
  labs(title = "Stacked bar graph", subtitle = "cumulative sum",
       x = NULL, y = "Count") +
  theme_pubclean() + scale_color_few() +
  scale_fill_few()

plt2 <- ggplot(data = iris.cnt, aes(x = "", y = prop, fill = Species)) +
  geom_bar(width = 1, stat = "identity") +
  scale_y_continuous(breaks = c(0.00, 0.33, 0.66, 1.00)) +
  labs(title = "Stacked bar graph", subtitle = "relative proportions",
       x = NULL, y = "Proportion") +
  theme_pubclean() + scale_color_few() +
  scale_fill_few()

plt2

plt3 <- plt1 + coord_polar("y", start = 0) +
  labs(title = "Friends don't let...", subtitle = "...friends make pie charts",
       x = NULL, y = NULL) +
  scale_fill_brewer(palette = "Blues") +
  theme_minimal()

plt4 <- ggplot(data = iris, aes(x = Species, fill = Species)) +
  geom_bar(show.legend = FALSE) +
  labs(title = "Side-by-side bars", subtitle = "n per species", y = "Count") +
  theme_pubclean() + scale_color_few() +
  scale_fill_few()

ggarrange(plt1, plt2, plt3, plt4, nrow = 2, ncol = 2, labels = "AUTO")

hist1 <- ggplot(data = faithful, aes(x = eruptions)) +
  geom_histogram(colour = "black", fill = "salmon", alpha = 0.6) +
  labs(title = "Old Faithful data",
       subtitle = "A vanilla frequency histogram",
       x = "Eruption duration (min)",
       y = "Count") + theme_pubclean()

summary(faithful)

hist2 <- ggplot(data = faithful, aes(x = eruptions)) +
  geom_histogram(aes(y = ..density..),
                 position = 'identity', binwidth = 1,
                 colour = "black", fill = "salmon", alpha = 0.6) +
  labs(title = "Old Faithful data",
       subtitle = "Relative frequency histogram",
       x = "Eruption duration (min)",
       y = "Count") + theme_pubclean()


hist3 <- ggplot(data = faithful, aes(x = eruptions)) +
  geom_histogram(aes(y = 0.5 * ..density..),
                 position = 'identity', binwidth = 0.5,
                 colour = "black", fill = "salmon", alpha = 0.6) +
  labs(title = "Old Faithful data",
       subtitle = "Relative frequency histogram",
       x = "Eruption duration (min)",
       y = "Relative contribution") + theme_pubclean()

hist4 <- ggplot(data = faithful, aes(x = eruptions)) + 
  stat_ecdf() +
  labs(title = "Old Faithful data",
       subtitle = "ECDF",
       x = "Eruption duration (min)",
       y = "Relative contribution") + theme_pubclean()


ggarrange(hist1, hist2, hist3, hist4, ncol = 2, nrow = 2, labels = "AUTO")


iris.long <- iris %>% 
  gather(key = "variable", value = "size", -Species)

ggplot(data = iris.long, aes(x = size)) +
  geom_histogram(position = "dodge", # ommitting this creates a stacked histogram
                 colour = NA, bins = 20,
                 aes(fill = Species)) +
  facet_wrap(~variable) +
  labs(title = "Iris data",
       subtitle = "Grouped frequency histogram",
       x = "Size (mm)",
       y = "Count") +
  theme_pubclean()

summary(iris)

plt1 <- ggplot(data = iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_boxplot(show.legend = FALSE, notch = FALSE) + theme_pubclean() +
  labs(y = "Sepal length (mm)") +
  theme(axis.text.x = element_text(face = "italic"))

plt2 <- ggplot(data = iris.long, aes(x = Species, y = size)) +
  geom_boxplot(fill = "red", alpha = 0.4, notch = TRUE) +
  geom_jitter(width = 0.1, shape = 21, colour = "blue", fill = NA, alpha = 0.2) +
  facet_wrap(~variable, nrow = 1) +
  labs(y = "Size (mm)") + theme_pubclean() +
  theme(axis.text.x = element_text(face = "italic")) +
  theme(axis.ticks.length=unit(-0.25, "cm"), axis.ticks.margin=unit(0.5, "cm"))

ggarrange(plt1, plt2, nrow = 2, ncol = 1, labels = "AUTO")

plt1 <- ggplot(data = iris, aes(x = Petal.Length, y = Petal.Width, colour = Species)) +
  geom_point() +
  labs(x = "Petal length (mm)", y = "Petal width (mm)") +
  theme(legend.position = c(0.18, 0.85)) +
  scale_color_fivethirtyeight() +
  scale_fill_fivethirtyeight() + 
  theme_pubclean()

plt2 <- ggplot(data = iris, aes(x = Petal.Length, y = Petal.Width, colour = Species)) +
  geom_point(show.legend = FALSE) +
  geom_smooth(method = "lm", se = FALSE, show.legend = FALSE) +
  scale_color_fivethirtyeight() +
  scale_fill_fivethirtyeight() +
  labs(x = "Petal length (mm)", y = "Petal width (mm)") + 
  theme_pubclean()

