#Chapter 12 Tidiest data
#day 4 continuedx2
#1 feb 2019
#Aadam Rawoot

library(tidyverse)
library(lubridate)

# load the data from a .RData file
load("data/SACTNmonthly_v4.0.RData")

# Copy the data as a dataframe with a shorter name
SACTN <- SACTNmonthly_v4.0

rm(SACTNmonthly_v4.0)#remove so environment is cleaner


SACTN_depth_mean <- SACTN_depth %>% 
  group_by(depth) %>% # Group by depth
  summarise(mean_temp = mean(temp, na.rm = TRUE),# Calculate mean temp by depth
            count = n())

SACTN_depth_mean


ggplot(data = SACTN_depth_mean, mapping = aes(x = depth, y = mean_temp)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = FALSE) +
  ggtitle("Mean temperature in relation to the depth of the reading") +#adding a title
  labs(x = "depth of reading (m)", y = "mean temperature (°C)")#adding labels to x and y  axis

SACTN_30_years <- SACTN %>%#renamed
  group_by(site, src) %>%#grouped by sites and sources
  filter(n() > 360)#filtered where n is greater 360 months = 30years

selected_sites <- c("Paternoster", "Oudekraal", "Muizenberg", "Humewood")#concatenate, creates a set of number

SACTN %>% 
  filter(site %in% selected_sites) %>%#concatenate function used in selected sites, extract  the info from those sites
  group_by(site, src) %>% #grouped by site and source
  summarise(mean_temp = mean(temp, na.rm = TRUE), #summarise to get mean and stdv of temp
            sd_temp = sd(temp, na.rm = TRUE))
