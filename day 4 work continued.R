#day 4 work continued
#1 feb 2019
#Aadam Rawoot

library(tidyverse)
library(lubridate)

#Chapter 11:Tidier R

load("data/SACTNmonthly_v4.0.RData")#LOAD DATA FROM RDATA FILE

SACTN <- SACTNmonthly_v4.0

# Remove the original
rm(SACTNmonthly_v4.0)#rm means remove


SACTN %>% 
  filter(site == "Amanzimtoti")#use SACTN and then filter by site in amanzimtoti

SACTN %>% 
  filter(site == "Pollock Beach", month(date) == 12 | month(date) == 1)#use SACTN, filter by  site Pollock beach, using month december or the january.
#use & if you want to use both

SACTN %>% 
  arrange(depth, temp)#arrage allows values to go from lowest to highest value

SACTN %>% 
  arrange(desc(temp))#desc-changes data from highest to lowest

SACTN %>% 
  filter(site == "Humewood", year(date) == 1990)#r filter site humewood, year by 1990

SACTN %>% 
  filter(site == "Humewood", year(date) == 1992)#r filter site humewood, year by 1992


try_1 <- SACTN %>% 
  select(site, src, date, temp)# Select columns individually by name


try_2 <- SACTN %>% 
  select(site:temp)# Select all columns between site and temp like a sequence, :-select from a point to a point

# Select all columns except those stated individually
try_3 <- SACTN %>% 
  select(-date, -depth)# Select all columns except those stated individually

try_4 <- SACTN %>% 
  select(-(date:depth))# select and remove date and depth, : exclude from a point to a point

try_5 <- SACTN %>% 
  mutate(kelvin = temp + 273.15)# fix bracket problem
#create a column using mutate, temp saved in kelvin = temp+237.15

try_6 <- SACTN %>% 
  mutate(kelvin = (temp +273.15/2))


try_7 <- SACTN %>% 
  summarise(mean_temp = mean(temp, na.rm = TRUE),#summarise, give a name to each first, mean of temp
            sd_temp = sd(temp, na.rm = TRUE),#standard dev of temp removing na.omits
            min_temp = min(temp, na.rm = TRUE),#minimum of temp
            max_temp = max(temp, na.rm = TRUE))#max of temp
#use na.rm when analysing a dataset
#can use "T" instead of True


#Chapter 12 Tidiest data



