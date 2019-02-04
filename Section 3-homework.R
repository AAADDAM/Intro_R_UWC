#section 3
#2 feb 2019
#Aadam Rawoot

# Section 3: 
# Make use of the SACTN_day1 data:
# Here create a graph showing temperature variation between sites
# Select all the temperatures recorded at the site Port Nolloth during August or September.
# Select all the monthly temperatures recorded in Port Nolloth during the year 1994
# Calculate the average temperature by depth
# Work through the tidyverse section within the document. Show what you have done by creating comments/ notes throughout the script

#load libraries
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggpubr)
library(readr)

SACTN_day_1 <- read_csv("data/SACTN_day_1.csv")#read in SACTN data using read_csv
SACTN_d <- SACTN_day_1

SACTN_tidy <- SACTN_d %>% 
  select(-index)


SACTN_tidyg <- ggplot(data = SACTN_tidy, aes(x = site, y = temp)) +#aes specifies variables
  geom_line(aes(colour = site, group = paste0(site, src))) +#paste0 lets you group multiple variables
  labs(x = "Sites", y = "Temperature (°C)", colour = "Site") +
  ggtitle("Temperature variations across various sites")
theme_bw()#correct code- sites by colour groups sites by temp, place and reader

SACTN_tidy2 <- SACTN_tidy %>% 
  filter(site == "Port Nolloth", month(date) == 8 | month(date) == 9, temp) %>%#use SACTN, filter by  site Port Nolloth, using month august or the september.
  select(-src)

SACTN_tidy3 <- SACTN_tidy %>% 
  filter(site == "Port Nolloth", year(date) == 2016, temp) %>% #r filter site Port Nolloth, year by 2016
  select(-src)

SACTN_tidy4 <- SACTN_tidy %>% 
  summarise(mean_temp = mean(temp, na.rm = TRUE))#summarise, give a name to each first, mean of temp

 
#script notes
#load("data/SACTN_mangled.RData")#loading in an r data file

ggplot(data = SACTN1, aes(x = date, y = temp)) +#ggplot creates a graph/map, data loaded from SACTN, aes is asthetics used for variables being plotted
  geom_line(aes(colour = site, group = paste0(site, src))) +#plus is used when plotting, paste0 lets you group multiple variables, geom_line creates a geometric line that plots data
  labs(x = "", y = "Temperature (°C)", colour = "Site") +#labs allows you to rename axis
  theme_bw()#theme gives your graph a theme

SACTN2_TIDY <- SACTN2 %>%#pipe funtion used when coding
  gather(DEA, KZNSB, SAWS, key = "src", value = "temp")#gather function used to combine many columns into a single variable column

SACTN3_tidy <- SACTN3 %>% 
  spread(key = var, value = val)#spread function lets you fix the problem of data being spread across too many rows

SACTN4a_tidy <- SACTN4a %>% 
  separate(col = index, into = c("site", "src"), sep = "/ ")#seperate lets you separate data in one coulmn into 2

SACTN4b_tidy <- SACTN4b %>% 
  unite(year, month, day, col = "date", sep = "-")#unite lets you bring various data entries into one column

SACTN4_tidy <- left_join(SACTN4a_tidy, SACTN4b_tidy)
#R> Joining by = c("site", "src", "date")#left join function detects similar words, R will group by site, source and date

#Arrange observations (rows) with arrange()
#Filter observations (rows) with filter()
#Select variables (columns) withselect()
#Create new variables (columns) with mutate()
#Summarise variables (columns) with summarise()

library(tidyverse)#load these packages
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

#assignment operators
#Greater than: >
#Greater than or equal to: >=
#Less than: <
#Less than or equal to: <=
#Equal to: ==
#Not equal to: !=

#tidiest data

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

SACTN %>% 
  filter(site == "Port Nolloth", temp > 10, temp < 15)#extract data, temp bigger than 10 but smaller than 15

SACTN %>% 
  filter(site == "Port Nolloth", !(temp <= 10 | temp  >= 15))#== means equal, temp less or equal to 10 or temp bigger and equal to 15


SACTN %>% #Choose dataframe
  filter(site %in% c("Bordjies", "Tsitsikamma", "Humewood", "Durban")) %>% #select sites
  select(-depth, -type) %>% #Remove depth and type columns
  mutate(month = month(date), #add month column called month
         index = paste(site, src, sep = "/ ")) %>% #make individual site column using "/" to separate data
  group_by(index, month) %>% #group by sites and months
  summarise(mean_temp = mean(temp, na.rm = TRUE), #calculate mean temp, na removed
            sd_temp = sd(temp, na.rm = TRUE)) %>% #calculate stdv
  ggplot(aes(x = month, y = mean_temp)) + #ggplot, change from '%>%' to '+'- pipe to plus
  geom_ribbon(aes(ymin = mean_temp - sd_temp, ymax = mean_temp + sd_temp),#create a ribbon 
              fill = "black", alpha = 0.4) + 
  geom_line(col = "red", size = 0.3) + #create lines within ribbon
  facet_wrap(~index) + #facet by individual sites
  scale_x_continuous(breaks = seq(2, 12, 4)) + #control x axis ticks
  labs(x = "Month", y = "Temperature (°C)") + #change labels
  theme_dark() #set theme dark

SACTN %>% 
  transmute(kelvin = temp + 273.15)#transmute, used to change dataframe

SACTN_n <- SACTN %>% 
  group_by(site, src) %>% #groups data by site and source
  summarise(mean_temp = round(mean(temp, na.rm = T))) %>% #summarises, gets mean temp,
  arrange(mean_temp) %>%#arranges data
  ungroup() %>% #ungroups data in columns
  select(mean_temp) %>% #selects data
  unique()#returns a dataframe with duplicates removed


SACTN %>%
  slice(c(1,8,19,24,3,400))# Slice specific rows, c is concatenate- creates a string of data

# The top 5 variable sites as measured by SD
SACTN %>% 
  group_by(site, src) %>% 
  summarise(sd_temp = sd(temp, na.rm = T)) %>% 
  ungroup() %>% 
  arrange(desc(sd_temp)) %>% #arranges in descending order
  slice(1:5)#slices from values 1 to 5

#end of section 3