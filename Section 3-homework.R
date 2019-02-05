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

 
#script notes Tidy data
#load("data/SACTN_mangled.RData")#loading in an r data file

load("data/SACTN_mangled.RData")#load r data

ggplot(data = SACTN1, aes(x = date, y = temp)) +#ggplot used to plot, data coded in, aes used to plot x and y values
  geom_line(aes(colour = site, group = paste0(site, src))) +#geom line creates a line, paste0 groups multiple variables
  labs(x = "", y = "Temperature (°C)", colour = "Site") +#labs renames x and y axis
  theme_bw()#creates theme

SACTN2_tidy <- SACTN2 %>%
  gather(DEA, KZNSB, SAWS, key = "src", value = "temp")#gather function used to combine many columns into a single variable column

SACTN3_tidy <- SACTN3 %>% 
  spread(key = var, value = val)#data be too long, meaning when observations are spread across multiple rows, we will need to use spread() to fix it.

SACTN4a_tidy <- SACTN4a %>% 
  separate(col = index, into = c("site", "src"), sep = "/ ")#separates data from one column

SACTN4b_tidy <- SACTN4b %>% 
  unite(year, month, day, col = "date", sep = "-")#unites data in many columns into 1

SACTN4_tidy <- left_join(SACTN4a_tidy, SACTN4b_tidy)

SACTN4_tidy <- left_join(SACTN4a_tidy, SACTN4b_tidy, by = c("site", "src", "date"))#joins data frames

#five primary data transformation functions
#Arrange observations with arrange()
#Filter observations with filter()
#Select variables with select()
#Create new variables with mutate()
#Summarise variables with summarise()

# Load libraries
library(tidyverse)
library(lubridate)

# Load the data from a .RData file
load("data/SACTNmonthly_v4.0.RData")

# Copy the data as a dataframe with a shorter name
SACTN <- SACTNmonthly_v4.0

# Remove the original
rm(SACTNmonthly_v4.0)

#Comparison operators

#Greater than: >
 # Greater than or equal to: >=
  #Less than: <
  #Less than or equal to: <=
  #Equal to: ==
  #Not equal to: !=

#Logical operators

#and: &
#or: |
#not: !

SACTN %>% 
  filter(site == "Pollock Beach", month(date) == 12 | month(date) == 1)#extract place,december or january

SACTN %>% 
  arrange(depth, temp)#arrange rows with arrange

SACTN %>% 
  arrange(desc(temp))#arrange in descending order

SACTN %>% 
  filter(site == "Humewood", year(date) == 1990)#filter by site and year

humewood_90s <- SACTN %>% 
  filter(site == "Humewood", year(date) %in% seq(1990, 1999, 1))#filter humewood by year sequentially from 1990 onwards

SACTN %>% 
  filter(site == "Port Nolloth", #site to filter
         src == "DEA", #source
         temp <= 11 | #Temps at or below 11°C OR
           is.na(temp)) #Include missing values


SACTN %>% 
  select(site, src, date, temp)# Select columns individually by name


SACTN %>% 
  select(site:temp)#Select all columns between site and temp from one varible up until another variable


SACTN %>% 
  select(-date, -depth)#select all columns except those stated individually, remove

SACTN %>% 
  select(-(date:depth))#Select all columns except those within a given sequence
# '-' goes outside of a new set of brackets

SACTN %>% 
  select(temp, src, date, site)#change up order

SACTN %>% 
  select(type, src, everything())#use everything to take all columns not already selected

SACTN %>% 
  select(temp:type, everything(), -src)#selecting, order, from one variable to another, removing

SACTN %>% 
  mutate(kelvin = temp + 273.15)#creates new variable columns

SACTN %>% 
  summarise(mean_temp = mean(temp, na.rm = TRUE))#summarise data, na's removed

#tidiest data

# load the data from a .RData file
load("data/SACTNmonthly_v4.0.RData")

# Copy the data as a dataframe with a shorter name
SACTN <- SACTNmonthly_v4.0

# Remove the original
rm(SACTNmonthly_v4.0)


SACTN_depth <- SACTN %>% 
  group_by(depth)#Group by depth

SACTN_depth_mean

SACTN_depth_mean <- SACTN_depth %>% 
  summarise(mean_temp = mean(temp, na.rm = TRUE),
            count = n())#summarise mean temp by depth

ggplot(data = SACTN_depth_mean, mapping = aes(x = depth, y = mean_temp)) +
  geom_point(aes(size = count), alpha = 1/3) +#alpha gives width
  geom_smooth(se = FALSE)#geom smooth gives best fit line

SACTN_temp_group <- SACTN %>% 
  group_by(round(temp), depth)

SACTN_src_group <- SACTN %>% 
  group_by(src, date)#create groupings based on source and date


SACTN_date_group <- SACTN %>% 
  group_by(date, depth)#create groupings based on date and depth

SACTN_ungroup <- SACTN_date_group %>% 
  ungroup()#ungroups data

SACTN_depth_mean_2 <- SACTN %>% #choose a base dataframe
  group_by(depth) %>% #group by the depth column
  summarise(mean_temp = mean(temp, na.rm = TRUE), #calculate means
            count = n()) #count observations 

SACTN_30_years <- SACTN %>%
  group_by(site, src) %>%
  filter(n() > 360)#filter by bigger than 360 months

SACTN_anom <- SACTN %>%
  group_by(site, src) %>% #groups variables
  mutate(anom = temp - mean(temp, na.rm = T)) %>% #adds column
  select(site:date, anom, depth, type) %>% #selects data
  ungroup()#ungroups data

SACTN %>% 
  filter(site == "Paternoster" | site == "Oudekraal") %>%#filter by site
  group_by(site, src) %>% 
  summarise(mean_temp = mean(temp, na.rm = TRUE), 
            sd_temp = sd(temp, na.rm = TRUE))

SACTN %>% 
  filter(site == "Paternoster" | "Oudekraal") %>% # This line has been changed/shortened using "divison"
  group_by(site, src) %>% 
  summarise(mean_temp = mean(temp, na.rm = TRUE), 
            sd_temp = sd(temp, na.rm = TRUE))

SACTN %>% 
  filter(site == "Port Nolloth", temp > 10, temp < 15)#shows temp bigger than 10 but less than 15

SACTN %>% # Choose starting dataframe
  filter(site %in% c("Bordjies", "Tsitsikamma", "Humewood", "Durban")) %>% #select sites
  select(-depth, -type) %>% #remove depth and type columns
  mutate(month = month(date), #create month column
         index = paste(site, src, sep = "/ ")) %>% #create individual site column
  group_by(index, month) %>% #group by individual sites and months
  summarise(mean_temp = mean(temp, na.rm = TRUE), #calculate mean temp
            sd_temp = sd(temp, na.rm = TRUE)) %>% #calculate stdv
  ggplot(aes(x = month, y = mean_temp)) + #ggplot, switch from '%>%' to '+'
  geom_ribbon(aes(ymin = mean_temp - sd_temp, ymax = mean_temp + sd_temp), 
              fill = "black", alpha = 0.4) + #make a ribbon
  geom_line(col = "red", size = 0.3) + # =make lines within ribbon
  facet_wrap(~index) + #facet by individual sites
  scale_x_continuous(breaks = seq(2, 12, 4)) + #control x axis ticks
  labs(x = "Month", y = "Temperature (°C)") + #Change labels
  theme_dark() # Set theme

SACTN %>% 
  rename(source = src)#rename variable column

SACTN %>% 
  transmute(kelvin = temp + 273.15)#transmute adds new column but with different dataframe

SACTN_n <- SACTN %>% 
  group_by(site, src) %>% 
  summarise(mean_temp = round(mean(temp, na.rm = T))) %>% 
  arrange(mean_temp) %>% 
  ungroup() %>% 
  select(mean_temp) %>% 
  unique()#unique chooses one of a kind variables


SACTN %>% 
  slice(10010:10020)#slices a seqeunce of rows


SACTN %>%
  slice(c(1,8,19,24,3,400))#slices specific rows


SACTN %>% 
  slice(-(c(1,8,4)))#slices all rows except these


SACTN %>% 
  slice(-(1:1000))#slices all rows except a sequence




#end of section 3