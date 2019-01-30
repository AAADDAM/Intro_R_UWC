#day1
#lamanaria
#Aadam Rawoot
#29 january 2019
#loading libraries
#tidyverse and lubridate

library(tidyverse)
lam <- library(readr)
laminaria <- read_csv("data/laminaria.csv")
View(laminaria)
lam <- read_csv("data/laminaria.csv")

head(lam)
head(lam, n = 3)
tail(lam, n = 3)

lam_select <- lam %>% 
  select(site, total_length) %>% 
  slice(54,88)

lam_kom <- lam %>%
  filter(site=="Kommetjie")


#in the lamanaria dataset, select only site and blade length column and filter only seapoint




lam_Seapoint <- lam %>% 
  select(site, blade_length) %>%
  filter(site == "Seapoint")

lam %>% 
  filter(total_length == max(total_length))


summary(lam)


lam %>% 
  summarise(avrg_bl = mean(blade_length),
            med_bl = median(blade_length),sd_bl = sd(blade_length))
#select lam data set then summarise it by giving it the name avrg b and then get the mean blade length

lam %>% 
  group_by(site) %>% 
  summarise(var_bl = var(blade_length),
            n = n()) %>% 
  mutate(se = sqrt(var_bl/n))
#select lam set, group by sites, and then summarise variance of blade lengths and number of entries, take variance divided by number of entries

lam2 <- lam %>%
  select(-blade_thickness, - blade_length)
#find only blade thickness and blade length

lam_count <- lam %>%
  select(stipe_mass) %>% 
  summarise(n = n())
#select lamanaria data set, select only stipe mass then summarise number of entries

lam %>%
  select(stipe_mass) %>% 
  na.omit
  summarise(n = n())
  
#select lam then choose stipe mass but exclude non applicable values then tell me how many entries I have

lam %>% 
  select(blade_length) %>%
  na.omit



ggplot(data = lam, aes(x = stipe_mass, y = stipe_length)) +
  geom_point(shape = 21, colour = "salmon", fill = "white") +
  labs(x = "Stipe mass (kg)", y = "Stipe length (cm)")


lamhalf <- lam %>%
  select(site,total_length) %>%
  mutate(total_length_half = total_length/2) %>%
  filter(total_length_half <= 100) %>%
  na.omit



lam_2 <- lam %>%
  group_by(blade_length) %>%
  summarise(mean_bl = mean(blade_length)
  
  

  
  
  
  
  