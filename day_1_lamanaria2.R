#day1
#lamanaria
#Aadam Rawoot
#29 january 2019
#loading libraries
#tidyverse and lubridate

library(tidyverse)
lam <- library(readr) # [A.A] what are you doing here?
laminaria <- read_csv("data/laminaria.csv")
View(laminaria)
lam <- read_csv("data/laminaria.csv")

# Here you should add comments describing what you are doing [A.A]
head(lam)
head(lam, n = 3)
tail(lam, n = 3)

lam_select <- lam %>% 
  select(site, total_length) %>% 
  slice(54,88) # The slice function should read slice(54:88) [A.A]

lam_kom <- lam %>%
  filter(site=="Kommetjie")


#in the lamanaria dataset, select only site and blade length column and filter only seapoint




lam_Seapoint <- lam %>% 
  select(site, blade_length) %>%
  filter(site == "Seapoint") # Here are no values in this dataset because the site is spelled wrong, it should be Sea Point [A.A]

lam %>% 
  filter(total_length == max(total_length))


summary(lam) # Describe what each of the functions are [A.A]


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
  summarise(n = n()) # This bit of code does not run, Simply because there is no pipe between line 66 and 67 [A.A]
   and 
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
  summarise(mean_bl = mean(blade_length),
  min_bl = min(blade_length),
  max_bl = max(blade_length),
  n = n())

library(tidyverse)
  
#exercise 1.3
  
lam %>%
  group_by(site) %>% 
  filter(stipe_mass == max(stipe_mass)) %>% 
  select(site, region, stipe_length)
#work in lam 
#grouping by site
#filter to take info from a specific column
#select is to choose specific columns you work in
  
  