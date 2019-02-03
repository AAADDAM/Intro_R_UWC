#day4
#1 feb 2019
#Aadam Rawoot
#Tidy data

library(tidyverse)
library(lubridate)

load("data/SACTN_mangled.RData")
#utr- underwater temp reader


ggplot(data= SANCTN1, aes(x = date, y = temp)) +
  geom_line(aes(colour = site, group = paste0(site, src))) +
  labs(X = "date", y = "temperature (°C)") +
  ggtitle("Dates and temperature of various readings using UTR at Port Nolloth")

ggplot(data = SACTN1, aes(x = date, y = temp)) +#aes specifies variables
  geom_line(aes(colour = site, group = paste0(site, src))) +#paste0 lets you group multiple variables
  labs(x = "dates", y = "Temperature (°C)", colour = "Site") +
  ggtitle("Dates and temperature of various readings using UTR at Port Nolloth")
  theme_bw()#correct code- sites by colour groups sites by temp, place and reader
  
  #tidyverse has gather,spread,unite etc functions
  
SACTN2_TIDY <- SACTN2 %>%
  gather(DEA, KZNSB, SAWS, key = "src", value = "temp")#gather function used to combine many columns into a single variable column

 
SACTN3_tidy <- SACTN3 %>% 
  spread(key = var, value = val) 


SACTN4a_tidy <- SACTN4a %>% 
  separate(col = index, into = c("site", "src"), sep = "/ ") #separate columns into 2

SACTN4b_tidy <- SACTN4b %>% 
  unite(year, month, day, col = "date", sep = "-")#unite used to combine month,day,column into date separate by hyphon

SACTN4_tidy <- left_join(SACTN4a_tidy, SACTN4b_tidy)#left join function detects similar words, R will group by site, soure and date


# [A.A]
# More comments could be added
# EXplain more
# Script runs complete


