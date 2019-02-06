#5feb 2019
#Aadam Rawoot
#recreating data plots

#load libraries
library(tidyverse)
library(lubridate)
library(ggpubr)
library(dbplyr)

#load data SACTNMONTHLY_V4.0

SACTN_GRAPH <- SACTNmonthly_v4.0#rename and save to environment

kznsb.temp <- SACTN_GRAPH#rename 

first <- SAC %>% 
  mutate(year = format(date, "%Y")) %>% #add column
  group_by(site, year) %>% #group site and year
  summarise(average_temp = mean(temp, na.rm = TRUE))#summarise and get ave temp


kznsb.temp %>% 
  filter(src == "KZNSB")#extract by source

SAC <- kznsb.temp %>% 
  filter(src == "KZNSB")

ggplot(first, aes(x = year, y = average_temp)) +#ggplot used to plot graph, aes used to plot a and y values
  geom_line(aes(group = site, colour = "red")) +#line that plots data
  facet_wrap(~site, ncol = 5) +#facet wrap used to show multiple graphs that were on one plane on many planes
  labs(x = "Year", y = "Temperature (C)") +#labs used to rename x and y axis
  scale_y_continuous(breaks = seq(20, 24, 2)) +#used for assigning range of values to y axis
  scale_x_discrete(breaks = seq(1980, 2014, 20))#used for assigning range of values to x axis


final_graph <- ggplot(first, aes(x = year, y = average_temp)) +
  geom_line(aes(group = site, colour = "red")) +
  facet_wrap(~site, ncol = 5) +#ncol is column number
  labs(x = "Year", y = "Temperature (C)") +
  scale_y_continuous(breaks = seq(20, 24, 2)) +#includes lower limit, upper limit and gap
  scale_x_discrete(breaks = seq(1980, 2014, 20)) +#includes lower limit, upper limit and gap
  ggtitle("KZNSB:series of annual means")#adds title to graphs



ggsave(final_graph, filename = "final_graph.png")#saves image



laminaria <- read_csv("data/laminaria.csv")#load Laminaria data




falsebay <- laminaria %>% #renamed falsebay and then
  filter(region == "FB") #filtered by region renamed


plot_1 <- ggplot(falsebay, aes(x = blade_length, y = blade_weight)) +#plot1 assigned, ggplot maps graphs, falsebay data used, aes used to plot x and y values
  geom_line(aes(colour = site), size = 1) +#geometric line created that joins data points, aes selects colour by site and size
  geom_point(aes(colour = site), size = 4) +#geometic points plotted
  facet_wrap(~site, ncol = 3) +#used to plot many graphs on many plots but one plane
  labs(x = "Blade length (cm)", y = "Blade mass (kg)") +#labels x and y axis
  scale_colour_brewer(palette = "Accent") +#scale colour given for palette, accent makes data points invisible
  ggtitle("A crazy graph of some data for False Bay sites")#name assigned

plot2 <- ggplot(falsebay, aes(x = blade_length, y = blade_weight)) +#plot2 assigned, ggplot maps graphs, falsebay data used, aes used to plot x and y values
  geom_line(aes(colour = site), size = 1) +#geometric line created that joins data points, aes selects colour by site and size
  geom_point(aes(colour = site), size = 4) +#geometic points plotted
  facet_wrap(~site, ncol = 3) +#used to plot many graphs on many plots but one plane
  labs(x = "Blade length (cm)", y = "Blade mass (kg)") +#labels x and y axis
  scale_colour_brewer(palette = "Set1") +#scale colour given for palette
  ggtitle("A crazy graph of some data for False Bay sites")#name assigned

finalplot <- ggarrange(plot_1, plot2,#final plot created by arranging plot 1 and 2.
                       ncol = 2,#2 columns created
                       labels = c("A", "B"),#labeled a and b
                       common.legend = FALSE)#legend assigned


ggsave(finalplot, filename = "finalplot.png")#save images

#load toothgrowth data

tooth <- datasets::ToothGrowth
datasets::ToothGrowth#loaded dataset toothgrowth

mn.sd <- ToothGrowth%>% #renamed and added to environment as mn.sd then piped
  group_by(supp, dose) %>%#group by used to group variables and then
  summarise(mn.ln = mean(len),#summarised average length and stdv length
            sd.ln = sd(len))


teeth_plot <- ToothGrowth#renamed teeth_plot

teeth_plot <- ggplot(mn.sd, aes(x = dose, y = mn.ln, fill = supp)) +#ggplot mn.sd data used
  geom_col(aes(fill = supp), position = "dodge", colour = "black") + #geom_col used to create columns
  geom_errorbar(aes(ymin = mn.ln - sd.ln,#geom_errorbar creates error bar
                    ymax = mn.ln + sd.ln), 
                position = "dodge") +
  labs(x = "Dose (mg/d)", y = "Tooth length (mm)") +#labs used to rename x and y axis
  ggtitle("Dosage of supplements in relation to tooth growth")#creates title



ggsave(teeth_plot, filename = "teethplot.png")##saves image

