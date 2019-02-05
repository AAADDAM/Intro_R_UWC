#Aadam Rawoot
#day 4 work
#1 feb 2019
#load libraries

# Section 1: 
# Make use of the rast_feb and rast_aug dataset:
# Explore the dataset (Hint* head, tail, glimpse etc) - Make use of google for more functions on exploring a dataset
# Create a map by making use of the lat and long variables
# Create a colour pallete using the link in the document and make use this colour pallete on the map
# Add complete labels and titles to the map
# Add the name of the oceans (Atlanic and indian ocean) on the map, increase the size of the labels
# The map should include the north arrow and scale bar
# Bonus marks for inserting (having a smaller map inside another map)
# Get creative, try new things.

library(tidyverse)#loading packages in r with functions
library(lubridate)
library(dplyr)
library(ggpubr)

#save script
#import r datasets

feb_data <- rast_feb#fwb_data is used along with ' <- ' to assign a name to rast_feb to save it in the environment
rm(rast_feb)#rm used to remove rast_feb dataset

head(feb_data) #r auto does 6 rows
head(feb_data, n = 5)#head lets you see top 5 rows, n is number of rows
tail(feb_data, n = 5)#tails lets you see bottom 5 rows

feb_data_select <- feb_data %>% #pipe function, created by shift, control and M
  select(-lon, -lat) #select function uses data selected, while select- removes data from dataset 
 
feb_data_select <- feb_data %>% 
  select(lon, lat) %>%
  filter(lon >= "17")#filter extracts info, '>=' means greater or equal to

feb_data %>%
  summarise(avg_temp = mean(temp))#summarise used to create a summary of just the data selected
  
feb_data %>% # Select 'feb data'
  group_by(bins) %>% # Group the dataframe by bins
  summarise(var_temp = var(temp), # Calculate variance
            n_temp = n()) %>%  # Count number of values
  mutate(se_temp = sqrt(var_temp / n_temp)) # Calculate standard error of temp

library(scales)
library(ggsn)



load("data/south_africa_coast.RData")#r format data, how to load it
load("data/sa_provinces.RData")
load("data/rast_annual.RData")
load("data/MUR.RData")
load("data/MUR_low_res.RData")

cols11 <- c("cyan", "cyan1", "cyan2", "cyan3", "cyan4",
            "darkblue", "darkcyan", "blue2", "blue3", "blue4")#concatenate colours for palette



sst <- MUR#SUBSURFACE TEMP

feb_map_final <-ggplot(data = feb_data, aes(x = lon, y = lat)) +#ggplot creats a map, aes used (aesthetics)
  geom_raster(data = feb_data, aes(fill = bins)) +# The ocean temperatures colour differentiation
  #gives black outline, fills it with grey
  geom_path(data = sa_provinces, aes(group = group)) +
  scale_fill_manual("Temp. (°C)", values = cols11) + # Set the colour palette
  coord_equal(xlim = c(14, 35), ylim = c(-37, -27), expand = 0) +#coord_equal-xlim and ylim gives limits
  labs(x = "longitude", y = "latittude") +#labs renames axis
  ggtitle("Temperature variations across the South African coast during February") +#ggtitle gives title
  annotate("text", label = "Atlantic\nOcean", #adding titles for the ocean
           x = 17.0, y = -32.0, #position of ocean title
           size = 5.5, #font size of ocean title
           angle = 35, #angle of ocean title
           colour = "navy") +#colour of ocean title
  annotate("text", label = "Indian\nOcean", 
           x = 31.0, y = -34.0, 
           size = 5.5, 
           angle = 335, 
           colour = "black") +
  scalebar(x.min = 20, x.max = 24, y.min = -36, y.max = -35, #location of scale bar
           dist = 200, height = 0.5, st.dist = 0.8, st.size = 4, #dimensions
           dd2km = TRUE, model = "WGS84") + #bar image
  north(x.min = 16.5, x.max = 18.5, y.min = -35, y.max = -33, #location of symbol
        scale = 1.5, symbol = 16)   


inset_map <- feb_map_final +#inset map placed within southern africa map
  annotation_custom(grob = ggplotGrob(africa_map),#grob used so that ggplot can read the data frame being inset
                    xmin = 20.0, xmax = 24.0,
                    ymin = -32.5, ymax = -30)#positioning on inset map

rm(feb_map)#remove feb_map


  aug_data <- rast_aug#aug_data is used along with ' <- ' to assign a name to rast_feb to save it in the environment
  rm(rast_aug)#rm used to remove rast_aug dataset
  
  head(aug_data) #r auto does 6 rows
  head(aug_data, n = 9)#head lets you see top 9 rows, n is number of rows
  tail(aug_data, n = 4)#tails lets you see bottom 4 rows
  
  aug_data_select <- aug_data %>% #pipe function, created by shift, control and M
    select(-temp, -bins) #select function uses data selected, while select- removes data from dataset 
  
  aug_data_select <- aug_data %>% 
    select(temp, bins) %>%
    filter(temp <= "15")#filter extracts info, '<=' means greater or equal to
  
  aug_data %>%
    summarise(avg_temp = mean(temp))#summarise used to create a summary of just the data selected
  
  aug_data %>% # Select 'aug data'
    group_by(bins) %>% # Group the dataframe by bins
    summarise(var_temp = var(temp), # Calculate variance
              n_temp = n()) %>%  # Count number of values
    mutate(se_temp = sqrt(var_temp / n_temp)) # Calculate standard error of temp
  
  
  cols12 <- c("springgreen", "springgreen1", "springgreen2", "springgreen3", "springgreen4",
              "red4", "yellow1", "red3", "red2", "red1")#concatenate colours for palette
  
  
  
  sst <- MUR#SUBSURFACE TEMP
  
  aug_map_final <-ggplot(data = aug_data, aes(x = lon, y = lat)) +#ggplot creats a map, aes used (aesthetics)
    geom_raster(data = aug_data, aes(fill = bins)) +# The ocean temperatures colour differentiation
    #gives black outline, fills it with white
    geom_path(data = sa_provinces, aes(group = group)) +#creates path to follow or connect
    scale_fill_manual("Temp. (°C)", values = cols12) + # Set the colour palette
    coord_equal(xlim = c(14, 35), ylim = c(-37, -27), expand = 0) +#coord_equal-xlim and ylim gives limits
    labs(x = "longitude", y = "latittude") +#labs renames axis
    ggtitle("Temperature variations across the South African coast during August") +#ggtitle gives title
    annotate("text", label = "Atlantic\nOcean", #adding titles for the ocean
             x = 17.0, y = -32.0, #position of ocean title
             size = 5.5, #font size of ocean title
             angle = 35, #angle of ocean title
             colour = "orangered2") +#colour of ocean title
    annotate("text", label = "Indian\nOcean", 
             x = 31.0, y = -34.0, 
             size = 5.5, 
             angle = 335, 
             colour = "magenta2") +
    scalebar(x.min = 20, x.max = 24, y.min = -36, y.max = -35, #location of scale bar
             dist = 200, height = 0.5, st.dist = 0.8, st.size = 4, #dimensions
             dd2km = TRUE, model = "WGS84") + #bar image
    north(x.min = 16.5, x.max = 18.5, y.min = -35, y.max = -33, #location of symbol
          scale = 1.5, symbol = 16)   
  
  
  inset_map2 <- aug_map_final +
    annotation_custom(grob = ggplotGrob(africa_map),
                      xmin = 20.0, xmax = 24.0,
                      ymin = -32.5, ymax = -30)
  
#end of section 1!  