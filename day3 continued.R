#day 3 continued-chapter8
#31 jan 2019
#Aadam Rawoot
#Mapping with style

library(tidyverse)
library(scales)
library(ggsn)

load("data/africa_map.RData")


#homework- practice how to read code,explain the 2 packages

ggplot() +
  borders() + # The global shape file- creates outline
  coord_equal() # Equal sizing for lon/lat 

sa_1 <- ggplot() +
  borders(fill = "grey70", colour = "black") +
  coord_equal(xlim = c(12, 36), ylim = c(-38, -22), expand = 0) # Force lon/lat extent
sa_1

sa_2 <- sa_1 +
  annotate("text", label = "Atlantic\nOcean", 
           x = 15.1, y = -32.0, 
           size = 5.0, 
           angle = 30, 
           colour = "red") +
  annotate("text", label = "Indian/nOcean", 
           x = 33.2, y = -34.2, 
           size = 5.0, 
           angle = 330, 
           colour = "black")
sa_2

sa_3 <- sa_2 +
  scalebar(x.min = 22, x.max = 26, y.min = -36, y.max = -35, # Set location of bar
           dist = 200, height = 1, st.dist = 0.8, st.size = 4, # Set particulars
           dd2km = TRUE, model = "WGS84") + # Set appearance
  north(x.min = 22.5, x.max = 25.5, y.min = -33, y.max = -31, # Set location of symbol
        scale = 1.2, symbol = 16)
sa_3

sa_4 <- sa_3 +
  annotation_custom(grob = ggplotGrob(africa_map),
                    xmin = 20.9, xmax = 26.9,
                    ymin = -30, ymax = -24)
sa_4

# [A.A]
# self made comments will only benifit you and increase your marks
# Try new things and explore the different functions in R




