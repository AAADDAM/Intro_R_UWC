#mapping with google
#afternoon session
#Aadam Rawoot
#31 Jan 2019

library(tidyverse)
library(ggsn)
library(scales)
library(ggmap)
library(readr)

#Load data
load("data/cape_point_sites.RData")

cape_point <- get_map(location = c(lon = 18.36519, lat = -34.2352581),
                      zoom = 10, maptype = 'satellite')
# load("data/cape_point.RData")

#telling R to gets maps from google, want to get map from satellite for that area(cape point)

??`ggsn-package`
