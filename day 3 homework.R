#homework for day 3
#practice how to read code,explain the 2 packages and convert lamanaria csv file to r base format
#Aadam Rawoot
library(tidyverse)
library(readr)
#Exercise 1
#lamanaria csv conversion

laminaria <- read_csv("data/laminaria.csv")
save(laminaria, file = "data/laminaria.RData")

#exercise 2-explaining packages scales and ggsn

#scales-Description Graphical scales map data to aesthetics, and provide methods for automatically determining breaks and labels for axes and legends.
#-package-scales is made up of Generic plot scaling methods.
#generic plot scaling methods include-parse_format Parse a text label to produce expressions for plotmath.
#The scales package is made up of the following interdependent components
#Palettes, pal for short, describe the useful palettes of aesthetics.
#Transformations, trans for short, describe common scale transformations, their inverses, and ways of generating breaks and labels.
#Bounds: various ways of rescaling the data#Scaling functions: pull together palettes, bounding functions and transformations to provide a complete pathway from raw data to perceptual properties
#Mutable ranges: in many graphics pathways, scale ranges can not be computed in a single pass, but must be computed over multiple groups or multiple panels. The mutable ranges (implemented with R's new reference based class) provide a thin layer of mutability to make this task easier.


#ggsn- North symbols and scale bars for maps created with 'ggplot' or 'ggmap'. 
#edits can be made to scale bar, fill colour, text colour.
#can change position and size of north symbol and scale bar.
#can create metric coordinates using ggplot
#Adds north symbols (18 options) and scale bars in kilometers to maps in geographic or metric coordinates created with 'ggplot' or 'ggmap'.


# [A.A]
# Good discriptions
# Highlights all important points
