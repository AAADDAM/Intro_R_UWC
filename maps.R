# maps.R
# 12 February 2018

# load library -------------------------------------------------------------

library(tidyverse)
library(ggpubr)
library(lubridate)
library(dplyr)
library(readr)
# load data ---------------------------------------------------------------
library(readxl)
Book1 <- read_excel("data/Book1.xlsx")
book <- Book1
book <- read_delim("Desktop/book.csv", ";", 
           escape_double = FALSE, trim_ws = TRUE)

# plot maps ---------------------------------------------------------------

zoom_map <- ggplot(Book1, aes(x = lat, y = long)) +
  geom_point(colour = "red", size = 0.85)
  borders(fill = "grey70", colour = "black") +
  coord_equal(xlim = c(-40, 0), ylim = c(15, 45), expand = 0) 
 
  
africa_map <- ggplot(book, aes(x = lat, y = long)) +
  borders(fill = "grey70", colour = "black") +
  coord_equal(xlim = c(-37, 60), ylim = c(-37, 50), expand = 0) +
  geom_point(data = book, aes(x = lat, y = long), colour = "red", size = 0.85)
  
world_map <- ggplot(book) +
  borders(fill = "grey70", colour = "black") +
  # coord_equal(xlim = c(-, 0), ylim = c(15, 45), expand = 0) +
  geom_point(data = book, aes(x = lat, y = long), colour = "red", size = 0.85)
    
  