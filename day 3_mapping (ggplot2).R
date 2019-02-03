#Day 3
#Aadam Rawoot
#31 Jan 2019
#Mapping in ggplot


#r format data, how to load it
load("data/south_africa_coast.RData")
load("data/sa_provinces.RData")
load("data/rast_annual.RData")
load("data/MUR.RData")
load("data/MUR_low_res.RData")

#convert csv to r data 

sst <- MUR_low_res

#sst-sea surface temp

cols11 <- c("#004dcd", "#0068db", "#007ddb", "#008dcf", "#009bbc",
            "#00a7a9", "#1bb298", "#6cba8f", "#9ac290", "#bec99a")
#creating a palette

#57D7B8,#44B6A8,#399695,#32777E,#2C5964,#233E4A

ggplot(data = ChickWeight, aes(x = Time, y = weight)) + geom_point()

Chickweight <- datasets::ChickWeight

ggplot(data = south_africa_coast, aes(x = lon, y = lat)) + geom_point() +
  labs(x = "longitude", y = "latittude") +
  ggtitle("Longitude and latitude of South African coast")

#geom_path-makes a pattern/picture
#geom_polygon-creates a outline

ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_polygon(colour = "red", fill = "blue", aes(group = group)) # The land mask


ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group)) # The province borders

ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group)) +
  coord_equal(xlim = c(15, 34), ylim = c(-36, -26), expand = 0) +#coord_equal-xlim and ylim gives limits
  labs(x = "longitude", y = "latittude") +
  ggtitle("Longitude and latitude of South African coast")# Force lon/lat extent


ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_raster(data = sst, aes(fill = bins)) + # The ocean temperatures
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group)) +
  coord_equal(xlim = c(15, 34), ylim = c(-36, -26), expand = 0)

#bins-range where temp falls within

ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_raster(data = sst, aes(fill = bins)) +
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group)) +
  scale_fill_manual("Temp. (°C)", values = cols11) + # Set the colour palette
  coord_equal(xlim = c(15, 34), ylim = c(-36, -26), expand = 0)

#scale fill manuel-add manuel lable for our scale
#values are the palettes you create.

final_map <- ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_raster(data = sst, aes(fill = bins)) +
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group)) +
  geom_tile(data = rast_annual, aes(x = lon, y = lat, fill = bins), 
            colour = "white", size = 0.1) +
  scale_fill_manual("Temp. (°C)", values = cols11) +
  coord_equal(xlim = c(15, 34), ylim = c(-36, -26), expand = 0) +
  scale_x_continuous(position = "top") + # Put x axis labels on top of figure
  theme(axis.title = element_blank(), # Remove the axis labels
        legend.text = element_text(size = 7), # Change text size in legend
        legend.title = element_text(size = 7), # Change legend title text size
        legend.key.height = unit(0.3, "cm"), # Change size of legend
        legend.background = element_rect(colour = "white"), # Add legend background
        legend.justification = c(1, 0), # Change position of legend
        legend.position = c(0.55, 0.4) # Fine tune position of legend
  )
final_map


# [A.A]
# The script runs complete
# Should add your own comments, this looks copied from the book
# Very vague
