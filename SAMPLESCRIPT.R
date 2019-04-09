load("data/sa_provinces.Rdata")
load("data/africa_coast.Rdata")

rm(africa_map)
ggplot(data = Book1, aes(x = lon, y = lat)) +
  geom_point() +
  geom_path(data = sa_pr, aes(x = lon, y = lat, group = group)) +
  coord_equal(xlim = c(-37, 60), ylim = c(-37, 50), expand = 0)

ggplot(data = africa_coast, aes(x = lon, y = lat)) +
  geom_path(aes(group = group)) +
  geom_point(data = Book1, aes(x = as.numeric(lon), y = as.numeric(lat))) +
  geom_path(data = sa_provinces, aes(group = group)) +
  coord_equal(xlim = c(-20, 60), ylim = c(-40, 40), expand = 0) +
  borders()



             