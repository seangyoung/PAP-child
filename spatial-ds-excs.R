library(tidyverse)
library(sf)
#> Linking to GEOS 3.7.0, GDAL 2.4.0, PROJ 5.2.0
system.file("gpkg/nc.gpkg", package="sf") %>%
  read_sf() %>%
  st_transform(32119) %>%
  select(BIR74) %>%
  plot(graticule = TRUE, axes = TRUE)

(file <- system.file("gpkg/nc.gpkg", package="sf"))
(file %>% read_sf()  -> nc)
class(nc)
(nc %>% st_transform(32119) -> nc.32119)
(nc.32119 %>% select(BIR74) -> nc.32119.bir74)
nc.32119.bir74 %>% plot(graticule = TRUE, axes = TRUE)

## ggplot 

ggplot() + geom_sf(data = nc.32119) + aes(fill = BIR74) +
  theme(panel.grid.major = element_line(color = "white")) +
  scale_fill_gradientn(colors = sf.colors(20))

## faceted ggplot - uses gather 

nc.32119 %>% select(SID74, SID79) %>% gather(VAR, SID, -geom) -> nc2
ggplot() + geom_sf(data = nc2, aes(fill = SID)) + facet_wrap(~VAR, ncol = 1) +
  scale_y_continuous(breaks = 34:36) +
  scale_fill_gradientn(colors = sf.colors(20)) +
  theme(panel.grid.major = element_line(color = "white"))

suppressPackageStartupMessages(library(mapview))
nc.32119 %>% mapview(zcol = "BIR74", legend = TRUE, col.regions = sf.colors)


## Exercise 

storms <- system.file("shape/storms_xyz_feature.shp", package="sf") %>%
  read_sf()

storms %>%
  st_zm() %>%
  st_set_crs(4326) %>% 
  plot(graticule = TRUE, axes = TRUE)

## st_set_crs(): now we have the degree projections 


