install.packages("sf")
install.packages("raster")
install.packages("spData")
devtools::install_github("Nowosad/spDataLarge")

library(sf)          # classes and functions for vector data
library(raster)      # classes and functions for raster data
library(spData)        # load geographic data
library(spDataLarge)   # load larger geographic data

pacman::p_load(lubridate, rgdal, broom, rgeos, GISTools)

# Load old packages
pacman::p_load(dplyr, ggplot2, ggthemes, magrittr, viridis)

setwd("C:/Users/jd033/Box/Child Maltreatment/Little Rock Data")

cm_shp =readOGR(
  dsn = paste0("Shapefile_LR"),
  layer = "LR_Municipal_Boundary_SF")

# Class
class(cm_shp)

# Dimensions
dim(cm_shp)

plot(cm_shp)

names(cm_shp)

slotNames(cm_shp)
