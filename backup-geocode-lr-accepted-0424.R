rm(list = ls())
library(devtools)
# devtools::install_github("dkahle/ggmap", ref = "tidyup")
# devtools::install_github("thomasp85/patchwork")
library(pacman)
p_load("sf"             # Spatial data objects and methods
       ,"mapview"        # Interactive Map Viewing
       ,"ggmap"          # ggplot2 addon for base maps
       ,"cowplot" 
       ,"spatstat"       # KDE and other spatial functions
       ,"raster"         # cell-based spatial operations
       ,"tidyverse"      # data manipulation framework
       ,"Hmisc"          # using cut2(  functions for ggplot legends
       ,"fitdistrplus"   # Distribution fitting functions
       ,"lubridate"      # Power tools for handling dates
       ,"tidycensus" 
       ,"lwgeom" 
       ,"Hmisc" 
       ,"hrbrthemes" 
       ,"gridExtra" 
       ,"spdep"          # KNN functions
       ,"foreach" 
       ,"doParallel" 
       ,"corrplot" 
       ,"ranger"         # randomforest implimentation      
       ,"glmnet"         # for Ridge and Lasso Regression
       ,"knitr"          # for kable table
       ,"kableExtra" 
       ,"FNN"            # KNN for CPS vs. NN plots
       ,"groupdata2" 
       ,"htmltools" 
       ,"viridis" 
       ,"viridisLite")

setwd("~/Data/LR-Child")
crime <- read.csv(file="child-mt-accepted.csv",header=T,sep=",",
                  na.strings='NULL')
attach(crime)

crime$STR_NME <- crime$STR_NME %>% as.character %>% str_to_title 

head(crime$STR_NME)

crime$CTY_NME <- crime$CTY_NME %>% as.character %>% str_to_title

full_add <- paste(STR_NBR,STR_NME,STR_SFX_TYP_DESC, ",", 
                  CTY_NME,",", "AR")
head(full_add)

crime$full_add = full_add

library(ggmap)
register_google(key = "AIzaSyAnpkPfp-QS40gnNzeNIioXrUZSSOaeOjo")

geocode("Houston", output = "all") ## check if geocode is working

# register_google(key = "<your key here>")

geocoded_latlon <- geocode(crime$full_add,output="latlon") 

geocoded_latlon %>% head()
crime.gc <- cbind(crime,geocoded_latlon)

# crime.gc.lr <- crime.gc %>% filter(CTY_NME == "Little Rock")

crime.gc.lr = crime.gc %>% filter(CTY_NME %in% c("Little Rock"))%>% 
  filter(lat >= 34.62606 & lat <= 34.82195) %>% 
  filter(lon >= -92.52091 & lon <= -92.15494)

nrow(crime.gc.lr)

qmplot(lon, lat, data = crime.gc.lr)

write.csv(crime.gc, "LR-child-mt-accepted-geocoded.csv")

# rm(list = ls())
crime.gc.read <- read.csv(file="LR-child-mt-accepted-geocoded.csv",
                     header=T, sep=",",na.strings='NULL')

crime.gc$lat = crime.gc$lat %>% as.numeric()
crime.gc$lon = crime.gc$lon %>% as.numeric()

# qmplot(lon, lat, data = crime.gc, maptype = "toner-lite", color = I("red"))

