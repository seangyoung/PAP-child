rm(list = ls())
# require(devtools)
# devtools::install_github("dkahle/ggmap", ref = "tidyup")

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
       ,"patchwork" 
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
crime <- read.csv(file="LR-child-maltreatment.csv",header=T,sep=",",
                  na.strings='NULL')

attach(crime)

STR_NME <- STR_NME %>% as.character %>% str_to_title %>% head()
CTY_NME <- CTY_NME %>% as.character %>% str_to_title %>% head()

crime$full_add <- paste(STR_NBR,STR_NME,STR_SFX_TYP_DESC, ",", 
                        CTY_NME,",", "AR")
head(crime$full_add)

library(ggmap)
# register_google(key = "AIzaSyAnpkPfp-QS40gnNzeNIioXrUZSSOaeOjo")

# geocode("Houston", output = "all") ## check if geocode is working

geocoded_latlon <- geocode(crime$full_add,output="latlon")
geocoded_latlon %>% summary()

crime.gc <- cbind(crime, geocoded_latlon)

crime.gc.lr <- crime.gc %>% filter(CTY_NME == "Little Rock")

qmplot(lon, lat, data = crime.gc.lr)

write.csv()
crime.gc <- read.csv(file="LR-child-Maltreatment-Geocoded.csv",header=T,
                     sep=",",na.strings='NULL')

qmplot(lon, lat, data = crime.gc, maptype = "toner-lite", color = I("red"))


