rm(list = ls())
require(devtools)
# devtools::install_github("dkahle/ggmap", ref = "tidyup")
devtools::install_github("thomasp85/patchwork")

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

child.death <- read.csv(file="Pulaski-child-death-data.csv",header=T,sep=",",
                  na.strings='NULL')
child.death$Address = as.character(child.death$Address)
geocoded_latlon <- geocode(child.death$Address,output="latlon")
geocoded_latlon %>% head()

child.death <- cbind(child.death,geocoded_latlon)
colnames(child.death)

write.csv(child.death, "Pulaski-child-death-data-geocoded.csv")

setwd("~/Data/LR-Child")

colClasses = c("character","character","numeric",rep("character",2),rep("factor",10),rep("character",2),rep("numeric",2))

cd.gc <- read.csv(file="Pulaski-child-death-data-geocoded.csv",header=T,sep=",",colClasses = colClasses,nrows = 41,
                  na.strings='NULL')

str(cd.gc)

qmplot(lon, lat, data = cd.gc, maptype = "toner-lite", color = I("red"))


qmplot(lon, lat, data = cd.gc, geom = "point", maptype = "toner-lite", darken = .2, color = I("red"), legend = "topleft") +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = .2, color = NA) +
  scale_fill_gradient2("Propensity", low = "white", mid = "yellow", high = "red", midpoint = 20)

library(lubridate)
# str(crime.gc)
# crime.gc.lr$Incident.Date <- ifelse(as.character(crime.gc.lr$Incident.Date) == "",as.character(crime.gc.lr$Referral.Date),as.character(crime.gc.lr$Incident.Date))

(cd.gc$Incident.Date <- as.Date(cd.gc$Date.of.Death, format = "%m/%d/%Y" ))
(cd.gc$IncidentYear <- lubridate::year(ymd(cd.gc$Incident.Date)))
cd.gc$IncidentWeek <- lubridate::week(ymd(cd.gc$Incident.Date))


library(dplyr)

crime.yearly.ct <- cd.gc %>% group_by(IncidentYear) %>%
  dplyr::summarise(count = n()) 

crime.weekly.ct <- cd.gc %>% group_by(IncidentYear, IncidentWeek ) %>%
  dplyr::summarise(count = n())

library(summarytools)

dfSummary(cd.gc[,6:15])


### Plots by year and week 

(weekly.plot <- ggplot(crime.weekly.ct, aes(x = IncidentWeek, y = count, group = as.factor(IncidentYear), colour = as.factor(IncidentYear)))+
    geom_bar(aes(fill=IncidentWeek),stat = "identity")+ylab("Incident Count")+
    xlab("Weeks") +
    facet_grid(IncidentYear~.,scales="free_y")+
    theme(legend.position="bottom"))

(yearly.plot <- ggplot(crime.yearly.ct, aes(x = IncidentYear, y = count))+
    geom_bar(stat="identity") +ylab("Incident Count")+
    xlab("Years") +labs(title = "Yearly Counts") +
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
    theme(legend.position="bottom"))

(weekly.plot <- ggplot(crime.weekly.ct, aes(x = IncidentYear, y = count))+
    geom_bar(aes(fill=IncidentWeek),stat = "identity",position = position_stack(reverse = TRUE)) +
    coord_flip() +
    theme(legend.position = "top")+ylab("Incident Count")+
    xlab("Years"))

# Fixed some minor errors for the child death data


library(tidyverse)
library(stringr)
attach(cd.gc)

get.first.name <- function(cell){ 
  x<-unlist(str_split(as.character(cell), "\\;|,")) 
  return(x[1]) 
} 
sapply(Allegation.Prelim.Cause,get.first.name)

# cd.gc %>% mutate(unpacked = str_split(Allegation.Prelim.Cause, "(?<!\\()\\-|[:\\)\\(]"))%>% 
#   get.first.name()
#   
#   
#   
#   mutate(unpacked = str_split(Allegation.Prelim.Cause, ";,"))%>% 
#   unlist() %>% 
#   mutate(Allegation.First.Cause = str_trim(unpacked))
