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

is.geocoded = T

if(is.geocoded = F){
  crime <- read.csv(file="LR-child-maltreatment.csv",header=T,sep=",",
                    na.strings='NULL')
  attach(crime)
  crime$STR_NME <- crime$STR_NME %>% as.character %>% str_to_title 
  crime$CTY_NME <- crime$CTY_NME %>% as.character %>% str_to_title
  
  full_add <- paste(STR_NBR,STR_NME,STR_SFX_TYP_DESC, ",", 
                    CTY_NME,",", "AR")
    crime <- cbind(crime, full_add)

  library(ggmap)
  register_google(key = "AIzaSyAnpkPfp-QS40gnNzeNIioXrUZSSOaeOjo")
  # # geocode("Houston", output = "all") ## check if geocode is working
  #
  geocoded_latlon <- geocode(crime$full_add,output="latlon")
  geocoded_latlon %>% head()

  crime.gc <- cbind(crime, full_add,geocoded_latlon)
  colnames(crime.gc)

  write.csv(crime.gc, "LR-child-maltreatment-geocoded.csv")
}

setwd("~/Data/LR-Child")

crime.gc <- read.csv(file="LR-child-maltreatment-geocoded.csv",header=T,sep=",",na.strings='NULL')
barplot(table(crime.gc$CTY_NME))


crime.gc.lr = crime.gc %>% filter(CTY_NME %in% c("Little Rock"))%>% 
  filter(lat >= 34.62606 & lat <= 34.82195) %>% 
  filter(lon >= -92.52091 & lon <= -92.15494)
nrow(crime.gc.lr)

qmplot(lon, lat, data = crime.gc.lr, maptype = "toner-lite", color = I("red"))

# table(crime.gc.lr$CTY_NME)

qmplot(lon, lat, data = crime.gc.lr,
       geom = "blank", maptype = "toner-lite", darken = .2, legend = "topleft") +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = .5, color = NA) +
  scale_fill_gradient2("Propensity", low = "white", mid = "yellow", high = "red", midpoint = 60)

# ggsave("LR-child-maltreatment-kernel-density.png", height = 5, width = 7)


library(lubridate)
# str(crime.gc)
(crime.gc.lr$Incident.Date <- ifelse(as.character(crime.gc.lr$Incident.Date) == "",
                                 as.character(crime.gc.lr$Referral.Date),as.character(crime.gc.lr$Incident.Date)))

(crime.gc.lr$Incident.Date <- as.Date(crime.gc.lr$Incident.Date,format = "%m/%d/%Y" ))
(crime.gc.lr$IncidentYear <- lubridate::year(ymd(crime.gc.lr$Incident.Date)))
crime.gc.lr$IncidentWeek <- lubridate::week(ymd(crime.gc.lr$Incident.Date))

library(dplyr)

crime.yearly.ct <- crime.gc.lr %>% group_by(IncidentYear) %>%
  dplyr::summarise(count = n()) %>% filter(IncidentYear > 2010)

crime.weekly.ct <- crime.gc.lr %>% group_by(IncidentYear, IncidentWeek ) %>%
  dplyr::summarise(count = n()) %>% filter(IncidentYear > 2010)

library(ggplot2)
(crime.plot <- ggplot(crime.weekly.ct, aes(x = IncidentWeek, y = count, group = IncidentYear, colour = IncidentYear))+
  geom_line(stat = "identity")+geom_bar(alpha = 0.2,stat = "identity")+ylab("Incident Count")+
  xlab("Weeks") + #+theme_bw()+
  # theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  facet_grid(IncidentYear~.,scales="free_y")+
  theme(legend.position="bottom"))
crime.plot <- crime.plot + theme(axis.title.y = element_text(size = rel(1), angle = 90))+
  theme(axis.title.x = element_text(size = rel(1)))
crime.plot<- crime.plot+ theme(axis.text = element_text(size = rel(1)))
crime.plot <- crime.plot+theme(strip.text.x = element_text(size=12, face="bold"),strip.text.y = element_text(size=12, face="bold"))
print(crime.plot)

library(ggplot2)
(crime.plot <- ggplot(crime.yearly.ct, aes(x = IncidentYear, y = count))+
  geom_bar(alpha = 0.2,stat = "identity")+ylab("Incident Count")+
  xlab("Years") + #+theme_bw()+
  # theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  #facet_grid(OffenseDes~.,scales="free_y")+
  theme(legend.position="bottom"))
crime.plot <- crime.plot + theme(axis.title.y = element_text(size = rel(1), angle = 90))+
  theme(axis.title.x = element_text(size = rel(1)))
crime.plot<- crime.plot+ theme(axis.text = element_text(size = rel(1)))
crime.plot <- crime.plot+theme(strip.text.x = element_text(size=12, face="bold"),strip.text.y = element_text(size=12, face="bold"))
print(crime.plot)


# Animated Map 

# This animation shows how the aggravated assaults move over time. 

devtools::install_github('thomasp85/gganimate')
library(gganimate)

(crime.gc.lr$IncidentMonth <- zoo::as.yearmon(crime.gc.lr$Incident.Date,"%Y-%m"))

crime.ct.geo <- crime.gc.lr %>% filter(IncidentYear>2010) %>% group_by(lon, lat, IncidentMonth) %>% 
  dplyr::summarise(count = n()) 

littlerock <- qmplot(lon, lat, data = crime.ct.geo, maptype = "toner-lite", color = I("red"))

littlerock <- littlerock +
  geom_point(aes(x = lon, y = lat, size = count), 
             data = crime.ct.geo, colour = 'purple', alpha = .7) + 
  # stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = .5, color = NA) +
  transition_states(as.Date(IncidentMonth), transition_length = 1, state_length = 1)+
  ggtitle('Year: {frame_time}')+ transition_time(as.Date(IncidentMonth)) +
  
  ease_aes('linear')

animate(littlerock)


## Yearly plot 

crime.ct.yr <- crime.gc.lr %>% filter(IncidentYear>2010) %>% group_by(lon, lat, IncidentYear) %>% 
  dplyr::summarise(count = n()) 

p <- qmplot(lon, lat, data = crime.ct.yr, maptype = "toner-lite", color = I("red"))

p<- p +
  geom_point(aes(x = lon, y = lat, size = count),
             data = crime.ct.yr, colour = 'purple', alpha = .7) +
  labs(title = 'Week: {frame_time}', x = 'lon', y = 'lat') +
  transition_time(IncidentYear) +
  ease_aes('linear')

animate(p, nframes = 4)

