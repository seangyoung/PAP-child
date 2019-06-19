rm(list = ls())

## Only R codes for repliacte PAP 

# install.packages("devtools")
# devtools::install_github("thomasp85/patchwork")

library("sf")            # Spatial data objects and methods
library("mapview")       # Interactive Map Viewing
library("ggmap")         # ggplot2 addon for base maps
library("cowplot")
library("spatstat")      # KDE and other spatial functions
library("raster")        # cell-based spatial operations
library("tidyverse")     # data manipulation framework
library("Hmisc")         # using cut2() functions for ggplot legends
library("fitdistrplus")  # Distribution fitting functions
library("lubridate")     # Power tools for handling dates
library("tidycensus")
library("lwgeom")
library("Hmisc")
library("hrbrthemes")
library("gridExtra")
library("patchwork")
library("spdep")         # KNN functions
library("foreach")
library("doParallel")
library("corrplot")
library("ranger")        # randomforest implimentation      
library("glmnet")        # for Ridge and Lasso Regression
library("knitr")         # for kable table
library("kableExtra")
library("FNN")           # KNN for CPS vs. NN plots
library("groupdata2")
library("htmltools")
library("viridis")
library("viridisLite")

## Themes 

mapTheme <- function() {
  theme(
    plot.title = element_text(size = 14, family = "sans", face = "plain", hjust = 0),
    plot.subtitle=element_text(size = 11, family = "sans", hjust = 0),
    plot.caption=element_text(size = 10, family = "sans", face = "italic", hjust = 0),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    legend.title = element_text(size = 10, family = "sans"),
    legend.text = element_text(size = 9, family = "sans"),
    panel.border = element_blank()
  )
}

plotTheme <- function() {
  theme(
    plot.title = element_text(size = 14, family = "sans", face = "plain", hjust = 0),
    plot.subtitle=element_text(size = 11, family = "sans", hjust = 0),
    plot.caption=element_text(size = 10, family = "sans", face = "italic", hjust = 0), 
    axis.title.x = element_text(size = 10, family = "sans", face = "plain", hjust = 1, vjust = -0.5),
    axis.title.y = element_text(size = 10, family = "sans", face = "plain", hjust = 1, vjust = 1),
    axis.text = element_text(size = 9, family = "sans", face = "plain"),
    panel.background = element_blank(),
    panel.grid.minor = element_line(colour = "gray"),
    panel.grid.major = element_line(colour = "gray"),
    axis.ticks = element_blank(),
    legend.title = element_text(size = 10, family = "sans"),
    legend.text = element_text(size = 9, family = "sans"),
    axis.line = element_blank()
  )
}

## Options

mapviewOptions(basemaps = c("Stamen.TonerLite", "OpenStreetMap.DE"))

base_dir = "C:/Users/jd033/Box/Child Maltreatment"
fishnet_grid_dim = 1000
k_direction = 8 # 4 = rook, 8 = queen
k_nearest_neighbors = 5
# Either k (e.g. 5 or 10) or "LOOCV"
n_folds = "LOOCV"
# threshold quntile for statArea grouping
stat_area_quantile = 0.60
# Number of simulations for CPS vs. NN
simulations = 1000
# Number of neighbors for CPS vs. NN
k = 5
# random seed
set.seed(11235)

## Source 

source('C:/Users/jd033/Documents/GitHub/PAP-child/FUNCTIONS_VAPAP_LR.R', echo = FALSE, keep.source = TRUE)
# source('C:/Users/jd033/Documents/GitHub/PAP-child/FEA_CREATE_VARIABLES_LR.R', echo = TRUE, keep.source = TRUE)

## Global variables 

# requires all data in *.csv or *.xls files containing coordinate field names "X" and "Y"
# `crs` in the call to `st_as_sf()` needs to be set to the ESPG code of your data projection
# `base_dir` file path and many feature names are specified for the current project.

##1.1 Global Variables
# mapviewOptions(basemaps = c("Stamen.TonerLite", "OpenStreetMap.DE"))
base_dir = "C:/Users/jd033/Box/Child Maltreatment"


##2.1 Load Data
files <-list.files(file.path(base_dir,"/Little Rock Data/CSV"), pattern = "*\\.xlsx$|*\\.csv$")
var_list <- vector(mode = "list")
var_names <- NULL
for(i in seq_along(files)){
  filename <- str_sub(files[i], start = 1, end = -5)
  sf_i <- tryCatch({
    if(tools::file_ext(files[i]) == "xlsx"){
      dat <- readxl::read_xlsx(file.path(base_dir,"/Little Rock Data/CSV",files[i])) 
    } else if(tools::file_ext(files[i]) == "csv"){
      dat <- read.csv(file.path(base_dir,"/Little Rock Data/CSV",files[i])) 
    }
    dat %>%
      filter(!is.na(X) | !is.na(Y)) %>%
      st_as_sf(., coords = c("X", "Y"), crs = 2765)
  }, error = function(e){
    cat(filename, "error = ",e$message,"\n")
    return(e)
  }
  )
  if(!inherits(sf_i, "error")){
    var_list[[length(var_list)+1]] <- sf_i
    var_names[length(var_list)] <- filename
  }
}
names(var_list) <- var_names

# knitr::kable(var_names, caption = "List of Variables")


#### Read shapefile 

# Load new packages (might be redundant)
pacman::p_load(lubridate, sf, raster, rgdal, broom, rgeos, GISTools)

# Load old packages
pacman::p_load(dplyr, ggplot2, ggthemes, magrittr, viridis)

setwd("C:/Users/jd033/Box/Child Maltreatment/Little Rock Data")

# nbr =readOGR(
#   dsn = paste0("Shapefile_LR"),
#   layer = "LR_Municipal_Boundary_SF")

nbr = st_read("C:/Users/jd033/Box/Child Maltreatment/Working Files_AR ACS Data/ACS_Tract_Shapefile/LR_Tracts_Working51.shp") 
      # %>% st_transform(2756)
plot(nbr)

# Class
class(nbr)

# Dimensions
dim(nbr)
# Info in shapefile
names(nbr)


#### Shapefile reading: neighborhood dissolve? 

# nbr <- read_sf("https://data.richmondgov.com/resource/7juf-nwis.geojson")  %>%
#   st_sf() %>%
#   st_transform(102747)

nbr_diss <- nbr %>%
  mutate(dissolve = 1) %>%
  # get rid of slivers
  st_buffer(., dist = 0.1) %>%
  group_by(dissolve) %>%
  summarise()

nbr_rast_SP <- raster(as(nbr_diss, "Spatial"), nrows = 2000, ncol = 2000)

## get map

(cm_bbox = unname(st_bbox(ll(st_buffer(var_list[["CM_geocoded"]],dist = 0.1)))))
# (cm_bbox = unname(st_bbox(ll(st_buffer(nbr,dist = 0.1)))))

# var_list[["CM_geocoded"]]
# 
# cm_bbox

cps_base_map   <- get_googlemap(location = cm_bbox,
                          source = "google",
                          maptype = "terrain")

# ggmap(cps_base_map)

### get CPS_Accepted values (add 1 column for dissolving)
cps_dissolve <- var_list[["CM_geocoded"]] %>%
  mutate(value = 1) %>%
  dplyr::select(value) #%>% st_transform(crs = 2756)

st_crs(nbr) <- "+proj=lcc +lat_1=34.76666666666667 +lat_2=33.3 +lat_0=32.66666666666666 +lon_0=-92 +x_0=400000 +y_0=400000
+ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

nbr <- nbr %>% st_transform(2756)

net <- st_make_grid(nbr, cellsize = fishnet_grid_dim) #%>%st_transform(2756)

nbr %>% st_crs()
cps_dissolve %>% st_crs()

# st_crs(cps_dissolve) <- "+proj=lcc +lat_1=34.76666666666667 +lat_2=33.3 +lat_0=32.66666666666666 +lon_0=-92 +x_0=400000 +y_0=400000
# +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

# count CPS incidents per net cell - really just to get net raster into sf polygon format
net_agg <- aggregate(cps_dissolve, net, sum) %>%
  tibble::rowid_to_column(.,"net_id")

# list of net cells IDs that intersect with Richmond
net_intersect <- st_intersects(nbr, net_agg) 

# extract Richmonds net cells based on intersect ID
net_littlerock <- net_agg[unique(unlist(net_intersect)),]
net_hood <- st_join(net_littlerock, nbr, largest = TRUE)
listw <- nb2listw(poly2nb(as(net_littlerock, "Spatial"), queen = TRUE))
