# gc()

rm(list = ls())

## Only R codes for repliacte PAP 

# install.packages("devtools")
# devtools::install_github("thomasp85/patchwork")

if(T){
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
}


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


# Load new packages (might be redundant)
pacman::p_load(lubridate, sf, raster, rgdal, broom, rgeos, GISTools)

# Load old packages
pacman::p_load(dplyr, ggplot2, ggthemes, magrittr, viridis)

setwd("C:/Users/jd033/Box/Child Maltreatment/Little Rock Data")

# nbr =readOGR(
#   dsn = paste0("Shapefile_LR"),
#   layer = "LR_Municipal_Boundary_SF")
# 
# nbr = nbr %>% st_as_sf() %>% st_transform(crs = 4269)

cm_lr_500ft = read_sf("C:/Users/jd033/Box/Child Maltreatment/Little Rock Data/Grid_withFactors/CM_LR_500ft_Factors.shp") 
#%>% st_transform(crs = 2765)

# Class
class(cm_lr_500ft)

# Dimensions
dim(cm_lr_500ft)
# Info in shapefile
names(cm_lr_500ft)

library(tmap)
tm_shape(cm_lr_500ft) + tm_borders() + tm_bubbles(col = "red", size = "Banks")
tm_shape(cm_lr_500ft) + tm_borders() + tm_bubbles(col = "red", size = "LiqStore")
tm_shape(cm_lr_500ft) + tm_borders() + tm_bubbles(col = "red", size = "Hotel")
tm_shape(cm_lr_500ft) + tm_borders() + tm_bubbles(col = "red", size = "CM_Count")


pryr::object_size(cm_lr_500ft)

regions_1k <- sf::st_simplify(cm_lr_500ft, preserveTopology = TRUE, dTolerance = 10)
pryr::object_size(regions_1k)

# plot(cm_lr_500ft)

regions_1k %>%
  ggplot(aes(colour = CM_Count)) + 
  geom_sf(color = NA) + 
  coord_sf(crs = 2765) + 
  scale_fill_viridis_c(option = "plasma") + 
  labs(title = "CM counts by grid")

ggplot() + geom_sf(data = cm_lr_500ft) + aes(fill = CM_Count) +
  theme(panel.grid.major = element_line(color = "white")) +
  scale_fill_gradientn(colors = sf.colors(20))

outer_lr = st_union(cm_lr_500ft)

cm_lr_500ft %>% head(3)

plot(cm_lr_500ft)

cm_lr_500ft %>% select(CM_Count) %>% plot(key.pos = 4,logz = TRUE, pal = viridis::viridis, graticule = TRUE, axes = TRUE) # 1: below; 4: right


cm_lr_500ft %>% st_drop_geometry() %>% summary()

cm_lr_factors <- cm_lr_500ft %>% st_drop_geometry()
cormat <- round(cor(cm_lr_factors),2)

library(reshape2)
melted_cormat <- melt(cormat)
# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
## reorder function 
reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

partial_reorder_cormat <- function(cormat, idx){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat[idx,idx])/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}
# Reorder the correlation matrix
cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
library(ggplot2)
# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))



#### ACS SHapefile 

cm_lr_acs = read_sf("C:/Users/jd033/Box/Child Maltreatment/Little Rock Data/SocialMeasures/LR_Tracts_ACS_Data_CM_Counts.shp") 
#%>% st_transform(crs = 2765)

# Class
class(cm_lr_acs)

# Dimensions
dim(cm_lr_acs)
# Info in shapefile
names(cm_lr_acs)

lr_tract_sel = cm_lr_acs %>% select(Incident_C, NLTotPop, PopDensity,Perc_Under,
                                              Perc_Black, Perc_NonWh, Perc_Hispa, Perc_NonMa,
                                              Perc_FHH,Perc_SingP,PercLowEdu, Perc_Rente, 
                                              Perc_PopUn, Perc_PopSt, Perc_NotIn, Perc_Publi, 
                                              PercHighHH, PercCollEd, Perc_OwnHo)


# plot(cm_lr_500ft)

p1 <- cm_lr_acs %>%
  ggplot(aes(fill = Incident_C/NLTotPop)) + 
  geom_sf(color = NA) + 
  coord_sf(crs = 2765) + 
  scale_fill_viridis_c(option = "plasma") + 
  labs(title = "Incident Counts / Pop. size (NL)")

p2 <- cm_lr_acs %>%
  ggplot(aes(fill = PercHighHH)) + 
  geom_sf(color = NA) + 
  coord_sf(crs = 2765) + 
  scale_fill_viridis_c(option = "plasma") + 
  labs(title = "%High Household Cost")

p3 <- cm_lr_acs %>%
  ggplot(aes(fill = PercLowEdu)) + 
  geom_sf(color = NA) + 
  coord_sf(crs = 2765) + 
  scale_fill_viridis_c(option = "plasma") + 
  labs(title = "% Low Education Level")

p4 <- cm_lr_acs %>%
  ggplot(aes(fill = PercCollEd)) + 
  geom_sf(color = NA) + 
  coord_sf(crs = 2765) + 
  scale_fill_viridis_c(option = "plasma") + 
  labs(title = "% College Education")

### Second Set 

p1 <- cm_lr_acs %>%
  ggplot(aes(fill = Perc_FHH)) + 
  geom_sf(color = NA) + 
  coord_sf(crs = 2765) + 
  scale_fill_viridis_c(option = "plasma") + 
  labs(title = "% Female Headed Household")+mapTheme()

p2 <- cm_lr_acs %>%
  ggplot(aes(fill = Perc_PopSt)) + 
  geom_sf(color = NA) + 
  coord_sf(crs = 2765) + 
  scale_fill_viridis_c(option = "plasma") + 
  labs(title = "% Population Struggling")+mapTheme()

p3 <- cm_lr_acs %>%
  ggplot(aes(fill = Perc_NotIn)) + 
  geom_sf(color = NA) + 
  coord_sf(crs = 2765) + 
  scale_fill_viridis_c(option = "plasma") + 
  labs(title = "% Not Insured")+mapTheme()

p4 <- cm_lr_acs %>%
  ggplot(aes(fill = Perc_Publi)) + 
  geom_sf(color = NA) + 
  coord_sf(crs = 2765) + 
  scale_fill_viridis_c(option = "plasma") + 
  labs(title = "% On Public Insurance")+mapTheme()

(p1 | p2) / (p3 | p4 )

## Third Set 

p1 <- cm_lr_acs %>%
  ggplot(aes(fill = Perc_Under)) + 
  geom_sf(color = NA) + 
  coord_sf(crs = 2765) + 
  scale_fill_viridis_c(option = "plasma") + 
  labs(title = "% under 18")+mapTheme()

p2 <- cm_lr_acs %>%
  ggplot(aes(fill = Perc_Black)) + 
  geom_sf(color = NA) + 
  coord_sf(crs = 2765) + 
  scale_fill_viridis_c(option = "plasma") + 
  labs(title = "% Black")+mapTheme()

p3 <- cm_lr_acs %>%
  ggplot(aes(fill = Perc_NonWh)) + 
  geom_sf(color = NA) + 
  coord_sf(crs = 2765) + 
  scale_fill_viridis_c(option = "plasma") + 
  labs(title = "% Non-White")+mapTheme()

p4 <- cm_lr_acs %>%
  ggplot(aes(fill = Perc_Hispa)) + 
  geom_sf(color = NA) + 
  coord_sf(crs = 2765) + 
  scale_fill_viridis_c(option = "plasma") + 
  labs(title = "% Hispanic")+mapTheme()

(p1 | p2) / (p3 | p4 )

ggplot() + geom_sf(data = cm_lr_acs) + aes(fill = Incident_C/NLTotPop) +
  theme(panel.grid.major = element_line(color = "white")) +
  scale_fill_gradientn(colors = sf.colors(20))

# Incident_Count_sum,NLTotPop, PopDensity,Perc_Under18,
# Perc_Black, Perc_NonWhite, Perc_Hispanic, Perc_NonMarr_Fam_HH,
# Perc_FHH,Perc_SingPrnt_HH,PercLowEduc, Perc_RenterOcc, 
# Perc_PopUnder18inPov, Perc_PopStrugg, Perc_NotInsured, Perc_PublicInsure, 
# PercHighHHCost, PercCollEduc, Perc_OwnHome

cm_lr_acs %>% select(Perc_FHH,Perc_SingPrnt_HH,PercLowEduc,Perc_NotInsured,PercCollEduc,Perc_OwnHome) %>% gather(VAR, Perc, -geom) -> nc2
ggplot() + geom_sf(data = nc2, aes(fill = SID)) + facet_wrap(~VAR, ncol = 1) +
  scale_y_continuous(breaks = 34:36) +
  scale_fill_gradientn(colors = sf.colors(20)) +
  theme(panel.grid.major = element_line(color = "white"))


### Reading Shaun's SPSS codes 

fname <- "USE_Reduced_LR Tract Data_ACS and Malreatment Counts_.sav"
library(foreign)

setwd("C:/Users/jd033/Box/Child Maltreatment/Little Rock Data/SPSS")

lr_tract_acs = read.spss(fname,to.data.frame = TRUE)

lr_tract_acs_select = lr_tract_acs %>% select(Incident_Count_sum,NLTotPop, PopDensity,Perc_Under18,
                                              Perc_Black, Perc_NonWhite, Perc_Hispanic, Perc_NonMarr_Fam_HH,
                                              Perc_FHH,Perc_SingPrnt_HH,PercLowEduc, Perc_RenterOcc, 
                                              Perc_PopUnder18inPov, Perc_PopStrugg, Perc_NotInsured, Perc_PublicInsure, 
                                              PercHighHHCost, PercCollEduc, Perc_OwnHome)

cormat <- round(cor(lr_tract_acs_select),2)

library(reshape2)
# melted_cormat <- melt(cormat)
# Get lower triangle of the correlation matrix

# Reorder the correlation matrix 
cormat <- partial_reorder_cormat(cormat, idx = 2:19)
upper_tri <- get_upper_tri(cormat)

# melted_cormat <- melt(cormat)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)

# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 2) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5)) + labs(title = "Correlation between Built Environment Factors",
                                                                                  caption = "Figure 2.2")

## KDE for Bars and Banks 

setwd("C:/Users/jd033/Box/Child Maltreatment/Little Rock Data/CSV")
mobhomes <- readxl::read_xlsx("Rental_MobileHomes.xlsx")

mobhomes <- mobhomes %>% filter(!is.na(X) | !is.na(Y)) %>%
  st_as_sf(., coords = c("X", "Y"), crs = 2765)

banks <- readxl::read_xlsx("Banks.xlsx")

banks <- banks %>% filter(!is.na(X) | !is.na(Y)) %>%
  st_as_sf(., coords = c("X", "Y"), crs = 2765)

ggplot() + geom_sf(data = banks)

ggplot() + geom_sf(data = mobhomes)

