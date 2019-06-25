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

# plot(cm_lr_500ft)

cm_lr_500ft %>%
  ggplot(aes(fill = Banks)) + 
  geom_sf(color = NA) + 
  coord_sf(crs = 2765) + 
  scale_fill_viridis_c(option = "plasma") + 
  labs(title = "Banks by 500-ft grid")

ggplot() + geom_sf(data = cm_lr_500ft) + aes(fill = CM_Count) +
  theme(panel.grid.major = element_line(color = "white")) +
  scale_fill_gradientn(colors = sf.colors(20))

outer_lr = st_union(cm_lr_500ft)

cm_lr_500ft %>% head(3)

plot(cm_lr_500ft)

cm_lr_500ft %>% select(CM_Count) %>% plot(key.pos = 4,logz = TRUE, pal = viridis::viridis, graticule = TRUE, axes = TRUE) # 1: below; 4: right

cm_lr_500ft %>% st_drop_geometry() %>% summary()

#### 

variable = "year"
values <- unique(cps[[variable]])
year_dat <- list()
brks <- 9
window_cps <- get_window(cps, buff_dist = 10000)
for(i in seq_along(values)){
  dat <- filter(cps, !!as.name(variable) == values[i])
  points.ppp <- as.ppp(st_coordinates(ll(dat)),window_cps)
  densityRaster <- raster(density(points.ppp, scalekernel=TRUE, sigma = 0.005))
  dens_data <- gplot_data(densityRaster, maxpixels = 2500) %>%
    mutate(!!as.name(variable) := values[i])
  year_dat[[i]] <- dens_data
}
year_dat <- do.call(rbind, year_dat)

CPS_KDE_BY_YEAR_plot <- ggmap(cps_base_map) +
  geom_tile(data = year_dat, 
            aes(x,y,fill = as.factor(ntile(value,brks)), 
                group = !!as.name(variable)), alpha=0.8) +
  scale_fill_viridis_d(name = variable) +
  labs(title = "CPS accepted in Richmond, VA by year",
       caption = "Figure 5.2") +
  facet_wrap(vars(!!as.name(variable))) +
  mapTheme() +
  theme(
    legend.key = element_rect(fill = "white"),
    strip.text = element_text(face = "plain", size = 11, hjust = 0),
    strip.background = element_rect(fill = "white"),
    legend.position = "none"
  )


library("rgeos")
regions_gSimplify <- gSimplify(cm_lr_500ft, tol = 0.05, topologyPreserve = TRUE)

library(raster)
library(spdep)
library(maptools)

nb <- poly2nb(cm_lr_500ft)

create_regions <- function(data) {
  group <- rep(NA, length(data))
  group_val <- 0
  while(NA %in% group) {
    index <- min(which(is.na(group)))
    nb <- unlist(data[index])
    nb_value <- group[nb]
    is_na <- is.na(nb_value)
    if(sum(!is_na) != 0){
      prev_group <- nb_value[!is_na][1]
      group[index] <- prev_group
      group[nb[is_na]] <- prev_group
    } else {
      group_val <- group_val + 1
      group[index] <- group_val
      group[nb] <- group_val
    }
  }
  group
}

region <- create_regions(nb)
pol_rgn <- maptools::spCbind(cm_lr_500ft, region)
pol2 <- unionSpatialPolygons(pol_rgn, region)
plot(pol2)