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


rm(list = ls())
setwd("~/Data/LR-Child")

crime.gc.read <- read.csv(file="LR-child-mt-accepted-geocoded.csv",
                          header=T, sep=",",na.strings='NULL')

tab = as.data.frame(table(crime.gc.read$CTY_NME))
cat("Number of different city names", nrow(tab))

tab = tab %>% filter(Freq > 50)

ggplot(tab, aes(x = reorder(Var1, -Freq), y=Freq)) + geom_bar(stat="identity") +
  xlab("City Names") + ylab("Count") + coord_flip() +
  theme(axis.text=element_text(size=10))


########### Frequent Itemset

library(arules)
library(arulesViz)
crime.lr <- as(as.matrix(crime.gc.lr[,10:130]),"transactions")
itemFrequency(crime.lr, type = "absolute")
itemFrequencyPlot(crime.lr, support = 0.05)

## First apply apriori
rules <- apriori(crime.lr, parameter = list(support = 0.06, confidence = 0.9, 
                                         minlen = 1))
#summary(rules)
#inspect(sort(rules, by ="lift"))
print(inspect(head(sort(rules, by ="lift"),20)),digits=4)

interestMeasure(rules, c("support", "chiSquare", "confidence", "coverage", 
                         "lift", "oddsRatio"),crime.lr)
quality(rules) <- cbind(quality(rules), interestMeasure(rules, c("coverage","oddsRatio"), crime.lr))
inspect(head(sort(rules, by = "coverage"),20))

plot(rules, method="graph")
