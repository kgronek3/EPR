library(spdep)
library(rgdal)
library(maptools)
library(sp)
library(RColorBrewer)
library(classInt)
library(maps)
library(sf)
library(ggplot2)

setwd("/home/oic/IiE/EkonometriaPrzestrzennaR")

pow <- readOGR("./data/", "powiaty") # 380 jedn. 
pow <- spTransform(pow, CRS("+proj=longlat +datum=NAD83"))

POW <- st_read("./data/powiaty.shp")
POW <- st_transform(POW, 4326)




