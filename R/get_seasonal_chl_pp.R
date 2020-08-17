library(dplyr)
library(raster)
library(sf)
library(ggplot2)
library(ncdf4)
library(reshape2)

## data coming from here ftp://ftp.nefsc.noaa.gov/pub/satdata/HAAK/OCCCI/NETCDFS/




#############################################################################################################
#################### Chlorophyll a ##########################################################################
#############################################################################################################

## Grab saved files
raw.dir <- here::here("data-raw/chl")
int.dir <- here::here("data-raw/chl/raw_pull/intermediate")
save.dir <- here::here("data-raw")
## list .nc files
fname<- list.files(raw.dir, pattern='*.nc',full.names=TRUE)

## Crop to NES
crs <- "+proj=longlat +lat_1=35 +lat_2=45 +lat_0=40
+lon_0=-77 +x_0=0 +y_0=0 +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"


## create and save raster stacks for each season
winter <- list.files(raw.dir, pattern = "03-OCCCI")
win_stack <- raster::stack(paste0(raw.dir,"/",winter))
raster::writeRaster(win_stack, filename=file.path(save.dir, "winter_chl.nc"), format = "CDF", overwrite = TRUE)

spring <- list.files(raw.dir, pattern = "06-OCCCI")
spr_stack <- raster::stack(paste0(raw.dir,"/",spring))
raster::writeRaster(spr_stack, filename=file.path(save.dir, "spring_chl.nc"), format = "CDF", overwrite = TRUE)

summer <- list.files(raw.dir, pattern = "09-OCCCI")
sum_stack <- raster::stack(paste0(raw.dir,"/",summer))
raster::writeRaster(sum_stack, filename=file.path(save.dir, "summer_chl.nc"), format = "CDF", overwrite = TRUE)

fall <- list.files(raw.dir, pattern = "12-OCCCI")
fal_stack <- raster::stack(paste0(raw.dir,"/",fall))
raster::writeRaster(fal_stack, filename=file.path(save.dir, "fall_chl.nc"), format = "CDF", overwrite = TRUE)


#############################################################################################################
#################### Primary Production #####################################################################
#############################################################################################################


## Grab saved files
raw.dir <- here::here("data-raw/pp")
int.dir <- here::here("data-raw/pp/raw_pull/intermediate")
save.dir <- here::here("data-raw")
## list .nc files
fname<- list.files(raw.dir, pattern='*.nc',full.names=TRUE)

## Crop to NES
crs <- "+proj=longlat +lat_1=35 +lat_2=45 +lat_0=40
+lon_0=-77 +x_0=0 +y_0=0 +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"


## create and save raster stacks for each season
winter <- list.files(raw.dir, pattern = "03-OCCCI")
win_stack <- raster::stack(paste0(raw.dir,"/",winter))
raster::writeRaster(win_stack, filename=file.path(save.dir, "winter_pp.nc"), format = "CDF", overwrite = TRUE)

spring <- list.files(raw.dir, pattern = "06-OCCCI")
spr_stack <- raster::stack(paste0(raw.dir,"/",spring))
raster::writeRaster(spr_stack, filename=file.path(save.dir, "spring_pp.nc"), format = "CDF", overwrite = TRUE)

summer <- list.files(raw.dir, pattern = "09-OCCCI")
sum_stack <- raster::stack(paste0(raw.dir,"/",summer))
raster::writeRaster(sum_stack, filename=file.path(save.dir, "summer_pp.nc"), format = "CDF", overwrite = TRUE)

fall <- list.files(raw.dir, pattern = "12-OCCCI")
fal_stack <- raster::stack(paste0(raw.dir,"/",fall))
raster::writeRaster(fal_stack, filename=file.path(save.dir, "fall_pp.nc"), format = "CDF", overwrite = TRUE)