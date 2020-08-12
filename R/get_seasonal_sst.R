library(dplyr)
library(raster)
library(sf)
library(ggplot2)
library(ncdf4)
library(reshape2)



## Grab saved files
raw.dir <- here::here("data-raw/temp/raw_pull")
int.dir <- here::here("data-raw/temp/raw_pull/intermediate")
save.dir <- here::here("data-raw")
## list .nc files
fname<- list.files(raw.dir, pattern='*.nc',full.names=TRUE)

## Crop to NES
crs <- "+proj=longlat +lat_1=35 +lat_2=45 +lat_0=40
+lon_0=-77 +x_0=0 +y_0=0 +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

for (i in fname){
  # get year from fname
  year <- stringr::str_extract(i, pattern = "[0-9]{4}")
  # Create stack for each file
  r_stack <- raster::stack(i)
  #crop and project each stack
  r_stack<-raster::crop(r_stack, extent(280,300,30,50))
  raster::crs(r_stack) <- crs
  # Break each file into seasons / get mean for season and rotate raster
  winter.sst <- r_stack[[1:90]]
  winter.sst <- raster::stackApply(winter.sst, indices = rep(1,nlayers(winter.sst)),mean)
  winter.sst <- raster::rotate(winter.sst)
  spring.sst <- r_stack[[91:181]]
  spring.sst <- raster::stackApply(spring.sst, indices = rep(1,nlayers(spring.sst)),mean)
  spring.sst <- raster::rotate(spring.sst)
  summer.sst <- r_stack[[182:273]]
  summer.sst <- raster::stackApply(summer.sst, indices = rep(1,nlayers(summer.sst)),mean)
  summer.sst <- raster::rotate(summer.sst)
  fall.sst <- r_stack[[274:365]]
  fall.sst <- raster::stackApply(fall.sst, indices = rep(1,nlayers(fall.sst)),mean)
  fall.sst <- raster::rotate(fall.sst)

  # create rasters with year in name
  raster::writeRaster(winter.sst, filename=file.path(int.dir, paste0(year, "winter.nc")), format = "CDF", overwrite = TRUE)
  raster::writeRaster(spring.sst, filename=file.path(int.dir, paste0(year, "spring.nc")), format = "CDF", overwrite = TRUE)
  raster::writeRaster(summer.sst, filename=file.path(int.dir, paste0(year, "summer.nc")), format = "CDF", overwrite = TRUE)
  raster::writeRaster(fall.sst, filename=file.path(int.dir, paste0(year, "fall.nc")), format = "CDF", overwrite = TRUE)
}


## Grab all files containing winter and write to raster stack
winter<- list.files(int.dir, pattern='*winter.nc',full.names=TRUE)
win_stack <- raster::stack(winter)
raster::writeRaster(win_stack, filename=file.path(save.dir, "winter_sst.nc"), format = "CDF", overwrite = TRUE)

spring<- list.files(int.dir, pattern='*spring.nc',full.names=TRUE)
spr_stack <- raster::stack(spring)
raster::writeRaster(spr_stack, filename=file.path(save.dir, "spring_sst.nc"), format = "CDF", overwrite = TRUE)

summer<- list.files(int.dir, pattern='*summer.nc',full.names=TRUE)
sum_stack <- raster::stack(summer)
raster::writeRaster(sum_stack, filename=file.path(save.dir, "summer_sst.nc"), format = "CDF", overwrite = TRUE)

fall<- list.files(int.dir, pattern='*fall.nc',full.names=TRUE)
fall_stack <- raster::stack(fall)
raster::writeRaster(fall_stack, filename=file.path(save.dir, "fall_sst.nc"), format = "CDF", overwrite = TRUE)

























########################################################################################
########################  TEST DOWNLOAD  #############################################
######################################################################################
library(dplyr) # A staple for modern data management in R
library(lubridate) # Useful functions for dealing with dates
library(ggplot2) # The preferred library for data visualisation
library(tidync) # For easily dealing with NetCDF data
library(rerddap) # For easily downloading subsets of data
library(doParallel)

daily_sst <- griddap(info("ncdcOisst21Agg_LonPM180"),
                     url = "https://coastwatch.pfeg.noaa.gov/erddap/",
                     time = c("1982-01-01", "1982-12-31"),
                     zlev = c(0, 0),
                     latitude = c(35, 45),
                     longitude = c(-65, -77),
                     fields = "sst")

 

