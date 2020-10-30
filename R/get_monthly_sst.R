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
fname<- list.files(raw.dir, pattern='*sst.nc',full.names=TRUE)

## Crop to NES
crs <- "+proj=longlat +lat_1=35 +lat_2=45 +lat_0=40
+lon_0=-77 +x_0=0 +y_0=0 +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

i <- "C:/Users/kimberly.bastille/Desktop/Rgghhh/sp-warnings-clean/data-raw/temp/raw_pull/sst.day.mean.2019.nc"

for (i in fname){
  # get year from fname
  year <- stringr::str_extract(i, pattern = "[0-9]{4}")
  # Create stack for each file
  r_stack <- raster::stack(i)
  #crop and project each stack
  r_stack<-raster::crop(r_stack, extent(280,300,30,50))
  raster::crs(r_stack) <- crs
  # Break each file into seasons / get mean for season and rotate raster
  #jan
  jan.sst <- r_stack[[1:31]]
  jan.sst <- raster::stackApply(jan.sst, indices = rep(1,nlayers(jan.sst)),mean)
  #jan.sst <- raster::rotate(jan.sst)
  #feb
  feb.sst <- r_stack[[32:59]]
  feb.sst <- raster::stackApply(feb.sst, indices = rep(1,nlayers(feb.sst)),mean)
  #feb.sst <- raster::rotate(feb.sst)
  #mar
  mar.sst <- r_stack[[60:90]]
  mar.sst <- raster::stackApply(mar.sst, indices = rep(1,nlayers(mar.sst)),mean)
  #mar.sst <- raster::rotate(mar.sst)
  #apr
  apr.sst <- r_stack[[91:120]]
  apr.sst <- raster::stackApply(apr.sst, indices = rep(1,nlayers(apr.sst)),mean)
  #apr.sst <- raster::rotate(apr.sst)
  #may
  may.sst <- r_stack[[121:151]]
  may.sst <- raster::stackApply(may.sst, indices = rep(1,nlayers(may.sst)),mean)
  #may.sst <- raster::rotate(may.sst)
  #jun
  jun.sst <- r_stack[[152:181]]
  jun.sst <- raster::stackApply(jun.sst, indices = rep(1,nlayers(jun.sst)),mean)
  #jun.sst <- raster::rotate(jun.sst)
  #jul
  jul.sst <- r_stack[[182:212]]
  jul.sst <- raster::stackApply(jul.sst, indices = rep(1,nlayers(jul.sst)),mean)
  #jul.sst <- raster::rotate(jul.sst)
  #aug
  aug.sst <- r_stack[[213:243]]
  aug.sst <- raster::stackApply(aug.sst, indices = rep(1,nlayers(aug.sst)),mean)
  #aug.sst <- raster::rotate(aug.sst)
  #sep
  sep.sst <- r_stack[[244:273]]
  sep.sst <- raster::stackApply(sep.sst, indices = rep(1,nlayers(sep.sst)),mean)
  #sep.sst <- raster::rotate(sep.sst)
  #oct
  oct.sst <- r_stack[[274:304]]
  oct.sst <- raster::stackApply(oct.sst, indices = rep(1,nlayers(oct.sst)),mean)
  #oct.sst <- raster::rotate(oct.sst)
  #nov
  nov.sst <- r_stack[[304:334]]
  nov.sst <- raster::stackApply(nov.sst, indices = rep(1,nlayers(nov.sst)),mean)
  #nov.sst <- raster::rotate(nov.sst)
  #dec
  dec.sst <- r_stack[[334:365]]
  dec.sst <- raster::stackApply(dec.sst, indices = rep(1,nlayers(dec.sst)),mean)
  #dec.sst <- raster::rotate(dec.sst)
  
  monthly_sst_stack <- raster::stack(jan.sst, feb.sst, mar.sst,
                                     apr.sst, may.sst, jun.sst,
                                     jul.sst, aug.sst, sep.sst, 
                                     oct.sst, nov.sst, dec.sst) 
  
  # create rasters with year in name
  raster::writeRaster(monthly_sst_stack, filename=file.path(int.dir, paste0(year,"monthly_sst.nc")), format = "CDF", overwrite = TRUE)

}


## Grab all files containing winter and write to raster stack
monthly_sst<- list.files(int.dir, pattern='*monthly_sst.nc',full.names=TRUE)
mon_stack <- raster::stack(monthly_sst)
raster::writeRaster(mon_stack, filename=file.path(save.dir, "monthly_sst.nc"), format = "CDF", overwrite = TRUE)

























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



