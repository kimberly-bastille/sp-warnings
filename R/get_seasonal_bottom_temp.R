#### 

#20+ folders containing 365 daily bottom temp data in net CDFS.
library(dplyr)
library(googledrive)
library(raster)

raw.dir<- here::here("data-raw/bottom_temp/raw-pull/")
int.dir<- here::here("data-raw/bottom_temp/intermediate")
save.dir <- here::here("data-raw")

i <- 2018
## list .nc files
folderlist<-c(1994:2018)
for (i in folderlist){
fname<- list.files(paste0(raw.dir,"/",i,"/"), pattern='*.nc',full.names=TRUE)

## Crop to NES
crs <- "+proj=longlat +lat_1=35 +lat_2=45 +lat_0=40
+lon_0=-77 +x_0=0 +y_0=0 +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

for (k in fname){
  # get year from fname
  year <- stringr::str_extract(k, pattern = "[0-9]{4}")
  # Create stack for each file
  ## Winter
  win<- list(fname[1:90])
  win <- raster::stack(win)
  winter <- raster::stackApply(win, indices = rep(1,nlayers(win)),mean)
  ## Winter
  spr<- list(fname[91:181])
  spr <- raster::stack(spr)
  spring <- raster::stackApply(spr, indices = rep(1,nlayers(spr)),mean)
  ## Winter
  summ<- list(fname[182:273])
  summ <- raster::stack(summ)
  summer <- raster::stackApply(summ, indices = rep(1,nlayers(summ)),mean)
  ## Winter
  fall<- list(fname[274:max(length(fname))])
  fall <- raster::stack(fall)
  fall <- raster::stackApply(fall, indices = rep(1,nlayers(fall)),mean)
  
  # create rasters with year in name
  raster::writeRaster(winter, filename=file.path(int.dir, paste0(year, "winter.nc")), format = "CDF", overwrite = TRUE)
  raster::writeRaster(spring, filename=file.path(int.dir, paste0(year, "spring.nc")), format = "CDF", overwrite = TRUE)
  raster::writeRaster(summer, filename=file.path(int.dir, paste0(year, "summer.nc")), format = "CDF", overwrite = TRUE)
  raster::writeRaster(fall, filename=file.path(int.dir, paste0(year, "fall.nc")), format = "CDF", overwrite = TRUE)
}
}



## Grab all files containing winter and write to raster stack
winter<- list.files(int.dir, pattern='*winter.nc',full.names=TRUE)
win_stack <- raster::stack(winter)
raster::writeRaster(win_stack, filename=file.path(save.dir, "winter_bt.nc"), format = "CDF", overwrite = TRUE)

spring<- list.files(int.dir, pattern='*spring.nc',full.names=TRUE)
spr_stack <- raster::stack(spring)
raster::writeRaster(spr_stack, filename=file.path(save.dir, "spring_bt.nc"), format = "CDF", overwrite = TRUE)

summer<- list.files(int.dir, pattern='*summer.nc',full.names=TRUE)
sum_stack <- raster::stack(summer)
raster::writeRaster(sum_stack, filename=file.path(save.dir, "summer_bt.nc"), format = "CDF", overwrite = TRUE)

fall<- list.files(int.dir, pattern='*fall.nc',full.names=TRUE)
fall_stack <- raster::stack(fall)
raster::writeRaster(fall_stack, filename=file.path(save.dir, "fall_bt.nc"), format = "CDF", overwrite = TRUE)



  
          
          