
### Needed functions
na_aware_ews <- function(mat, cg_subsize) { 
  require(moments) # required packages
  require(raster)
  mat_coarse <- coarse_grain(mat, cg_subsize)
  c(skewness = skewness(as.vector(mat_coarse), na.rm = TRUE), 
    variance = var(as.vector(mat_coarse), na.rm = TRUE), 
    moran    = Moran(raster(mat_coarse)))
}

randomize_matrix_no_na <- function(mat) {
  mat[!is.na(mat)] <- sample(mat[!is.na(mat)])
  return(mat)
}


### read in monthly mean oisst files 
raw.dir<-here::here("temp/data-raw")
i<- here::here("temp/data-raw/mon_sst_1985_1989.nc")

fname<- list.files(raw.dir, pattern='*.nc',full.names=TRUE)
outstack<-raster::stack()
for (i in fname){
  r_stack <- raster::stack(fname)
  raster::crs(r_stack) <- "+proj=longlat +lat_1=35 +lat_2=45 +lat_0=40 +lon_0=-77 +x_0=0 +y_0=0 +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
  r_layer<-raster::unstack(r_stack) }


## Sort raster layers by month
#r_layer[[k]]@z[[1]] ## date defined here


jan1 <- lapply(1:length(r_layer), function(x)  stringr::str_detect(r_layer[[x]]@z[[1]], pattern =  "\\d{4}\\-01\\-\\d{2}")) ## returns true false

jan <- lapply(1:length(r_layer), function(x)  dplyr::select(r_layer[[x]] == stringr::str_detect(r_layer[[x]]@z[[1]], ## error using select with rasters
                                                                                                     pattern =  "\\d{4}\\-01\\-\\d{2}")))


## run spatial EWS
library(spatialwarningsGis)

r_layer <- raster::unstack(r_stack)
indices <- spatialwarnings::compute_indicator(r_layer, fun = na_aware_ews,
                                              cg_subsize = 2) 

indices_test <- spatialwarnings::indictest(indices,
                                           nulln = 999,
                                           null_method = randomize_matrix_no_na)

plot(indices_test)