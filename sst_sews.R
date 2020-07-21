
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



raw.dir<-here::here("temp/data-raw")
#i<- here::here("temp/data-raw/mon_sst_1985_1989.nc")
epu <- ecodata::epu_sf 
epu_name <- "GB"

### read in monthly mean oisst files 
fname<- list.files(raw.dir, pattern='*.nc',full.names=TRUE)

for (i in fname){
  r_stack <- raster::stack(fname)
  raster::crs(r_stack) <- "+proj=longlat +lat_1=35 +lat_2=45 +lat_0=40 +lon_0=-77 +x_0=0 +y_0=0 +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
  r_layer<-raster::unstack(r_stack) }

epu <- ecodata::epu_sf %>%
  filter(EPU != "SS")

## Sort raster layers by month
#r_layer[[k]]@z[[1]] ## date defined here
monthlyRasters <- vector("list",12)
for (k in 1:length(r_layer)){
  month <- lubridate::month(r_layer[[k]]@z[[1]])
  monthlyRasters[[month]] <- c(monthlyRasters[[month]],r_layer[[k]])
  
}

mon<-monthlyRasters[[12]]



#stack_mon<-raster::stack(mon)
#mask<-raster::mask(stack_mon, epu[epu$EPU == epu_name,])## crop to area
#mon2<-raster::unstack(mask)
indices <- spatialwarnings::compute_indicator(mon, fun = na_aware_ews,
                                              cg_subsize = 2) 
indices_test <- spatialwarnings::indictest(indices,
                                           nulln = 999,
                                           null_method = randomize_matrix_no_na)
plot(indices_test)+ ggtitle("Dec oisst")
## run spatial EWS
library(spatialwarningsGis)

for (j in 1:12){
indices <- spatialwarnings::compute_indicator(monthlyRasters[[j]], fun = na_aware_ews,
                                              cg_subsize = 2) 

indices_test <- spatialwarnings::indictest(indices,
                                           nulln = 999,
                                           null_method = randomize_matrix_no_na)

plot(indices_test)
}
