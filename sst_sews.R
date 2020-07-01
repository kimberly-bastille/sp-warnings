
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

fname<-"mon_sst_2010_2019.nc"

r_stack <- raster::stack(file.path(raw.dir, fname))

crs(r_stack) <- "+proj=longlat +lat_1=35 +lat_2=45 +lat_0=40 +lon_0=-77 +x_0=0 +y_0=0 +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

## Sort raster layers by month
unst<-unstack(r_stack)
for (i in 1:length(unst)){
  ind<- unst[[i]]@data@names == stringr::str_detect(unst[[i]]@data@names, pattern = "\\d{4}\\.\\d{2}")
  data[ind]
}







## run spatial EWS
library(spatialwarningsGis)

r_layer <- raster::unstack(r_stack)
indices <- spatialwarnings::compute_indicator(r_layer, fun = na_aware_ews,
                                              cg_subsize = 2) 

indices_test <- spatialwarnings::indictest(indices,
                                           nulln = 999,
                                           null_method = randomize_matrix_no_na)

plot(indices_test)