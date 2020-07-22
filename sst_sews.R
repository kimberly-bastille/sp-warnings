library(raster)
library(sf)
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


run_plot_sews<- function(epu_name, month_number){
  ## set dirs
  raw.dir<-here::here("data-raw/temp")
  gis.dir<-here::here("data-raw/gis")
  
  # projection
  crs <- "+proj=longlat +lat_1=35 +lat_2=45 +lat_0=40 +lon_0=-77+x_0=0+y_0=0 +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
  
  # Bring in epu polygons
  epu <- rgdal::readOGR(file.path(gis.dir, "EPU_NOESTUARIES.shp"), verbose = T)
  crs(epu_shp) <- crs
  
  ### read in monthly mean oisst files 
  fname<- list.files(raw.dir, pattern='*.nc',full.names=TRUE)
  for (i in fname){
    r_stack <- raster::stack(fname)
    raster::crop(r_stack, extent(280,300,30,50))
    r_stack <- raster::rotate(r_stack)
    raster::crs(r_stack) <- crs
    r_layer<-raster::unstack(r_stack) 
    }
  
  ## Sort raster layers by month
  #r_layer[[k]]@z[[1]] ## date defined here
  monthlyRasters <- vector("list",12)
  for (k in 1:length(r_layer)){
    month <- lubridate::month(r_layer[[k]]@z[[1]])
    monthlyRasters[[month]] <- c(monthlyRasters[[month]],r_layer[[k]])
  }
  
  ## Select the month to be plotted
  mon<-raster::stack(monthlyRasters[[month_number]])
  
  ## Cut to EPU of choice
  for (j in 1:length(mon)){
    mon1<-raster::mask(mon, epu[epu$EPU == epu_name,])
    mon1<-raster::unstack(mon1)
    }
  
  ## Run SEWS analysis
  indices <- spatialwarnings::compute_indicator(mon1, fun = na_aware_ews,
                                              cg_subsize = 2) ##### Erroring out "Error in check_mat(mat) : NAs in provided matrix."

  indices_test <- spatialwarnings::indictest(indices,
                                           nulln = 999,
                                           null_method = randomize_matrix_no_na)

  plot(indices_test)+ ggtitle(month_number, "oisst")

}