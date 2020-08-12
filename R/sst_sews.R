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
   #moran    = Moran(raster(mat_coarse), na.rm = TRUE))
}

randomize_matrix_no_na <- function(mat) {
  mat[!is.na(mat)] <- sample(mat[!is.na(mat)])
  return(mat)
}

## Example arguements
epu_name <- "GB"
month_number <- 12


run_plot_sews<- function(epu_name, month_number){
  ## set dirs
  raw.dir<-here::here("data-raw/temp")
  gis.dir<-here::here("data-raw/gis")
  
  # projection
  crs <- "+proj=longlat +lat_1=35 +lat_2=45 +lat_0=40 +lon_0=-77+x_0=0+y_0=0 +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
  
  # Bring in epu polygons
  epu <- rgdal::readOGR(file.path(gis.dir, "EPU_NOESTUARIES.shp"), verbose = T)
  crs(epu) <- crs
  
  ### read in monthly mean oisst files 
  fname<- list.files(raw.dir, pattern='*.nc',full.names=TRUE)
  for (i in fname){
    #r_stack <- raster::stack(i)
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
  
  ### TEST CLOSE CROP SO AT LEAST IT WILL RUN
  close_crop_gb <- as(extent(-71, -66, 40, 42), 'SpatialPolygons')
  mon.test<-crop(mon, close_crop_gb)
  mon.test<-unstack(mon.test)
  ### MASK NOT WORKING 
  ## Cut to EPU of choice
   for (j in 1:length(mon)){
     mon1<-raster::mask(mon, epu[epu$EPU == epu_name,])
     mon1<-raster::unstack(mon1)
   }
  # for (j in 1:length(mon)){
  #   mon1<-raster::mask(mon, epu[epu$EPU == epu_name,])
  #   mon1<-raster::unstack(mon1)
  #   }

  ### FIX ME!!! ###
  ## Run SEWS analysis
  library(spatialwarningsGis)
  indices <- spatialwarnings::compute_indicator(mon.test, fun = na_aware_ews,
                                              cg_subsize = 2) ##### Erroring out "Error in check_mat(mat) : NAs in provided matrix."

  indices_test <- spatialwarnings::indictest(indices,
                                           nulln = 999,
                                           null_method = randomize_matrix_no_na)

  plot(indices_test)+ ggplot2::ggtitle(month_number, "oisst")


}















#### Try on season.nc
## Example arguements
epu_name <- "GB"
season <- "fall"


gis.dir<-here::here("data-raw/gis")
raw.dir<-here::here("data-raw")
plot.dir<-here::here("plots")
# Projection
crs <- "+proj=longlat +lat_1=35 +lat_2=45 +lat_0=40 +lon_0=-77+x_0=0+y_0=0 +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

# Bring in epu polygons
epu <- rgdal::readOGR(file.path(gis.dir, "EPU_NOESTUARIES.shp"), verbose = T)
crs(epu) <- crs

run_sst_sews<- function(epu_name, season){

# masked seasonal raster stack to epu
fname<- list.files(raw.dir, pattern = paste0(season,'_sst.nc'),full.names=TRUE)
for (i in fname){
  r_stack <- raster::stack(i)
  masked<-raster::mask(r_stack, epu[epu$EPU == epu_name,])
  masked<-raster::unstack(masked)
}

# Run spatial ews analysis
library(spatialwarningsGis)
indices <- spatialwarnings::compute_indicator(masked, fun = na_aware_ews,
                                              cg_subsize = 2) 

indices_test <- spatialwarnings::indictest(indices,
                                           nulln = 999,
                                           null_method = randomize_matrix_no_na)

plot(indices_test)+ ggplot2::ggtitle(paste(epu_name, season, "oisst"))

# Save plot
ggsave(filename = file.path("plots", paste0(epu_name,"_",season,"_OISST.png")))

# Save data
df <- tibble::enframe(indices) %>% 
  mutate(name = as.numeric(df$name)) %>% 
  mutate(Year = as.numeric(1981+(df$name))) %>% 
  separate(value, c("skewness", "variance", "moran"),  ",") %>% 
  separate(moran, c("before", "moran"),  "=") %>% 
  mutate(moran = stringr::str_sub(df$moran, end=-2)) %>% 
  separate(variance, c("before", "variance"), "=") %>% 
  separate(skewness, c("a","b", "skewness"),  "=") %>% 
  dplyr::select(-a, -b, -before) %>% 
  mutate(skewness = as.numeric(skewness), 
         variance = as.numeric(variance), 
         moran = as.numeric(moran))

write.csv(df, file=here::here("data", paste0(epu_name,"_",season,".csv")))
}




### Run function for all EPUs and Seasons

run_sst_sews("MAB", "winter")
run_sst_sews("MAB", "spring")
run_sst_sews("MAB", "summer")
run_sst_sews("MAB", "fall")

run_sst_sews("GB", "winter")
run_sst_sews("GB", "spring")
run_sst_sews("GB", "summer")
run_sst_sews("GB", "fall")

run_sst_sews("GOM", "winter")
run_sst_sews("GOM", "spring")
run_sst_sews("GOM", "summer")
run_sst_sews("GOM", "fall")
