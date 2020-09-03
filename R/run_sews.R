library(raster)
library(sf)
library(tidyverse)
library(dplyr)
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


gis.dir<-here::here("data-raw/gis")
raw.dir<-here::here("data-raw")
plot.dir<-here::here("plots")
# Projection
crs <- "+proj=longlat +lat_1=35 +lat_2=45 +lat_0=40 +lon_0=-77+x_0=0+y_0=0 +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

# Bring in epu polygons
epu <- rgdal::readOGR(file.path(gis.dir, "EPU_NOESTUARIES.shp"), verbose = T)
crs(epu) <- crs

epu_name<-"MAB"
season<-"winter"
indicator<-"par"
year_start<-1998

run_sews<- function(epu_name, season, indicator, year_start){
  library(raster)
  library(sf)
  library(tidyverse)
  # masked seasonal raster stack to epu
  fname<- list.files(raw.dir, pattern = paste0(season,'_',indicator,'.nc'),full.names=TRUE)
  for (i in fname){
    r_stack <- raster::stack(i)
    masked<-raster::mask(r_stack, epu[epu$EPU == epu_name,])
    masked<-raster::unstack(masked)
  }
  
  # Run spatial ews analysis
  library(spatialwarningsGis)
  indices <- spatialwarnings::compute_indicator(masked, fun = na_aware_ews,
                                                cg_subsize = 2) 
  
  #indices_test <- spatialwarnings::indictest(indices,
  #                                           nulln = 999,
  #                                           null_method = randomize_matrix_no_na)
  
  #plot(indices_test)+ ggplot2::ggtitle(paste(epu_name, season, indicator))
  
  # Save plot
  #ggplot2::ggsave(filename = file.path("plots", paste0(epu_name,"_",season,"_",indicator,".png")))
  
  # Save data
  library(dplyr)
  df <- tibble::enframe(indices) %>% 
    dplyr::mutate(Name = as.numeric(df$name)) %>% 
    dplyr::mutate(Year = as.numeric(year_start + (df$name)-1)) %>% 
    tidyr::separate(value, c("skewness", "variance", "moran"),  ",") %>% 
    tidyr::separate(moran, c("before", "moran"),  "=") %>% 
    dplyr::mutate(moran = stringr::str_sub(df$moran, end=-2)) %>% 
    tidyr::separate(variance, c("before", "variance"), "=") %>% 
    tidyr::separate(skewness, c("a","b", "skewness"),  "=") %>% 
    dplyr::select(-a, -b, -before) %>% 
    dplyr::mutate(skewness = as.numeric(skewness), 
                  variance = as.numeric(variance), 
                  moran = as.numeric(moran),
                  EPU = as.character(epu_name), 
                  season = as.character(season))
  
  write.csv(df, file=here::here("data", paste0(epu_name,"_",season,"_",indicator,".csv")))
}




### Run function for all EPUs and Seasons and indicators

## CHLa
run_sews("MAB", "winter", "chl", 1998)
run_sews("MAB", "spring", "chl", 1998)
run_sews("MAB", "summer", "chl", 1998)
run_sews("MAB", "fall", "chl", 1998)

run_sews("GB", "winter", "chl", 1998)
run_sews("GB", "spring", "chl", 1998)
run_sews("GB", "summer", "chl", 1998)
run_sews("GB", "fall", "chl", 1998)

run_sews("GOM", "winter", "chl", 1998)
run_sews("GOM", "spring", "chl", 1998)
run_sews("GOM", "summer", "chl", 1998)
run_sews("GOM", "fall", "chl", 1998)

## PP
run_sews("MAB", "winter", "pp", 1998)
run_sews("MAB", "spring", "pp", 1998)
run_sews("MAB", "summer", "pp", 1998)
run_sews("MAB", "fall", "pp", 1998)

run_sews("GB", "winter", "pp", 1998)
run_sews("GB", "spring", "pp", 1998)
run_sews("GB", "summer", "pp", 1998)
run_sews("GB", "fall", "pp", 1998)

run_sews("GOM", "winter", "pp", 1998)
run_sews("GOM", "spring", "pp", 1998)
run_sews("GOM", "summer", "pp", 1998)
run_sews("GOM", "fall", "pp", 1998)

## CHLa Log
run_sews("MAB", "winter", "chl_log", 1998)
run_sews("MAB", "spring", "chl_log", 1998)
run_sews("MAB", "summer", "chl_log", 1998)
run_sews("MAB", "fall", "chl_log", 1998)

run_sews("GB", "winter", "chl_log", 1998)
run_sews("GB", "spring", "chl_log", 1998)
run_sews("GB", "summer", "chl_log", 1998)
run_sews("GB", "fall", "chl_log", 1998)

run_sews("GOM", "winter", "chl_log", 1998)
run_sews("GOM", "spring", "chl_log", 1998)
run_sews("GOM", "summer", "chl_log", 1998)
run_sews("GOM", "fall", "chl_log", 1998)

## PP Log
run_sews("MAB", "winter", "pp_log", 1998)
run_sews("MAB", "spring", "pp_log", 1998)
run_sews("MAB", "summer", "pp_log", 1998)
run_sews("MAB", "fall", "pp_log", 1998)

run_sews("GB", "winter", "pp_log", 1998)
run_sews("GB", "spring", "pp_log", 1998)
run_sews("GB", "summer", "pp_log", 1998)
run_sews("GB", "fall", "pp_log", 1998)

run_sews("GOM", "winter", "pp_log", 1998)
run_sews("GOM", "spring", "pp_log", 1998)
run_sews("GOM", "summer", "pp_log", 1998)
run_sews("GOM", "fall", "pp_log", 1998)

## SST
run_sews("GOM", "winter", "sst", 1983)
run_sews("GOM", "spring", "sst", 1983)
run_sews("GOM", "summer", "sst", 1983)
run_sews("GOM", "fall", "sst", 1983)

run_sews("GB", "winter", "sst", 1983)
run_sews("GB", "spring", "sst", 1983)
run_sews("GB", "summer", "sst", 1983)
run_sews("GB", "fall", "sst", 1983)

run_sews("MAB", "winter", "sst", 1983)
run_sews("MAB", "spring", "sst", 1983)
run_sews("MAB", "summer", "sst", 1983)
run_sews("MAB", "fall", "sst", 1983)


## Bottom Temp
run_sews("GOM", "winter", "bt", 1994)
run_sews("GOM", "spring", "bt", 1994)
run_sews("GOM", "summer", "bt", 1994)
run_sews("GOM", "fall", "bt", 1994)

run_sews("GB", "winter", "bt", 1994)
run_sews("GB", "spring", "bt", 1994)
run_sews("GB", "summer", "bt", 1994)
run_sews("GB", "fall", "bt", 1994)

run_sews("MAB", "winter", "bt", 1994)
run_sews("MAB", "spring", "bt", 1994)
run_sews("MAB", "summer", "bt", 1994)
run_sews("MAB", "fall", "bt", 1994)



## PAR
run_sews("MAB", "winter", "par", 1998)
run_sews("MAB", "spring", "par", 1998)
run_sews("MAB", "summer", "par", 1998)
run_sews("MAB", "fall", "par", 1998)

run_sews("GB", "winter", "par", 1998)
run_sews("GB", "spring", "par", 1998)
run_sews("GB", "summer", "par", 1998)
run_sews("GB", "fall", "par", 1998)

run_sews("GOM", "winter", "par", 1998)
run_sews("GOM", "spring", "par", 1998)
run_sews("GOM", "summer", "par", 1998)
run_sews("GOM", "fall", "par", 1998)