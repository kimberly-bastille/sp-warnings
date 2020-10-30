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

# epu_name<-"MAB"
# indicator<-"sst"
# year_start<-1983

run_sews_monthly<- function(epu_name, indicator, year_start){
  library(raster)
  library(sf)
  library(tidyverse)
  # masked seasonal raster stack to epu
  fname<- list.files(raw.dir, pattern = paste0("monthly_",indicator,'.nc'),full.names=TRUE)
  for (i in fname){
    r_stack <- raster::stack(i)
    r_stack1<- raster::rotate(r_stack)
    masked<-raster::mask(r_stack1, epu[epu$EPU == epu_name,])
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
                  #moran = as.numeric(moran),
                  EPU = as.character(epu_name), 
                  Month = as.numeric(Name),
                  Year = as.numeric(rep(1982:2019, each=12)))
  
  write.csv(df, file=here::here("data", paste0(epu_name,"_",indicator,".csv")))
}


### Run function for all EPUs and indicators

## SST
run_sews_monthly("MAB", "sst", 1983)
run_sews_monthly("GB", "sst", 1983)
run_sews_monthly("GOM", "sst", 1983)