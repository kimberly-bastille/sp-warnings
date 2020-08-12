# Download OISST for NEUS
## From: https://cran.r-project.org/web/packages/heatwaveR/vignettes/OISST_preparation.html




# The packages we will need
# install.packages("dplyr")
# install.packages("lubridate")
# install.packages("ggplot2")
# install.packages("tidync")
# install.packages("doParallel")
# install.packages("rerddap")
# install.packages("plyr") # Note that this library should never be loaded, only installed

# The packages we will use
library(dplyr) # A staple for modern data management in R
library(lubridate) # Useful functions for dealing with dates
library(ggplot2) # The preferred library for data visualisation
library(tidync) # For easily dealing with NetCDF data
library(rerddap) # For easily downloading subsets of data
library(doParallel) # For parallel processing
library(tidyr)
library(spatialwarnings)
library(spatialwarningsGis)
library(ecodata)

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


# info("ncdcOisst21Agg_LonPM180")

daily_sst <- griddap(info("ncdcOisst21Agg_LonPM180"),
                     url = "https://coastwatch.pfeg.noaa.gov/erddap/",
                     time = c("1982-01-01", "1982-12-31"),
                     zlev = c(0, 0),
                     latitude = c(35, 45),
                     longitude = c(-65, -77),
                     fields = "sst")

nc_sst <-
  daily_sst$summary$filename %>%
  tidync()


df_sst <- daily_sst$summary$filename %>%
  tidync() %>% 
  activate("sst") %>%
  hyper_tibble() %>%
  mutate(date = as_date(as_datetime(time)),
         year_month = format(date, "%Y-%m")) %>%
  group_by(year_month, longitude, latitude) %>%
  summarize(mean_sst = mean(sst, na.rm = TRUE),
            CV_sst = sd(sst, na.rm = TRUE)/mean_sst)


df_nest <- df_sst %>%
  dplyr::select(year_month,
                x = longitude,
                y = latitude,
                z = mean_sst) %>%
  group_by(year_month) %>%
  tidyr::nest()

date_list <- unique(df_sst$year_month)
df_l <- lapply(1:length(date_list), function(x) raster::rasterFromXYZ(df_nest$data[[x]]))


#Read in EPU shapefile (will be downsampled to match OI raster resolution)
epu <- ecodata::epu_sf

td_mask <- lapply(1:length(date_list), function(x) raster::mask(df_l[[x]], epu[epu$EPU == "GB", ]))
raster::plot(td_mask[[3]])

# remotes::install_github("spatial-ews/spatialwarningsGis")
indices <- spatialwarnings::compute_indicator(td_mask, fun = na_aware_ews,
                                              cg_subsize = 2) ##### Erroring out "Error in check_mat(mat) : NAs in provided matrix."

indices_test <- spatialwarnings::indictest(indices,
                                           nulln = 999,
                                           null_method = randomize_matrix_no_na)

plot(indices)

plot(indices_test)