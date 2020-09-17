# zooplankton analysis



#```{r, echo = T, eval = F}

#libraries
library(vegan)
library(stats)
library(mgcv)
library(reshape2)
library(readxl)
library(lubridate)
library(sp)
library(maptools)
library(marmap)
library(rgeos)
library(sf)
raw.dir<-here::here("data-raw/zoo/")
gis.dir<- here::here("data-raw/gis/")

crs <- "+proj=longlat +lat_1=35 +lat_2=45 +lat_0=40
+lon_0=-77 +x_0=0 +y_0=0 +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
# load data
#URL='ftp://ftp.nefsc.noaa.gov/pub/hydro/zooplankton_data/EcoMon_Plankton_Data_v3_0.xlsx'
#ZPD=openxlsx::read.xlsx(file.path(raw.dir,"EcoMon_Plankton_Data_v3_6.xlsx"), sheet='Data')

ZPD <- read.csv(file.path(raw.dir,"EcoMon_Plankton_Data_v3_6.csv"))
ZPD <- ZPD %>%
  tidyr::separate(date, c("day", "season", "year"),"-") 
  
ZPD$lat2=ceiling(ZPD$lat) #use for binning into 1 degree bins for removal of undersampled bins
ZPD$lon2=floor(ZPD$lon) #use for binning into 1 degree bins for removal of undersampled bins

# ASSIGN EPU based on GPS data
epus=rgdal::readOGR(file.path(gis.dir,"EPU_NOESTUARIES.shp"))
crs(epus)<-crs

coordinates(ZPD) <- ~lon+lat
crs(ZPD)  <- crs

a.data <- over(ZPD, epus)
a.data<- as.data.frame(a.data)
ZPD<- as.data.frame(ZPD)
ZPDb<- cbind(ZPD, a.data) 

dat<-ZPDb %>% pivot_longer(., 17:291, "zoo_name") %>% # convert to long format
  mutate(p.a = case_when(value > 0 ~ 1, # create presence absence column
                         value == 0 ~ 0)) %>% 
  drop_na(value) %>% # remove rows that have NAs
  dplyr::mutate(season = dplyr::recode(season, "Dec" = "winter"), # Convert months to assigned seasons
                season = dplyr::recode(season, "Jan" = "winter"), 
                season = dplyr::recode(season, "Feb" = "winter"),
                season = dplyr::recode(season, "Mar" = "spring"),
                season = dplyr::recode(season, "Apr" = "spring"),
                season = dplyr::recode(season, "May" = "spring"),
                season = dplyr::recode(season, "Jun" = "summer"),
                season = dplyr::recode(season, "Jul" = "summer"),
                season = dplyr::recode(season, "Aug" = "summer"),
                season = dplyr::recode(season, "Sep" = "fall"),
                season = dplyr::recode(season, "Oct" = "fall"),
                season = dplyr::recode(season, "Nov" = "fall")) %>% 
  filter(!EPU == "SS")

dat1<-dat%>% 
  dplyr::group_by(zoo_name, EPU, season) %>% 
  dplyr::mutate(count = sum(p.a), # Reduce to taxa occurrance > x (20) percent in samples
         total = length(p.a),
         pct = count/total * 100) %>% 
  dplyr::filter(pct > 20) %>%
  dplyr::filter(value > 0) %>% 
  dplyr::mutate(logvalue = log10(value)+1) %>%  # log transform data
  dplyr::group_by(EPU, season, zoo_name) %>% 
  dplyr::mutate(bmm = mean(logvalue), # calculate mean
                anom = logvalue - bmm)  %>% 
  dplyr::filter(zoo_name %in% c("calfin_10m2", "ctyp_10m2","cham_10m2","tlong_10m2","pseudo_10m2")) %>% # Small-Large body copepod abundance anomaly
  dplyr::mutate(zoo_name = dplyr::recode(zoo_name, "calfin_10m2" = "Large"), # Calanus finmarchicus
                zoo_name = dplyr::recode(zoo_name, "ctyp_10m2" = "Small"), #Pseudocalanus spp, Centropoges typicus, Centropages hamatus, Temora longicornis
                zoo_name = dplyr::recode(zoo_name, "cham_10m2" = "Small"),
                zoo_name = dplyr::recode(zoo_name, "tlong_10m2" = "Small"),
                zoo_name = dplyr::recode(zoo_name, "pseudo_10m2" = "Small")) %>% 
  dplyr::group_by(EPU, zoo_name, season, year) %>% 
  dplyr::summarise(anom1 = mean(anom)) %>% # take mean anom for small
  dplyr::ungroup() %>% 
  dplyr::mutate(year = as.integer(paste(year))) %>% 
  dplyr::mutate(year = ifelse(year > 75, year + 1900, year + 2000 ))
         
write.csv(dat1, file=here::here("data", paste0("zooplankton.csv")))
