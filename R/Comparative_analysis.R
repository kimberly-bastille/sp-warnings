# CCF 

library(broom)
library(dplyr)
library(stringr)
library(tidyverse)


######################################################################
########### Compare SST and PP #######################################
######################################################################

## Grab mean PPD for each season/year/EPU
pp <- ecodata::chl_pp %>% 
  filter(str_detect(Var, "MONTHLY_PPD"), 
         !str_detect(Time, "1997")) %>% 
  tidyr::extract(Time, into = c("Year", "Month"), "(.{4})(.{2})") %>% 
  mutate(
    Season = case_when(
      Month %in% c("10", "11", "12") ~ "fall",
      Month %in% c("01", "02", "03")  ~ "winter",
      Month %in% c("04", "05", "06")  ~ "spring",
      TRUE ~ "summer"), 
    Year = as.numeric(Year))%>% 
  dplyr::group_by(Season, EPU, Year) %>% 
  dplyr::summarise(mean = mean(Value)) %>% 
  dplyr::mutate(anom.mean = mean(mean) - mean) %>% 
  ungroup()


### This is comparing the mean PPD for each season to the Variance observed in SST. Is this right??
ccf_test<-function(epu_name, season_name, indicator){
  data.dir<-here::here("data//")
test<- pp %>% 
  filter(EPU == epu_name, 
         Season == season_name)

sews <- read.csv(file = paste0(data.dir, paste0(epu_name,"_",season_name,"_",indicator,".csv"))) %>% 
  left_join(test) %>% 
  dplyr::mutate(anom.var = mean(variance) - variance, 
                anom.skew = mean(skewness) - skewness) 

ccf.values <- ccf( sews$anom.var, sews$anom.mean)
ccf.df<-data.frame(lag=ccf.values$lag, CCF=ccf.values$acf) %>% 
  dplyr::mutate(val = abs(CCF)) %>% 
  dplyr::filter(val >= 0.42) %>% 
  dplyr::select(!val) %>% 
  dplyr::mutate("var~index")


ccf.values2 <- ccf( sews$anom.skew, sews$anom.mean)
ccf.df2<-data.frame(lag=ccf.values2$lag, CCF=ccf.values2$acf) %>% 
  dplyr::mutate(val = abs(CCF)) %>% 
  filter(val >= 0.42) %>% 
  select(!val)%>% 
  dplyr::mutate("skew~index") %>% 
  rbind(ccf.df)

sews2<- sews %>% 
  tidyr::pivot_longer( cols = starts_with("anom"), 
                       names_to = "Variable", 
                       values_to = "Values")
  
p1<- sews2 %>%
  filter(EPU == epu_name,
         Season == season_name) %>%
  ggplot()+
  geom_line(aes(x = Year, y = Values))+
  geom_point(aes(x = Year, y = Values))+
  facet_wrap(.~Variable, ncol = 1, scales = "free")+
  annotation_custom(tableGrob(ccf.df), xmin=1998, xmax=2000, ymin=0, ymax=0.1)+
  ggtitle(paste(indicator, epu_name, season_name ))
p1

head(ccf.df2)
}

##### What does this mean??
ccf_test("MAB", "winter", "sst") #NO
ccf_test("MAB", "spring", "sst") # NO
ccf_test("MAB", "summer", "sst") # NO
ccf_test("MAB", "fall", "sst") #NO

ccf_test("GB", "winter", "sst") # yes at 10
ccf_test("GB", "spring", "sst") # NO
ccf_test("GB", "summer", "sst") #yes at -1 and 9
ccf_test("GB", "fall", "sst")  #  yes at 1

ccf_test("GOM", "winter", "sst") # Yes at 3
ccf_test("GOM", "spring", "sst") # NO
ccf_test("GOM", "summer", "sst") # NO
ccf_test("GOM", "fall", "sst") # NO


###########################################################################
############## Compare PPD and ZOO ########################################
###########################################################################

ccf_test2<-function(epu_name, season_name, indicator){
zoo <- read.csv(here::here("data/zooplankton.csv")) %>% 
    filter(EPU == epu_name,
           season == season_name,
           zoo_name == "Large")

pp <- read.csv(file = paste0(data.dir, paste0(epu_name,"_",season_name,"_",indicator,".csv"))) 

intsect<-intersect(zoo$year, pp$Year)

pp1<- pp %>% filter(Year %in% intsect)
zoo1<-zoo %>% filter(year %in% intsect)

cc <- ccf(pp1$variance, zoo1$anom1)
print(cc)


p1<- zoo1 %>%
  ggplot()+
  geom_line(aes(x =  year, y = anom1, color = "Zoo abundance"))+
  geom_line(aes(x= pp1$Year, y = pp1$variance, color = "PP Variance"))+
  geom_line(aes(x= pp1$Year, y = pp1$skewness, color = "PP Skewness"))+
  ggtitle(paste("PPD~Zoo CCF", epu_name, season_name ))
p1

}

##### What does this mean??
ccf_test2("MAB", "winter", "pp") # NO 
ccf_test2("MAB", "spring", "pp") # NO
ccf_test2("MAB", "summer", "pp") # 1 and 2 
ccf_test2("MAB", "fall", "pp") # NO

ccf_test2("GB", "winter", "pp") # NO
ccf_test2("GB", "spring", "pp") # 0
ccf_test2("GB", "summer", "pp") # -2
ccf_test2("GB", "fall", "pp")  #  -3

ccf_test2("GOM", "winter", "pp") # NO
ccf_test2("GOM", "spring", "pp") # yes
ccf_test2("GOM", "summer", "pp") # No
ccf_test2("GOM", "fall", "pp") # -5 



















