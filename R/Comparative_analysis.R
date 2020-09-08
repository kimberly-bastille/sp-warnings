# CCF 

library(broom)
library(dplyr)

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
      TRUE ~ "summer"))%>% 
  dplyr::group_by(Season, EPU, Year) %>% 
  dplyr::summarise(mean = mean(Value)) %>% 
  ungroup()

### This is comparing the mean PPD for each season to the Variance observed in SST. Is this right??
ccf_test<-function(epu_name, season_name, indicator){
  data.dir<-here::here("data//")
pp.test<- pp %>% 
  filter(EPU == epu_name, 
         Season == season_name)
sst <- read.csv(file = paste0(data.dir, paste0(epu_name,"_",season_name,"_",indicator,".csv"))) %>% 
  filter(Year > 1998)

cc <- ccf(sst$variance, pp.test$mean)
cc
}

##### What does this mean??
ccf_test("MAB", "winter", "pp_log") #NO
ccf_test("MAB", "spring", "pp_log") # yes at 0
ccf_test("MAB", "summer", "pp_log") # yes at 7
ccf_test("MAB", "fall", "pp_log") #NO

ccf_test("GB", "winter", "pp_log") # yes at -2
ccf_test("GB", "spring", "pp_log") # yes at 1
ccf_test("GB", "summer", "pp_log") #yes at 0
ccf_test("GB", "fall", "pp_log")  # NO

ccf_test("GOM", "winter", "pp_log") # NO
ccf_test("GOM", "spring", "pp_log") # yes at 5 - 6
ccf_test("GOM", "summer", "pp_log") # NO
ccf_test("GOM", "fall", "pp_log") # NO
