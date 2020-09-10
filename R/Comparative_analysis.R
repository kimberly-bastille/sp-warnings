# CCF 

library(broom)
library(dplyr)


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
  ungroup()


### This is comparing the mean PPD for each season to the Variance observed in SST. Is this right??
ccf_test<-function(epu_name, season_name, indicator){
  data.dir<-here::here("data//")
test<- pp %>% 
  filter(EPU == epu_name, 
         Season == season_name)

sst <- read.csv(file = paste0(data.dir, paste0(epu_name,"_",season_name,"_",indicator,".csv"))) %>% 
  filter(Year > 1998)

cc <- ccf(sst$variance, test$mean)
print(cc)
#p2<-plot(cc)
  
p1<- pp %>%
  filter(EPU == epu_name,
         Season == season_name) %>%
  ggplot()+
  geom_line(aes(x = Year, y = mean, color = "Mean pp"))+
  geom_line(aes(x= indA$Year, y = indA$variance, color = "SST Variance"))+
  ggtitle(paste("SST~PPD CCF", epu_name, season_name ))
p1

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
  zoo <- ecodata::zoo_abund %>% 
    filter(EPU == epu_name,
         Time >= 1998, 
         Var == "large")

pp <- read.csv(file = paste0(data.dir, paste0(epu_name,"_",season_name,"_",indicator,".csv"))) %>% 
  filter(Year <= 2018)

cc <- ccf(pp$variance, test$mean)
print(cc)


p1<- zoo %>%
  ggplot()+
  geom_line(aes(x =  Time, y = Value, color = "Zoo abundance"))+
  geom_line(aes(x= pp$Year, y = pp$variance, color = "PP Variance"))+
  ggtitle(paste("PPD~Zoo CCF", epu_name, season_name ))
p1

}

##### What does this mean??
ccf_test2("MAB", "winter", "pp") # -4, 0, 6 
ccf_test2("MAB", "spring", "pp") # NO
ccf_test2("MAB", "summer", "pp") # 9
ccf_test2("MAB", "fall", "pp") # 9 

ccf_test2("GB", "winter", "pp") # NO
ccf_test2("GB", "spring", "pp") # -1 
ccf_test2("GB", "summer", "pp") # 5
ccf_test2("GB", "fall", "pp")  #  NO

ccf_test2("GOM", "winter", "pp") # NO
ccf_test2("GOM", "spring", "pp") # NO
ccf_test2("GOM", "summer", "pp") # 8
ccf_test2("GOM", "fall", "pp") # -6, 


