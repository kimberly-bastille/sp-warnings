## Plot kims data
library(tidyverse)
mon_sst<-read.csv(here::here("temp/data-raw/MUR-NES_EPU_NOESTUARIES-SST-STATS-V2021-EDAB_SOE_V2021.CSV"))
head(mon_sst)

mon_sst<-mon_sst %>% 
  mutate(year = as.numeric(substr(PERIOD,3,6)),
          month = as.character(substr(PERIOD,7,8))) 
  
mon_sst %>% 
  filter(SUBAREA == "MAB", 
         ) %>% 
  ggplot(aes(x=year, y = VAR), group = month) +
  geom_point() +
  geom_path()+
  facet_wrap(~ month, ncol = 2) +
  ggtitle("MAB variance")
  
mon_sst %>% 
  filter(SUBAREA == "MAB") %>% 
  ggplot(aes(x=year, y = SKEW), group = month) +
  geom_point() +
  geom_path()+
  facet_wrap(~ month, ncol = 2) +
  ggtitle("MAB skewness")

p1<-mon_sst %>% filter(SUBAREA == "MAB",
                       month == "01"
) %>% 
  ggplot(aes(x=year, y = VAR)) +
  geom_point() +
  geom_path()+
  ggtitle("MAB Jan Var")

p2<-mon_sst %>% filter(SUBAREA == "MAB",
                       month == "01"
) %>% 
  ggplot(aes(x=year, y = SKEW)) +
  geom_point() +
  geom_path()+
  ggtitle("MAB Jan Skewness")
  
cowplot::plot_grid(p2,p1)
