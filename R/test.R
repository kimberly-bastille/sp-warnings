#Other info on EWS - http://www.early-warning-signals.org/?page_id=480


################################################
### Spatial Data
library(spatialwarnings)

# Compute the generic indicators values. The options here are set according
# to the methods in Eby et al. (2017)
serengeti.ic <- generic_sews(serengeti,
                             subsize = 5,
                             moranI_coarse_grain = TRUE)
# Assess the indicator significance
serengeti.test <- indictest(serengeti.ic)
# Display textual summary
summary(serengeti)
plot(serengeti.test, along = serengeti.rain)



## Timeseries data
library(earlywarnings)
library(ecodata)

cp<-as.vector(ecodata::cold_pool$Value)
ews<-generic_ews(cp)
summary(ews)
stat(ews)


