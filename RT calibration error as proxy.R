install.packages("flextable")

library(dplyr)
library(janitor)
library(ggplot2)
library(lubridate)
library(tidyr)
library(stringr)
library(flextable)
library(scales)
library(readxl)

rate <- read_excel("C:/Users/krein/OneDrive - California Department of Water Resources/Documents/Projects/Field Lab SC/RT analysis proxy/RTD_drift_ratings_combined_CEMP_NRO_MWQP_NCRO_2019_to_2022.xlsx",sheet="Sheet2")

#clean column headers

rate <- clean_names(rate)

#subset to SPC ratings

unique(rate$parameter)
SPC <- filter(rate, parameter == "SPC", group != "CEMP")

SPC <- subset(rate, parameter == "SPC")

#convert field_date,  from character to date
#NCRO did not have data in this column
#SPC$field_date <- as.Date(SPC$field_date, "%m/%d/%Y")

#use field_exchange_datetime instead

SPC$field_exchange_datetime <- as.Date(SPC$field_exchange_datetime, "%m/%d/%Y")

#create year column

SPC$year <- year(SPC$field_exchange_datetime)

SPC$month <- month(SPC$field_exchange_datetime)


#all data - after running lines 44 - 47, it looks like one outlier exists with a 
#cal error % of ~10K % - sonde reading was 27.567, standard 
#was recorded as 2767.0 which appears to me as a transcription error but I should follow
#up with NCRO 

SPC %>% 
  ggplot() +
  geom_point(aes(x = field_exchange_datetime, y = calibration_error_pct))+
  ggtitle("SPC - cal error % (2018-2022)")

#all data grouped by groups (Fig. 1)

SPC %>% 
  ggplot() +
  geom_point(aes(x = field_exchange_datetime, y = calibration_error_pct))+
  facet_wrap(~group, scales = "free")+
  ggtitle("Fig. 1 SPC - cal error % (2018-2022)")

#all data with the one NCRO outlier removed

SPC_filt <- filter(SPC, calibration_error_pct <200)

SPC_filt %>% 
  ggplot() +
  geom_point(aes(x = field_exchange_datetime, y = calibration_error_pct))+
  facet_wrap(~group, scales = "free")+
  ggtitle("SPC - cal error % (2018-2022)")

#two more outliers stand out for CEMP, 
#removing them for this figure: one is >100; second is ~ -98

SPC_filt_2 <- filter(SPC, calibration_error_pct <100 & calibration_error_pct > -98)

#Fig. 2 

SPC_filt_2 %>% 
  ggplot() +
  geom_point(aes(x = field_exchange_datetime, y = calibration_error_pct))+
  facet_wrap(~group, scales = "free")+
  ggtitle("Fig. 2 SPC - cal error % (2018-2022) ")

#Now for Total error % - first all data

SPC %>% 
  ggplot() +
  geom_point(aes(x = field_exchange_datetime, y = total_error_pct))+
  ggtitle("SPC - total error % (2018-2022)")

#all total error % data by groups (Fig. 3)

SPC %>% 
  ggplot() +
  geom_point(aes(x = field_exchange_datetime, y = total_error_pct))+
  facet_wrap(~group, scales = "free")+
  ggtitle("Fig. 3 - SPC - total error % (2018-2022)")

#same NCRO outlier for total error % (as cal error %) removed (Fig. 4)
#there are quite a few more outliers here than the three removed for % cal error
#I refrained from removing them

SPC_tot_filt <- filter(SPC, total_error_pct <200)

SPC_tot_filt %>% 
  ggplot() +
  geom_point(aes(x = field_exchange_datetime, y = total_error_pct))+
  facet_wrap(~group, scales = "free")+
  ggtitle("Fig. 4 - SPC - total error % (2018-2022)")

