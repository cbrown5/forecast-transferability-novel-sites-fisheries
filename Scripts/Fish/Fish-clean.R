#cleaning RLS data - fish
#Leigha Aitken
#27/08/2024
#22/11/2024

#install.packages
library(dplyr)
library(readr)
library(lubridate)

#read in fish data
dat <- read_csv("fish.csv")
head(dat)
tail(dat)
summary(dat)

#remove columns
datsub <- select(dat, site_code, survey_date, survey_id, species_name, latitude, longitude, survey_date,total)
head(datsub)

#extract year from survey_date (add as a new column)
datsub$survey_date <- dmy(datsub$survey_date)
datsub$year <- year(datsub$survey_date)

#add protection status
datsub$protection_status <- ifelse(datsub$site_code %in% c("MIR-S1", "MIR-S2", "MIR-S3", "MIR-S5", "MIR-S7", "MIR-S8"), "No take", "Fishing")

#remove rows 
#list of sites to keep
sites_to_keep <- c("MIR-S1", "MIR-S2", "MIR-S3", "MIR-S5", "MIR-S7", "MIR-S8",
                 "MIR-S9", "MIR-S10", "MIR-S11", "MIR-S12", "MIR-S13", "MIR-S14")

#remove rows that don't match the sites_to_keep
datsub <- datsub %>% filter(site_code %in% sites_to_keep)

#double check site codes kept in data
unique(datsub$site_code)

#export data
write_csv(datsub, "fish-clean_3.csv")
