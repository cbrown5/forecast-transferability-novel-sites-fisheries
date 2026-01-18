#cleaning RLS data - Inverts
#Leigha Aitken
#22/11/2024

#install.packages
library(dplyr)
library(readr)
library(lubridate)
  
#read in invert data
dat <- read_csv("Scripts/summary-data/invert.csv")
head(dat)
tail(dat)
summary(dat)

#remove columns
datsub1 <- select(dat, site_code, survey_date, survey_id, species_name, latitude, longitude, survey_date,total)
head(datsub1)

#extract year from survey_date (add as a new column)
datsub1$survey_date <- dmy(datsub1$survey_date)
datsub1$year <- year(datsub1$survey_date)

#add protection status
datsub1$protection_status <- ifelse(datsub1$site_code %in% c("MIR-S1", "MIR-S2", "MIR-S3", "MIR-S5", "MIR-S7", "MIR-S8"), "No take", "Fishing")

#remove rows 
#list of sites to keep
sites_to_keep <- c("MIR-S1", "MIR-S2", "MIR-S3", "MIR-S5", "MIR-S7","MIR-S8", 
                 "MIR-S9", "MIR-S10", "MIR-S11", "MIR-S12", "MIR-S13", "MIR-S14")

#remove rows that don't match the sites_to_keep
datsub1 <- datsub1 %>% filter(site_code %in% sites_to_keep)

#double check site codes kept in data
unique(datsub1$site_code)

#export data
write_csv(datsub1, "invert-clean_3.csv")

#filter for species of interest
datsub1 <- datsub1 %>% 
  filter(species_name == "Jasus edwardsii")

#check data normally distributed
hist(datsub1$total, main = "Histogram of Jasus edwardsii Total", xlab = "Total", ylab = "Frequency")

# Q-Q plot
qqnorm(datsub1$total, main = "Q-Q Plot of Jasus edwardsii Total")
qqline(datsub1$total, col = "red")

# Shapiro-Wilk test for normality
shapiro_test <- shapiro.test(datsub1$total)
print(shapiro_test)

# Interpretation of Shapiro-Wilk test
if (shapiro_test$p.value > 0.05) {
  print("The data is normally distributed (fail to reject H0).")
} else {
  print("The data is not normally distributed (reject H0).")
}


