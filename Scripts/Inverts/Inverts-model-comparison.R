#Inverts - model comparison - enviro vs no-enviro vs lagged, + 10m 
#Chris Brown
#25/02/25

# To install INLA package see here (non-standard installation)
# https://www.r-inla.org/download-install

#clear environment
rm(list = ls())
install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
library(INLA)
library(tidyverse)

datI2 <- read.csv("Scripts/summary-data/invert-clean_3.csv") 

# **uncomment as needed** 
enviro_10 <- read.csv("Scripts/summary-data/enviro_10.csv")
#enviro <- read.csv("Scripts/summary-data/enviro.csv") 

#
# Select spp and initial plot to check it works from (species_protection_totals)
#
species_to_plot <- "Jasus edwardsii" #rock lobster" 
#species_to_plot <- "Heliocidaris erythrogramma" #short spined sea urchin 
#species_to_plot <- "Haliotis rubra" #abalone
#species_to_plot <- "Lunella undulata" #periwikle 
#species_to_plot <- "Centrostephanus rodgersii" #long spined sea urchin 
#species_to_plot <- "Plagusia chabrus" #red bait crab 
#species_to_plot <- "Equichlamys bifrons" #queen scallop
#species_to_plot <- "Mimachlamys asperrima" #scallop
#species_to_plot <- "Nectocarcinus tuberculosus" #velvet crab
#species_to_plot <- "Scutus antipodes" #elephant snail
#species_to_plot <- "Strigopagurus strigimanus" #hermit crab

#print(species_to_plot)

#all sites combined 
datmod <- datI2 %>%
  filter(species_name == species_to_plot)

do_plot <- FALSE
#only make the plot if do_plot is TRUE
if(do_plot) g1 <- datmod %>%
ggplot() + 
  aes(x = year, y = total, color = protection_status) +
  geom_point() +
  stat_smooth() +
  scale_y_sqrt() + 
  labs(title = species_to_plot, x = "Year", y = "Abundance") 
g1

#
#determine outliers 
#
#define function to detect outliers using the IQR method
detect_outliers <- function(data, column_name) {
  Q1 <- quantile(data[[column_name]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data[[column_name]], 0.75, na.rm = TRUE)
  IQR_value <- Q3 - Q1
  
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  
  outliers <- data[data[[column_name]] < lower_bound | data[[column_name]] > upper_bound, column_name]
  
  return(outliers)
}

#set a condition to run specific blocks of code
run_block_1 <- TRUE # Set to TRUE to run enviro_10
run_block_2 <- FALSE # Set to TRUE to run enviro

if (run_block_1) {
  
#apply the function to selected columns enviro_10
#nitrate_outliers_10 <- detect_outliers(enviro_10, "Nitrate_umolL")
#silicate_outliers_10 <- detect_outliers(enviro_10, "Silicate_umolL")
#salinity_outliers_10 <- detect_outliers(enviro_10, "Salinity")
#mean_temp_outliers_10 <- detect_outliers(enviro_10, "mean_temp")
PC1_outliers_10 <- detect_outliers(enviro_10, "PC1")
PC2_outliers_10 <- detect_outliers(enviro_10, "PC2")
PC1_1yrlag_outliers_10 <- detect_outliers(enviro_10, "PC1_1yrlag")
PC2_1yrlag_outliers_10 <- detect_outliers(enviro_10, "PC2_1yrlag")
PC1_2yrlag_outliers_10 <- detect_outliers(enviro_10, "PC1_2yrlag")
PC2_2yrlag_outliers_10 <- detect_outliers(enviro_10, "PC2_2yrlag")

# Remove outliers from enviro_10
enviro_10 <- enviro_10 %>%
  #filter(!Nitrate_umolL %in% nitrate_outliers_10) %>% 
  #filter(!Silicate_umolL %in% silicate_outliers_10) %>% 
  #filter(!Salinity %in% salinity_outliers_10) %>% 
  #filter(!mean_temp %in% mean_temp_outliers_10) %>% 
  filter(!PC1 %in% PC1_outliers_10) %>% 
  filter(!PC2 %in% PC2_outliers_10) %>% 
  filter(!(PC1_1yrlag %in% PC1_1yrlag_outliers_10) | is.na(PC1_1yrlag)) %>% 
  filter(!(PC2_1yrlag %in% PC2_1yrlag_outliers_10) | is.na(PC2_1yrlag)) %>% 
  filter(!(PC1_1yrlag %in% PC1_1yrlag_outliers_10) | is.na(PC1_1yrlag)) %>% 
  filter(!(PC2_1yrlag %in% PC2_1yrlag_outliers_10) | is.na(PC2_1yrlag))
}
#write_csv(enviro_10, "Scripts/summary-data/enviro_10_outliers.csv")

if (run_block_2) {
#Apply the function to selected columns enviro
#nitrate_outliers <- detect_outliers(enviro, "Nitrate_umolL")
#silicate_outliers <- detect_outliers(enviro, "Silicate_umolL")
#salinity_outliers <- detect_outliers(enviro, "Salinity")
#mean_temp_outliers <- detect_outliers(enviro, "mean_temp")
PC1_outliers <- detect_outliers(enviro, "PC1")
PC2_outliers <- detect_outliers(enviro, "PC2")
PC1_1yrlag_outliers <- detect_outliers(enviro, "PC1_1yrlag")
PC2_1yrlag_outliers <- detect_outliers(enviro, "PC2_1yrlag")

# Remove outliers from enviro
enviro <- enviro %>%
  #filter(!Nitrate_umolL %in% nitrate_outliers) %>%
  #filter(!Silicate_umolL %in% silicate_outliers) %>%
  #filter(!Salinity %in% salinity_outliers) %>%
  #filter(!mean_temp %in% mean_temp_outliers) %>%
  filter(!PC1 %in% PC1_outliers) %>%
  filter(!PC2 %in% PC2_outliers) %>%
  filter(!(PC1_1yrlag %in% PC1_1yrlag_outliers) | is.na(PC1_1yrlag)) %>%
  filter(!(PC2_1yrlag %in% PC2_1yrlag_outliers) | is.na(PC2_1yrlag))
}  
#write_csv(enviro, "Scripts/summary-data/enviro_outliers.csv")

#
#INLA model 
#

#create fitting dataframe
  # **uncomment as needed**
 #datmod_fit <- datI2 %>%
   #mutate(y = total) %>% 
   #left_join(enviro_10, by = "year") 
  datmod_fit <- datmod %>%
    mutate(y = total) %>% 
    left_join(enviro_10, by = "year") 
  
plot(datmod_fit$PC2, datmod_fit$PC1)
plot(datmod_fit$PC2_1yrlag, datmod_fit$PC1_1yrlag)
#plot(datmod_fit$PC2_2yrlag, datmod_fit$PC1_2yrlag)

formula_enviro <- y ~ f(year, model = "ar1") + protection_status + PC1 + PC2 + f(site_code, model = "iid")
formula_enviro_lagged <- y ~ f(year, model = "ar1") + protection_status + PC1_1yrlag + PC2_1yrlag + f(site_code, model = "iid")
formula_no_enviro <- y ~ f(year, model = "ar1") + protection_status + f(site_code, model = "iid")
#formula_enviro_lagged_2 <- y ~ f(year, model = "ar1") + protection_status + PC1_2yrlag + PC2_2yrlag + f(site_code, model = "iid")

#without protection status
#formula_enviro_noPS <- y ~ f(year, model = "ar1") + PC1 + PC2 + f(site_code, model = "iid")
#formula_enviro_lagged_noPS <- y ~ f(year, model = "ar1") + PC1_1yrlag + PC2_1yrlag + f(site_code, model = "iid")
#formula_no_enviro_noPS <- y ~ f(year, model = "ar1") + f(site_code, model = "iid")

#fit INLA model
model_enviro <- inla(formula_enviro,
                family = "poisson",
                data = datmod_fit,
                control.predictor = list(compute = TRUE, link = 1),
                control.compute = list(waic = TRUE))
  
model_enviro_lagged <- inla(formula_enviro_lagged,
                family = "poisson",
                data = datmod_fit,
                control.predictor = list(compute = TRUE, link = 1),
                control.compute = list(waic = TRUE))

model_no_enviro <- inla(formula_no_enviro,
                family = "poisson",
                data = datmod_fit,
                control.predictor = list(compute = TRUE, link = 1),
                control.compute = list(waic = TRUE))

#model_enviro_lagged_2 <- inla(formula_enviro_lagged,
                            #family = "poisson",
                           # data = datmod_fit,
                            #control.predictor = list(compute = TRUE, link = 1),
                            #control.compute = list(waic = TRUE))

#without protection status
#model_enviro_noPS <- inla(formula_enviro_noPS,
                #family = "poisson",
                #data = datmod_fit,
                #control.predictor = list(compute = TRUE, link = 1),
                #control.compute = list(waic = TRUE))

#model_enviro_lagged_noPS <- inla(formula_enviro_lagged_noPS,
                #family = "poisson",
                #data = datmod_fit,
                #control.predictor = list(compute = TRUE, link = 1),
                #control.compute = list(waic = TRUE))

#model_no_enviro_noPS <- inla(formula_no_enviro_noPS,
               # family = "poisson",
               # data = datmod_fit,
               # control.predictor = list(compute = TRUE, link = 1),
               # control.compute = list(waic = TRUE))

#compute and print WAIC
model_enviro$waic$waic
model_enviro_lagged$waic$waic
model_no_enviro$waic$waic
#model_enviro_lagged_2$waic$waic

#model_enviro_noPS$waic$waic
#model_enviro_lagged_noPS$waic$waic
#model_no_enviro_noPS$waic$waic

#compare models to baseline of model_no_enviro (lowest = best)
model_enviro$waic$waic - model_no_enviro$waic$waic
model_enviro_lagged$waic$waic - model_no_enviro$waic$waic
model_enviro$waic$waic - model_enviro_lagged$waic$waic
#model_enviro$waic$waic - model_enviro_lagged_2$waic$waic

summary(model_enviro)
summary(model_no_enviro)
summary(model_enviro_lagged)
#summary(model_enviro_lagged_2)

#summary(model_enviro_noPS)
#summary(model_no_enviro_noPS)
#summary(model_enviro_lagged_noPS)
