#Fish - model comparison - enviro vs no-enviro vs lagged, + 10m 
#Leigha Aitken  
#14/03/25

# To install INLA package see here (non-standard installation)
# https://www.r-inla.org/download-install

#clear environment
rm(list = ls())
install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
library(INLA)
library(tidyverse)

datI2 <- read.csv("Scripts/summary-data/fish-clean_3.csv")

# **uncomment as needed** 
enviro_10 <- read.csv("Scripts/summary-data/enviro_10.csv")
#enviro <- read.csv("Scripts/summary-data/enviro.csv") 

#
# Select spp and initial plot to check it works from (species_protection_totals)
#
#species_to_plot <- "Cheilodactylus spectabilis" #banded morwong" --> doesn't exist
species_to_plot <- "Notolabrus tetricus" #blue throat wrasse *
#species_to_plot <- "Acanthaluteres vittiger" #toothbrush leatherjacket
#species_to_plot <- "Caesioperca rasor" #baber perch *
#species_to_plot <- "Notolabrus fucicola" #purple wrasse 
#species_to_plot <- "Latridopsis forsteri" #bastard trumpeter *
#species_to_plot <- "Trachurus declivis" #jack mackerel *
#species_to_plot <- "Dinolestes lewini" #longfin pike *
#species_to_plot <- "Pictilabrus laticlavius" #senator wrasse
#species_to_plot <- "Arripis trutta" #Australian salmon * 
#species_to_plot <- "Pseudocaranx dentex" #white trevally *
#species_to_plot <- "Meuschenia australis" #brown-striped leatherjacket
#species_to_plot <- "Arripis spp." #salmon spp. *
#species_to_plot <- "Olisthops cyanomelas" #hering cale *
#species_to_plot <- "Atypichthys strigatus" #mado sweep *
#species_to_plot <- "Engraulis australis" #Australian anchovy *

##from species_protection_totals (* spp. above also relevant here) - not including outliers 
#species_to_plot <- "Aplodactylus arctidens" #marblefish
#species_to_plot <- "Aracana aurita" #shaws cowfish
#species_to_plot <- "Cephaloscyllium laticeps" #draughtboard shark
#species_to_plot <- "Diodon nicthemerus" #porcupinefish
#species_to_plot <- "Nemadactylus macropterus" #jackass morwong
#species_to_plot <- "Neosebastes scorpaenoides" #gurnard perch
#species_to_plot <- "Pentaceropsis recurvirostris" #long-snouted boarfish
#species_to_plot <- "Psuedocaranx georgianus" #silver trevally
#species_to_plot <- "Pseudophycis bachus" #red cod
#species_to_plot <- "Pseudophycis barbata" #bearded cod
#species_to_plot <- "Sardinops neopilchardus" #Australian sardine
#species_to_plot <- "Scorpis aequipinnis" #sea sweep
#species_to_plot <- "Seriolella brama" #blue warehou
#species_to_plot <- "Trachurus novaezelandiae" #yellowtail scad
#species_to_plot <- "Upeneichthys vlamingii" #blue-spotted goatfish

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
  
  #remove outliers from enviro_10
  enviro_10 <- enviro_10 %>%
    #filter(!Nitrate_umolL %in% nitrate_outliers_10) %>% 
    #filter(!Silicate_umolL %in% silicate_outliers_10) %>% 
    #filter(!Salinity %in% salinity_outliers_10) %>% 
    #filter(!mean_temp %in% mean_temp_outliers_10) %>% 
    filter(!PC1 %in% PC1_outliers_10) %>% 
    filter(!PC2 %in% PC2_outliers_10) %>% 
    filter(!(PC1_1yrlag %in% PC1_1yrlag_outliers_10) | is.na(PC1_1yrlag)) %>% 
    filter(!(PC2_1yrlag %in% PC2_1yrlag_outliers_10) | is.na(PC2_1yrlag))
}
#write_csv(enviro_10, "Scripts/summary-data/enviro_10_outliers_2.csv")

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
  
  #remove outliers from enviro
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

formula_enviro <- y ~ f(year, model = "ar1") + protection_status + PC1 + PC2 + f(site_code, model = "iid")
formula_enviro_lagged <- y ~ f(year, model = "ar1") + protection_status + PC1_1yrlag + PC2_1yrlag + f(site_code, model = "iid")
formula_no_enviro <- y ~ f(year, model = "ar1") + protection_status + f(site_code, model = "iid")

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
                             #family = "poisson",
                             #data = datmod_fit,
                             #control.predictor = list(compute = TRUE, link = 1),
                             #control.compute = list(waic = TRUE))



# Compute and print WAIC
model_enviro$waic$waic
model_enviro_lagged$waic$waic
model_no_enviro$waic$waic

#model_enviro_noPS$waic$waic
#model_enviro_lagged_noPS$waic$waic
#model_no_enviro_noPS$waic$waic

#compare models to baseline of model_no_enviro  (lowest = best)
model_enviro$waic$waic - model_no_enviro$waic$waic
model_enviro_lagged$waic$waic - model_no_enviro$waic$waic
model_enviro$waic$waic - model_enviro_lagged$waic$waic

summary(model_enviro)
summary(model_no_enviro)
summary(model_enviro_lagged)

#summary(model_enviro_noPS)
#summary(model_no_enviro_noPS)
#summary(model_enviro_lagged_noPS)
