#Fish - model elaboration - PC1 and PC2, enviro + enviro_10
#Chris Brown and Leigha Aitken 
#21/01/2024

# To install INLA package see here (non-standard installation)
# https://www.r-inla.org/download-install

#clear environment
rm(list = ls())
#install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
library(INLA)
library(tidyverse)

datI2 <- read.csv("Scripts/Summary-data/fish-clean_3.csv") 

#
#select spp and initial plot to check it works from (species_protection_totals)
#
fit_date <- Sys.Date()

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

#read in environmental data with outliers removed - before below looping 
enviro_10 <- read.csv("Scripts/summary-data/enviro_10_outliers.csv")
#enviro <- read.csv("Scripts/summary-data/enviro_outliers.csv")

#
# INLA model 
#

#Set the last year for fitting. Will forecast from 2016 onwards 
origin_year <- 2015

site_names <- unique(datI2$site)

training_combinations <- combn(site_names, 5)

#create a list to store all training-test splits
splits <- list()

#generate all splits
for(i in 1:ncol(training_combinations)) {
  training_sites <- training_combinations[,i]
  test_sites <- setdiff(site_names, training_sites)
  
  splits[[i]] <- list(
    training = training_sites,
    test = test_sites
  )
}

#function to count site types
count_site_types <- function(sites) {
  nt_count <- sum(startsWith(sites, "NT"))
  f_count <- sum(startsWith(sites, "F"))
  return(c(nt_count, f_count))
}

#create empty dataframe to store results
split_counts <- data.frame(
  split_id = integer(),
  nt_count = integer(),
  f_count = integer(),
  stringsAsFactors = FALSE
)

#loop through splits and count
for(i in 1:length(splits)) {
  training_sites <- splits[[i]]$training
  counts <- count_site_types(training_sites)
  
  #add row to dataframe
  split_counts <- rbind(split_counts, 
                        data.frame(split_id = i,
                                   nt_count = counts[1], 
                                   f_count = counts[2]))
}

#subset splits to pick N splits for each of 0:5 nt_count
set.seed(123) # for reproducibility
sampled_splits <- split_counts %>%
  group_by(f_count) %>%
  slice_sample(n = 50) %>%
  ungroup()
sampled_splits 
print( sampled_splits, n = 212)

#dataframe saved from inverts 

#get the split IDs we'll use
selected_split_ids <- sampled_splits$split_id
#view what training and test sites are in each split
splits[selected_split_ids]

#subset the splits list to only include selected combinations
selected_splits <- splits[selected_split_ids]
length(selected_splits)
length(selected_split_ids)
#create a list to store results for each split
all_results <- list()
all_predictions <- list()

system.time(
  #loop through each split
  for(i in 1:length(selected_splits)) {
    #for(i in 1:2) {
    #i <- 1 #never uncomment this line!!
    #get training and test sites for this split
    training_sites <- selected_splits[[i]]$training
    test_site <- selected_splits[[i]]$test
    
    #create fitting dataframe
    datmod_fit <- datI2 %>%
      filter(site %in% training_sites & species_name == species_to_plot & year <= origin_year) %>%
      mutate(y = total)
    
    #data for forecasting (test site from origin_year onwards)
    datmod_predict <- datI2 %>%
      filter(site %in% test_site & species_name == species_to_plot & year > origin_year) %>%
      mutate(y = NA)
    
    #predict to test site before origin_year
    datmod_predict2 <- datI2 %>%
      filter(site %in% test_site & species_name == species_to_plot & year <= origin_year) %>%
      mutate(y = NA)
    
    #combine fitting and prediction data
    datmod_fit$fit_predict <- "fit up to origin"
    datmod_predict$fit_predict <- "forecast"
    datmod_predict2$fit_predict <- "predict up to origin"
    datmod_combined <- bind_rows(datmod_fit, datmod_predict, datmod_predict2) %>%
      mutate(site_fit = if_else(site %in% training_sites, "fit", "predict"))
    
    #merge enviro_10 (less than 10m) data into datmod_combined by year
    datmod_combined <- datmod_combined %>%
      left_join(enviro_10, by = "year")
    
    #merge enviro data into datmod_combined by year
    #datmod_combined <- datmod_combined %>%
    #left_join(enviro, by = "year")
    
    #check if all training sites have the same prefix
    nt_count <- sum(startsWith(training_sites, "NT"))
    f_count <- sum(startsWith(training_sites, "F"))
    
    #INLA model formula
    if (nt_count == 5 || f_count == 5) {
      formula <- y ~ f(year, model = "ar1") + f(site_code, model = "iid") + PC1_1yrlag + PC2_1yrlag
      # PC1 + PC2
      # or PC1_1yrlag + PC2_1yrlag
    } else {
      formula <- y ~ f(year, model = "ar1") + protection_status +  f(site_code, model = "iid") + PC1_1yrlag + PC2_1yrlag
      #+ PC1 + PC2
      # or PC1_1yrlag + PC2_1yrlag
    }
    
    #fit INLA model
    model <- inla(formula,
                  family = "poisson",
                  data = datmod_combined,
                  control.predictor = list(compute = TRUE, link = 1))
    #save predictions
    datmod_combined$mean <- model$summary.fitted.values$mean
    datmod_combined$lower <- model$summary.fitted.values$`0.025quant`
    datmod_combined$upper <- model$summary.fitted.values$`0.975quant`
    
    #store results
    all_results[[i]] <- list(
      split_id = i,
      training_sites = training_sites,
      test_site = test_site,
      model = model
    )
    
    #save data with predictions
    xout <- datmod_combined %>%
      select(survey_id, site, year, total, protection_status, fit_predict, site_fit, mean, lower, upper) %>%
      mutate(split_id = i)
    
    all_predictions[[i]] <- xout
    
    
  }
)

filename <- paste0("Scripts/Fish/Workspace/50-splits-per-fcount_LAGGED_",fit_date,"/spp-",species_to_plot,"-inla-forecasts_lagged.Rdata")

#check if filename exists, if it doesn't created it
if (!file.exists(filename)) {
  dir.create(dirname(filename), recursive = TRUE)
}

#Save results
save(all_predictions, all_results,selected_splits, file = filename)




