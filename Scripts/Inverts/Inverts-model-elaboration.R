#Inverts - model elaboration - PC1 and PC2 - enviro + enviro_10
#Chris Brown and Leigha Aitken 
#21/01/2024

# To install INLA package see here (non-standard installation)
# https://www.r-inla.org/download-install

#clear environment
rm(list = ls())
#install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
library(INLA)
library(tidyverse)

datI2 <- read.csv("Scripts/summary-data/invert-clean_3.csv") 

#
#select spp and initial plot to check it works from (species_protection_totals)
#
fit_date <- Sys.Date()

species_to_plot <- "Jasus edwardsii" #rock lobster"
#species_to_plot <- "Heliocidaris erythrogramma" #short spined sea urchin 
# species_to_plot <- "Haliotis rubra" #abalone
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

readr::write_csv(datmod, file = "jasus-edwardsii-maria-island.csv")

do_plot <- TRUE
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

#set the last year for fitting. Will forecast from 2016 onwards 
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
set.seed(123) 
sampled_splits <- split_counts %>%
  group_by(f_count) %>%
  slice_sample(n = 50) %>%
  ungroup()
sampled_splits 
print( sampled_splits, n = 212)

#save dataframe 
write_csv(sampled_splits, "Scripts/summary-data/inverts_sampled-splits_50.csv")

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
  # i <- 1 #never uncomment this line!!
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
    #+ PC1 + PC2
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

filename <- paste0("Scripts/Inverts/Workspace/50-splits-per-fcount_LAGGED_",fit_date,"/spp-",species_to_plot,"-inla-forecasts.Rdata")

#check if filename exists, if it doesn't created it
if (!file.exists(filename)) {
  dir.create(dirname(filename), recursive = TRUE)
}

#save results
save(all_predictions, all_results,selected_splits, file = filename)



