#PCA analysis of environmental variables
#Leigha Aitken 

install.packages(tidyverse)
install.packages(ggplot2)
install.packages(ggfortify)

library(tidyverse)
library(ggplot2)
library(ggfortify)

# Load datasets
data_10m <- read.csv("Data/enviro_10.csv")
data_all <- read.csv("Data/enviro.csv")

# Select variables for PCA
vars <- c("Nitrate_umolL", "Silicate_umolL", "Salinity", "mean_temp", "max_temp", "min_temp")

# Scale data
data_10m_scaled <- scale(data_10m[vars])
data_all_scaled <- scale(data_all[vars])

# Perform PCA
pca_10m <- prcomp(data_10m_scaled, center = TRUE, scale. = TRUE)
pca_all <- prcomp(data_all_scaled, center = TRUE, scale. = TRUE)

# Biplot for 10m data
biplot_10m <- autoplot(pca_10m, data = data_10m, loadings = TRUE, loadings.label = TRUE, loadings.label.size = 3) +
  ggtitle("Biplot of 10m Data")

# Biplot for all data
biplot_all <- autoplot(pca_all, data = data_all, loadings = TRUE, loadings.label = TRUE, loadings.label.size = 3) +
  ggtitle("Biplot of All Data")

# Show plot for 10m data
biplot_10m

# Show plot for all data
biplot_all

# Save plots
ggsave("Plots/pca-results/biplot_10m.png", biplot_10m)
ggsave("Plots/pca-results/biplot_all.png", biplot_all)
