#Inverts - 50-splits - plots
#Leigha Aitken 
#21/04/2025

#clear environment
rm(list = ls())

#load package
library(tidyverse)

#load data using appropriate species 
load(file = "Scripts/Inverts/Workspace/50-splits-per-fcount_LAGGED_2025-03-28/spp-Jasus edwardsii-inla-forecasts_lagged.RData")

#
# Calculate the ASE
#

datorig <- read.csv("Scripts/summary-data/invert-clean_3.csv") %>% 
  filter(species_name == "Jasus edwardsii")

#calculate MASE test statistic for g1 plot
dat_ASE <- datorig %>% 
  group_by(site, year) %>% 
  arrange(year) %>% 
  arrange(site) %>% 
  summarize(total = mean(total))
ggplot(dat_ASE) + 
  aes(x = year, y = total, color = site) + 
  geom_line()

#order by years and calculate ASE for each site as differences
dat_ASE2 <- dat_ASE %>% 
  ungroup() %>% 
  group_by(site) %>% 
  reframe(ydiff = diff(total)) %>% 
  group_by(site) %>% 
  summarize(ASE_scale = mean(abs(ydiff)))
#ASE scaling factor (denominator) for each site
#overall ASE scaling factor: 
ASE_overall <- mean(dat_ASE2$ASE_scale)

#mega dataframe of all model predictions 
preds_plots <- bind_rows(all_predictions) 

#merge sampled_splits data into preds_plots by split_id
sampled_splits <- read.csv("Scripts/summary-data/inverts_sampled-splits_50.csv")
preds_plots <- preds_plots %>% 
  left_join(sampled_splits, by = "split_id")
names(preds_plots)

#calculate MASE test statistic for g1 plot
origin_year <- 2015
preds_plots <- preds_plots %>% 
  mutate(
    horizon = year - origin_year, 
    AE = abs(total - mean),
    model_scaled_ASE = AE / ASE_overall)

#plot the predictions (50-splits)
g1 <- preds_plots %>% 
  filter(fit_predict == "forecast") %>% 
  ggplot(aes(x = year, y = mean, color = as.factor(f_count))) +
  geom_line(linewidth = 0.75) +
  facet_grid(site ~ split_id) +
  theme_minimal()+ 
  theme(axis.text.x = element_text(size = 6, angle = 45), 
        axis.text.y = element_text (size = 6)) +
  scale_x_continuous(labels = function(x) substr(x, 3, 4)) +  
  scale_color_manual(values = c("0" = "red", "1" = "#FF1493", "2" = "#FF69B4", "3" = "#87CEEB", "4" = "#4682B4", "5" = "blue"),
                     labels = c("0" = "0 (NTMR)", "1" = "1 (Mixed - NTMR)", "2" = "2 (Mixed - NTMR)", 
                                "3" = "3 (Mixed - Fished)", "4" = "4 (Mixed - Fished)", "5" = "5 (Fished)")) +
  labs(x = "Year", y = "Prediction", color = "No. of fished sites")
g1
#ggsave(g1, filename = paste0("Plots/Inverts/Lobster/Lobster_mega-plot_enviro-10_50.png"))

#plot the predictions on 1 axis 
g2 <- preds_plots %>%  
  filter(fit_predict == "forecast") %>% 
  ggplot(aes(x = year, y = mean, color = as.factor(f_count), group = interaction(site, split_id))) +
  geom_line(linewidth = 0.3) +
  theme_classic() +
  theme(axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.line = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(color = "black")) +
  scale_color_manual(values = c("0" = "red", "1" = "#FF1493", "2" = "#FF69B4", "3" = "#87CEEB", "4" = "#4682B4", "5" = "blue"),
                     labels = c("0" = "0 (NTMR)", "1" = "1 (Mixed - NTMR)", "2" = "2 (Mixed - NTMR)", 
                                "3" = "3 (Mixed - Fished)", "4" = "4 (Mixed - Fished)", "5" = "5 (Fished)")) +
  scale_x_continuous(breaks = seq(2016, 2023, by = 1), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, max(preds_plots$mean, na.rm = TRUE), by = 0.5), expand = c(0, 0)) +
  labs(x = "Year", y = "Prediction", color = "No. of fished sites")
g2
#ggsave(g2, filename = paste0("Plots/Inverts/Lobster/lobster_mega-plot_enviro-10_2_50.png"))

#mega plot of ASE values
g3 <- preds_plots %>% 
  filter(fit_predict == "forecast") %>% 
  ggplot(aes(x = year, y = model_scaled_ASE, color = as.factor(f_count))) +
  geom_line(linewidth = 0.75) +
  facet_grid(site ~ split_id) +
  theme(axis.text.x = element_text(size = 6, angle = 45), 
        axis.text.y = element_text (size = 6)) +
  scale_x_continuous(labels = function(x) substr(x, 3, 4)) + 
  labs(x = "Year", y = "Scaled error", color = "No. of fished sites")
g3
#ggsave(g3, filename = paste0("Plots/Inverts/Lobster/lobster_mega-plot_ASE_enviro-10_50.png"))

#
# Summary ASE as median ASE
#

mase_dat <- preds_plots %>% 
  group_by(site, split_id, year) %>% 
  summarize(MASE = mean(model_scaled_ASE)) %>%
  ungroup() %>%
  group_by(site, split_id) %>%
  summarize(MASE = median(MASE))

#group split_id for the legend
mase_dat <- mase_dat %>% 
  mutate(split_group = case_when(
    split_id %in% 1:6 ~ "1-6",
    split_id %in% 7:56 ~ "7-56", 
    split_id %in% 57:106 ~ "57-106",
    split_id %in% 107:156 ~ "107-156",
    split_id %in% 157:206 ~ "157-206",
    split_id %in% 207:212 ~ "207-212"
  ),
  `F sites` = case_when(
    split_id %in% 1:6 ~ 0,
    split_id %in% 7:56 ~ 1,
    split_id %in% 57:106 ~ 2,
    split_id %in% 107:156 ~ 3,
    split_id %in% 157:206 ~ 4,
    split_id %in% 207:212 ~ 5
  ),
  `NT sites` = 5 - `F sites`
  ) %>%
  mutate(`Status of test site` = if_else(grepl("F", site), "Fished", "No-take"))

#calculate summary stats
site_means <- mase_dat %>%
  group_by(site) %>%
  summarise(mean_MASE = mean(MASE))

g4 <- ggplot(mase_dat) + 
  aes(x = site, y = MASE, color = split_group) + 
  geom_hline(yintercept = 1, linetype = 2) + 
  geom_point() + 
  theme_classic() +
  theme(axis.text.x = element_text(color = "black", size = 12),
        axis.text.y = element_text(color = "black", size = 12),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_color_manual(values = c("1-6" = "red", "7-56" = "#FF1493", "57-106" = "#FF69B4", 
                                "107-156" = "#87CEEB", "157-206" = "#4682B4", "207-212" = "blue"),
                     labels = c("1-6" = "0 (NTMR)", "7-56" = "1 (Mixed - NTMR)", "57-106" = "2 (Mixed - NTMR)", 
                                "107-156" = "3 (Mixed - Fished)", "157-206" = "4 (Mixed - Fished)", "207-212" = "5 (Fished)")) +
  scale_y_continuous(breaks = seq(0, max(mase_dat$MASE), by = 0.5), expand = expansion(mult = c(0.05, 0.05))) + 
  labs(x = "Site", y = "MASE", color = "No. of fished sites") +
  stat_summary(fun = mean, color = "black", shape = 18, size = 0.75) 
g4
#ggsave(g4, filename = paste0("Plots/Inverts/Lobster/Figure_4.png"), dpi=1000)
#ggsave(g4, filename = paste0("Plots/Inverts/Lobster/Figure_4.pdf"))

#calculate cluster robust SEs
cluster_stats <- mase_dat %>% 
  group_by(`F sites`, `Status of test site`) %>% 
  summarise(
    mean_MASE = mean(MASE),
    n_clusters = n_distinct(site),
    cluster_se = sqrt(var(MASE) / n_clusters),
    ci_lower = mean_MASE - 1.96 * cluster_se,
    ci_upper = mean_MASE + 1.96 * cluster_se,
    .groups = 'drop'
  ) 

#create plot with both individual points and cluster SEs
dodge_width <- 0.25  

robust_se_plot <- ggplot() +
  geom_point(data = mase_dat,
             aes(x = `F sites`, y = MASE, color = `Status of test site`),
             alpha = 0.2, 
             position = position_dodge(width = dodge_width),
             size = 3) + 
  geom_errorbar(data = cluster_stats,
                aes(x = `F sites`, y = mean_MASE, 
                    ymin = ci_lower, ymax = ci_upper,
                    group = `Status of test site`),
                width = 0.4, 
                position = position_dodge(width = dodge_width),
                color = "black",
                linewidth = 1.25) + 
  geom_point(data = cluster_stats,
             aes(x = `F sites`, y = mean_MASE, fill = `Status of test site`),
             size = 6, 3,
             position = position_dodge(width = dodge_width),
             shape = 21, color = "black" ) +
  geom_hline(yintercept = 1, linetype = 2) +
  theme_classic() +
  theme(
    axis.text = element_text(color = "black", size = 28), 
    axis.line = element_line(color = "black"), 
    axis.title = element_text(size = 18), 
    legend.text = element_text(size = 18), 
    legend.title = element_text(size = 18) 
  ) +
  scale_color_manual(values = c("Fished" = "#00BFC4", "No-take" = "#F8766D")) +
  scale_fill_manual(values = c("Fished" = "#0A47FFFF", "No-take" = "#FF3333")) +
  labs(x = "Number of fished sites in training set",
       y = "Mean MASE")

print(robust_se_plot)
#ggsave(robust_se_plot, filename = paste0("Plots/Inverts/Lobster/Figure_5.png"), dpi=1000)

# 
#Additional Plots 
# 

#Plot year vs. prediction for site "X" and split_ID "X"
g5 <- preds_plots %>% 
  filter(fit_predict == "forecast", site == "F10", split_id == 1) %>% #update site and split ID as needed
  ggplot(aes(x = year, y = mean, color = as.factor(f_count))) +
  geom_line(linewidth = 0.75) +
  theme_classic() +
  theme(axis.text = element_text(color = "black", size =12),
        axis.title = element_text(size = 12),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        legend.title = element_text(size = 12),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank()) +  
  scale_x_continuous(breaks = seq(min(preds_plots$year), max(preds_plots$year), by = 1)) +  
  scale_y_continuous(breaks = seq(0, max(preds_plots$mean, na.rm = TRUE), by = 0.1)) + 
  scale_color_manual(values = c("0" = "red", "1" ="#FF1493", "2" = "#FF69B4", "3" = "#87CEEB", "4" = "#4682B4", "5" = "blue")) +  
  labs(x = "Year", y = "Prediction") +
  theme(legend.position = "none")
g5
#ggsave(g5, filename = paste0("Plots/Inverts/Lobster/Lobster_mega-plot_enviro-10_site_splitID.png"))

# Plot year vs. prediction for 2 sites and split_IDs
g6 <- preds_plots %>%
  filter(fit_predict == "forecast", (site == "F10" & split_id == 1) | (site == "NT2" & split_id == 13)) %>% 
  ggplot(aes(x = year, y = mean, color = interaction(site, split_id))) +
  geom_line(linewidth = 0.75) +
  theme_classic() +
  theme(axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank()) +  
  scale_x_continuous(breaks = seq(2016, max(preds_plots$year), by = 1)) +  
  scale_y_continuous(breaks = seq(0, max(preds_plots$mean, na.rm = TRUE), by = 0.2)) +
  scale_color_manual(values = c("F10.1" = "red", "NT2.13" ="#FF1493"), 
                     labels = c("F10.1" = "F10.1 (NTMR)", "NT2.13" = "NT2.13 (Mixed - NTMR)")) + 
  labs(x = "Year", y = "Prediction", color = "Site.Split ID") 
g6
#ggsave(g6, filename = paste0("Plots/Inverts/Lobster/Lobster_mega-plot_enviro-10_site_splitID_2.png"))

# Plot year vs. prediction for multiple sites and split_IDs
g7 <- preds_plots %>%
  filter(
    fit_predict == "forecast",
    (site == "F10" & split_id == 1) |
      (site == "NT2" & split_id == 13) |
      (site == "F11" & split_id == 49) |
      (site == "F9" & split_id == 107) |
      (site == "F12" & split_id == 193) |
      (site == "NT7" & split_id == 211)
  ) %>%
  ggplot(aes(x = year, y = mean, color = interaction(site, split_id))) +
  geom_line(linewidth = 1) +
  theme_classic() +
  theme(
    axis.text.x = element_text(color = "black", size = 12),
    axis.text.y = element_text(color = "black", size = 12),
    axis.title = element_text(size = 12),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    legend.title = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
  ) +
  scale_y_continuous(
    breaks = seq(0, max(preds_plots$mean, na.rm = TRUE), by = 0.5))+
  scale_color_manual(
    values = c(
      "F10.1" = "red",
      "NT2.13" = "#FF1493", 
      "F11.49" = "#FF69B4",
      "F9.107" = "#87CEEB", 
      "F12.193" = "#4682B4",
      "NT7.211" = "blue"  
    ),
    labels = c(
      "F10.1" = "F10.1 (NTMR)",
      "NT2.13" = "NT2.13 (Mixed - NTMR)",
      "F11.49" = "F11.49 (Mixed - NTMR)", 
      "F9.107" = "F9.107 (Mixed - Fished)",
      "F12.193" = "F12.193 (Mixed - Fished)",
      "NT7.211" = "NT7.211 (Fished)"  
    )
  ) +
  labs(x = "Year", y = "Prediction", color = "Site.Split ID")
g7
#ggsave(g7, filename = paste0("Plots/Inverts/Lobster/Figure_3.png"), dpi=1000)

# Load patchwork package
library(patchwork)

# Combine plots
# Update g5, g6, and g7 with modified axis labels and titles
g5 <- g5 + 
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.y = element_text(size = 18)
  ) +
  scale_x_continuous(breaks = seq(2016, 2023, by = 2), labels = function(x) substr(x, 3, 4))

g6 <- g6 + 
  theme(
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 18), 
    legend.position = "none"
  ) +
  scale_x_continuous(breaks = seq(2016, 2023, by = 2), labels = function(x) substr(x, 3, 4))

g7 <- g7 + 
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16)
  ) +
  scale_x_continuous(breaks = seq(2016, 2023, by = 2), labels = function(x) substr(x, 3, 4))

# Combine plots
combined_plot <- (g5 | g6 | g7) +
  plot_annotation(tag_levels = "A")+
  plot_layout(guides = "collect")
combined_plot


# Save combined plot
ggsave(combined_plot, filename = "Plots/Inverts/Lobster/Lobster_mega-plot_enviro-10_combined.png",
       width = 10, height = 4)

