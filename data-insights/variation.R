setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(ggplot2)
library(dplyr)
library(tidyverse)
library(yaml)
library(ggcorrplot)

rm(list=ls())

list.files('../DockerShinyApp/app/data/')
data <- read.csv('../DockerShinyApp/app/data/final_data.csv')
dict <- read.csv('../DockerShinyApp/app/data/final_data_dictionary.csv')

# read in config file for colors
list.files('../DockerShinyApp/app/')
config <- read_yaml('../DockerShinyApp/app/config.yaml')
colors <- config$colors

sdoh_scores <- paste0("sdoh_score_",c(1:6))
summary(data[,sdoh_scores])
sdoh_names <- dict %>% 
  filter(column_name %in% sdoh_scores) %>% 
  select(description) %>% 
  pull() %>% 
  as.character()

# pivot data
long <- data %>% 
  select_at(sdoh_scores)
names(long) <- sdoh_names
long <- long %>% 
  gather(sdoh_group, sdoh_score) %>% 
  rename(`SDoH Pillar` = sdoh_group) %>% 
  mutate(`SDoH Pillar` = factor(`SDoH Pillar`, levels = c("Economic Stability","Neighborhood & Physical Environment",
                                                             "Education","Food","Community","Health Care System"))) %>% 
  arrange(`SDoH Pillar`) 
head(long)
unique(long$`SDoH Pillar`)

################################
########### Plot 1 #############
################################

# histograms of all scores

sdoh <- ggplot(data = long,
         aes(y = sdoh_score,
             group = `SDoH Pillar`,
             fill = str_wrap(`SDoH Pillar`, width = 20))) +
  geom_boxplot() +
  ylab("SDoH Score") +
  ggtitle("Social Determinants of Health Scores across Counties") +
  scale_fill_manual(values = c(colors$tan100, colors$green100, colors$grey100, colors$purple100, colors$yellow125, colors$teal100)) +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  labs(fill = "SDoH Pillar")
sdoh
ggsave('SDoH across Counties.png', sdoh)

################################
########### Plot 2 #############
################################

# correlation matrix between scores and outcomes
# all scores & one outcome of each type
outcomes <- dict %>% 
  filter(outcome == 1)
outcomes %>% 
  select(column_name, description)

corr_data <- data %>% 
  select_at(c(sdoh_scores, "overobese_pct","diabetes_prevalence_2016","chronic_kidney_disease_pct"))

names(corr_data) <- str_wrap(c(sdoh_names, "% Adults who are Overweight or Obese", "% Adults with Diabetes", "% Chronic Kidney Disease in Medicare age 65+"), width = 20)
summary(corr_data)
corr_data_cor <- cor(corr_data)

cplot <- ggcorrplot::ggcorrplot(corr_data_cor,
                       method = "square",
                       colors = c(colors$red100, colors$grey25, colors$teal100)) +
  ggtitle("Correlation across SDoH Scores and Outcomes")
cplot
ggsave('Correlation.png', cplot)
