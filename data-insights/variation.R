setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(ggplot2)
library(dplyr)
library(tidyverse)
library(yaml)

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

################################
########### Plot 1 #############
################################

# histograms of all scores

ggplot(data = long,
         aes(y = sdoh_score,
             group = `SDoH Pillar`,
             fill = `SDoH Pillar`)) +
  geom_boxplot() +
  ylab("SDoH Score") +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  ggtitle("Social Determinants of Health Scores across Counties") +
  scale_fill_manual(values = c(colors$tan100, colors$green100, colors$grey100, colors$purple100, colors$yellow125, colors$teal100))

################################
########### Plot 2 #############
################################

# relationship of all scores & econ score
for(other_score in sdoh_scores[c(2:6)]){
  p <- ggplot(data = data,
         aes(x = sdoh_score_1,
             y = !!rlang::sym(other_score))) +
    geom_point()
  print(p)
}

# relationship of un-adjusted scores with econ score

# relationship of outcomes with scores
data$quartile <- cut(data$overobese_pct,4)

for(other_score in sdoh_scores){
  p <- ggplot(data = data,
              aes(x = quartile,
                  y = !!rlang::sym(other_score))) +
    geom_boxplot()
  print(p)
}
