setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(ggplot2)
library(dplyr)
library(tidyverse)
library("installMPR")
install_mpr("plotMPR")

rm(list=ls())

list.files('../DockerShinyApp/app/data/')

data <- read.csv('../DockerShinyApp/app/data/final_data.csv')
sdoh_scores <- paste0("sdoh_score_",c(1:6))
summary(data[,sdoh_scores])

# pivot data
long <- data %>% 
  select_at(sdoh_scores) %>% 
  gather(sdoh_group, sdoh_score)

# histograms of all scores
ggplot(data = long,
         aes(y = sdoh_score,
             group = sdoh_group,
             fill = sdoh_group)) +
  geom_boxplot()

# relationship of all scores & econ score
for(other_score in sdoh_scores[c(2:6)]){
  p <- ggplot(data = data,
         aes(x = sdoh_score_1,
             y = !!rlang::sym(other_score))) +
    geom_point()
  print(p)
}

# relationship of un-adjusted scores with econ score

# relationship