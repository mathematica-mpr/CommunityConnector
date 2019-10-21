# Resource: https://arxiv.org/pdf/1112.0295.pdf

# cluster variables to obtain homogeneous clusters
# then, either select one variable from each
# or construct a synthetic variable (i.e. 1st PCA) for each cluster

# Common approach: dissimilarities between variables & apply cluster analysis
# i.e. stats::hclust or cluster::agnes
# cluster::diana and cluster::pam

# Dissimilarity matrix: correlation coefficients
# cateogircal: chi squared, etc.

# methods not in R:
# VARCLUS in SAS
# PCA methods by Vigneau and Qannari = Clustering around Latent Variables (CLV)
# or by Dhillon, Marcotte, Roshan = Diametrical Clustering

# missing data is allowed - replaced by means

# Sections 2 and 3

rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# devtools::install_github("chavent/ClustOfVar")
library(ClustOfVar)
library(tidyverse)

data <- read.csv('../../data/data_sdoh_scores.csv')
names(data)
use_data <- data %>% 
  select(-sdoh_score_1, -sdoh_score_2, -sdoh_score_3, -sdoh_score_4, -sdoh_score_5,
         -sdoh_score_6, -fips)

# check if there are any identical columns
for(i in c(1:length(names(use_data)))){
  for(j in c(1:length(names(use_data)))){
    
    if(i != j){
      col1 <- names(use_data)[i]
      col2 <- names(use_data)[j]
      
      if(identical(use_data[[col1]], use_data[[col2]])){
        print(col1)
        print(col2)
        print('')
      }
    }
  }
}

tree <- hclustvar(use_data)
plot(tree) # this is off the page??

# stability of the partitians in the dendrogram
stab <- stability(tree, B=40)
plot(stab, main = "Stability of the partitians")

