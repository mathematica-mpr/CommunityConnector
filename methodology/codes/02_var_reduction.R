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
dict <- read.csv('../../data/data_dictionary.csv')
outcomes <- dict %>% 
  filter(outcome %in% 1) %>% 
  select(column_name)
outcomes <- gsub("[\\%,]","X.", outcomes$column_name)
outcomes <- gsub(" ", ".", outcomes)
outcomes
length(names(data))
use_data <- data %>% 
  select(-sdoh_score_1, -sdoh_score_2, -sdoh_score_3, -sdoh_score_4, -sdoh_score_5,
         -sdoh_score_6, -fips)
`%ni%` <- Negate(`%in%`)
use_data <- subset(use_data, select = names(use_data) %ni% outcomes)
length(names(use_data))

# replace with Nas with mean for now
for(i in 1:ncol(use_data)){
  use_data[is.na(use_data[,i]), i] <- mean(use_data[,i], na.rm = TRUE)
}

# check to make sure there are no NA correlations
cor_data <- cor(use_data)
cor_data[is.na(cor_data)]

# not sure why the matsim=TRUE parameter is not working
tree <- hclustvar(use_data)
plot(tree) # this is off the page??

# stability of the partitians in the dendrogram
# if columns are identical error, means there are some NAs?
# 25-40 errors out for some reason, 20 works
stab <- stability(tree, B=20, graph = TRUE)
stab$meanCR
plot(stab, main = "Stability of the partitians") # use around 22 clusters?
mat <- stab$matCR
boxplot(mat, main = "Dispersion of the adjusted Rand index")

results <- cutreevar(tree, 22)
print(results)
cluster <- results$cluster
princomp(use_data[,which(cluster==1)],cor=TRUE)$sdev^2
# ideally, would select 1 or more from each cluster, depending how similar they are
results$var
results$sim # need to use the matsim=TRUE parameter in hclustvar() - but this isn't working
results$scores # scores for each county & cluster

# can also try k-means partitioning instead of hierarchical
