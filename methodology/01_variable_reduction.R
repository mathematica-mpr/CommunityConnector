setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(ggplot2)                 #graphing
library(ggcorrplot)              #correlations graphs
library(GGally)                  #correlations graphs
library(corrr)                   #correlations graphs
library(Amelia)                  #visualizing missing data
library(missForest)              #imputing missing data
library(randomForest)            #For random forests and VarImpPlots
library(grid)                    #organizing plots in one figure
library(gridExtra)               #organizing plots in one figure
library(glmnet)                  #for running lasso and ridge regression
library(dplyr)                   #rownames to col
library(devtools)       
library(ggbiplot)                #pca plots
library(tidyverse)
library(rlang)
library(lazyeval)
library(purrr)
library(sparsepca)
library(ClustOfVar)

#data-----

#inputting data
sdohallorig <- read.csv('../data/data_2_selected_variables.csv')
dictionaryorig <- read.csv("../data/dictionary_1_manual.csv")

#removing score and county/state data
toremove <- c("sdoh_score_1", "sdoh_score_2", "sdoh_score_3", "sdoh_score_4", "sdoh_score_5", "sdoh_score_6", "state", "county")
dictionary <- dictionaryorig[!dictionaryorig$column_name %in% toremove,]  
sdohall <- sdohallorig[,!names(sdohallorig) %in% toremove]

#imputing missing data
set.seed(1234)
sdoh_numeric <- missForest(sdohall)
sdoh_numeric <- sdoh_numeric$ximp

#list of 6 datasets for each score
datlist <- list()
for (i in 1:6) {
  index <- which(dictionary[,i+7]==1)
  datlist[[i]] <- sdoh_numeric[,index]
}
#unlisting and naming new datasets
for (i in seq(datlist)) {
  assign(paste0("Score", i), datlist[[i]])
}

#SPCA-----

varreduc_spca <- function(data, alpha) {
  #function to output non-zero variables and loadings
  spca <- robspca(data, center = T, scale = T, verbose = F, alpha = alpha)
  cum <- summary(spca)[4,]
  exp <- summary(spca)[3,]
  
  #skree plot
  plot <- ggplot(mapping = aes(x = seq_along(cum), y = cum)) +
    geom_point() +
    scale_x_continuous(breaks = pretty_breaks(10)) +
    ggtitle("Sparse PCA: Cumulative Skree Plot") +
    xlab("Number of Principal Component") +
    ylab("Cumulative % of Variation Explained") +
    ylim(0,1) +
    theme_bw()
  print(plot)
  allcomps <- list()
  
  #creating data frame with variable name, category, PC#, var explained, and loading
  for (i in 1:dim(data)[2]) {
    comps <- spca$loadings[,i]
    comps <- as.data.frame(comps)
    rownames(comps) <- colnames(data)
    comps <- tibble::rownames_to_column(comps)
    score <- deparse(substitute(data))
    score <- str_remove(score, "Score")
    comps[,3] <- as.numeric(score)
    comps[,4] <- i
    comps[,5] <- exp[i]
    names(comps)[1:5] <- c("Variable_Name", "Loading", "sdoh_Category", "PC_Number", "Variance_Explained")
    zeros <- which(comps[,2]==0)
    comps <- comps[-zeros,]
    allcomps[[i]] <- comps
  }
  return(allcomps)
}

varreduc_uni <- function(list, num.comp) {
  #function to output list of uniquely selected variables
  all <- list[1:num.comp] 
  all <- ldply(all)
  unique <- unique(all$Variable_Name)
  return(unique)
}

#Testing Tuning Parameter-----
alpmin20 <- robspca(Score6, center = T, scale = T, verbose = F, alpha = 1e-20)
cum_var_exp <- summary(test2)[4,]
plot(cum_var_exp, pch = 20, ylim = c(0,1), main = "Testing Parameters for Score 6")

alpmin10<- robspca(Score6, center = T, scale = T, verbose = F, alpha = 1e-10)
cum_var_exp <- summary(test1)[4,]
points(cum_var_exp, pch = 20, type = "l", lwd = 2, col = "red")

alpmin4 <- robspca(Score6, center = T, scale = T, verbose = F, alpha = .0001)
cum_var_exp <- summary(test2)[4,]
points(cum_var_exp, pch = 20, type = "l", lwd = 2, col = " green", lty = 2)

alp005 <- robspca(Score6, center = T, scale = T, verbose = F, alpha = .005)
cum_var_exp <- summary(test3)[4,]
points(cum_var_exp, pch = 20, type = "l", lwd = 2, col = "pink")

alp01 <- robspca(Score6, center = T, scale = T, verbose = F, alpha = .01)
cum_var_exp <- summary(test4)[4,]
points(cum_var_exp, pch = 20, type = "l", lwd = 2, col = "blue")

alp05 <- robspca(Score6, center = T, scale = T, verbose = F, alpha = .05)
cum_var_exp <- summary(test5)[4,]
points(cum_var_exp, pch = 20, type = "l", lwd = 2, col = "purple")

alp1 <- robspca(Score6, center = T, scale = T, verbose = F, alpha = .1)
cum_var_exp <- summary(test6)[4,]
points(cum_var_exp, pch = 20, type = "l", lwd = 2, col = "orange")

#Results-----

#running SPCA on all Scores
S1PC <- varreduc_spca(Score1, .01) #5
S2PC <- varreduc_spca(Score2, .01) #13
S3PC <- varreduc_spca(Score3, .01) #3
S4PC <- varreduc_spca(Score4, .01) #2
S5PC <- varreduc_spca(Score5, .01) #8
S6PC <- varreduc_spca(Score6, .01) #8

#number of principal components used. 
num1 <- 5
num2 <- 13
num3 <- 3
num4 <- 2
num5 <- 8
num6 <- 8

#selected variables
varreduc_uni(S1PC, num1)
varreduc_uni(S2PC, num2)
varreduc_uni(S3PC, num3)
varreduc_uni(S4PC, num4)
varreduc_uni(S5PC, num5)
varreduc_uni(S6PC, num6)

#flagging dictionary-----

#creating new dataframe with all variables
Dictionary_PostSPCA <- as.data.frame(dictionaryorig$column_name)
names(Dictionary_PostSPCA) <- "Variable_Name"

#combining lists of PC into one df
all <- c(S1PC, S2PC, S3PC, S4PC, S5PC, S6PC) %>% 
  ldply()

#Post SPCA manually selected variables
varadd <- c("budget_water", "pct_physically_inactive")
test <- function(varname, df) {
  index <- which(df$Variable_Name==varname)
  PCinfo <- df[index,]
  return(PCinfo)
}
pc_add <- lapply(varadd, test, df = all)

#combining spca and manually selected variables
all_selected <- c(S1PC[1:num1],S2PC[1:num2],S3PC[1:num3],S4PC[1:num4],S5PC[1:num5],S6PC[1:num6]) %>% 
  c(pc_add) %>% 
  ldply() %>% 
  unique()

#populating dataframe with SPCA information
for (i in 1:length(all_selected$Variable_Name)) {
  index <- which(Dictionary_PostSPCA$Variable_Name==all_selected$Variable_Name[i])
  Dictionary_PostSPCA[index, 2] <- all_selected$sdoh_Category[i]
  Dictionary_PostSPCA[index, 3] <- all_selected$PC_Number[i]
  Dictionary_PostSPCA[index, 4] <- all_selected$Variance_Explained[i]
  Dictionary_PostSPCA[index, 5] <- all_selected$Loading[i]
}
names(Dictionary_PostSPCA)[2:5] <- c("sdoh_Category", "PC_Number", "Variance_Explained", "Loading")

#Post SPCA Removals
remove <- c("food_environment_index", "pct_frequent_mental_distress", "short_hosp_pp_rate", 
            "pct.adult.uninsured", "medicare_std_adj_cost_pp")
remove_index <- which(Dictionary_PostSPCA$Variable_Name %in% remove)
Dictionary_PostSPCA[remove_index, 2:5] <- NA

#outputting new dictionary
write.csv(Dictionary_PostSPCA, "../data/DictionaryPostSPCA.csv", na = "", row.names = F)
