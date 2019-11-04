
##############
###PACKAGES###
##############

install.packages("ggplot2")                 
install.packages("ggcorrplot")              
install.packages("GGally")                  
install.packages("corrr")                   
install.packages("Amelia")                  
install.packages("missForest")              
install.packages("randomForest")            
install.packages("grid")                    
install.packages("gridExtra")               
install.packages("glmnet")                  
install.packages("dplyr")
install.packages("devtools")
devtools::install_github("vqv/ggbiplot")
install.packages("tidyverse")
install.packages("sparsepca")
install.package("elasticnet")
#install.packages("psycho")

library(ggplot2)                 #graphing
library(ggcorrplot)              #correlations graphs
library(GGally)                  #correlations graphs
library(corrr)                   #correlations graphs
library(Amelia)                  #visualizing missing data
library(missForest)              #imputing missing data
library(RandomForest)            #For random forests and VarImpPlots
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

##########
###DATA###
##########

#data sets
sdohall <- read.csv("C:/Users/ECody/Desktop/AHRQProj/CommunityConnector/data/final_data.csv")
dictionary <- read.csv("C:/Users/ECody/Desktop/AHRQProj/CommunityConnector/data/final_data_dictionary.csv")
#removing score
dictionary <- dictionary[-(89:94),] 
sdohall <- sdohall[-65,-(119:124)]  

#cleaning data
cleandata <- function(data) {
  #function to rename variables
  colnames(data) <- gsub("_", ".", colnames(data))
  #Removing non numeric variables
  numeric_boolean <- sapply(data, is.numeric)
  data <- data[ ,numeric_boolean]
  return(data)
}
sdohall <- cleandata(sdohall)

#imputing missing data
set.seed(1234)
sdoh_numeric <- missForest(sdohall)
sdoh_numeric <- sdoh_numeric$ximp

#removed index and doubles of demographics
sdoh_numeric <- sdoh_numeric[,-c(3, 5, 6, 26, 27)]  
dictionary <- dictionary[-c(3, 5, 6, 26, 27),]
names(sdoh_numeric)[103] <- "Gini.Index.Income.Inequal"
names(sdoh_numeric)[92] <- "American.Indian.Alaskan.Native.Alone"
names(sdoh_numeric)[93] <- "Asian.Alone"
names(sdoh_numeric)[94] <- "Black.or.African.America.Alone"
names(sdoh_numeric)[95] <- "Native.Hawiian.Other.Pacific.Islander.Alone"
names(sdoh_numeric)[96] <- "Some.Other.Race.Alone"

#multiple data sets for each SDOH Score
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

############
###GRAPHS###
############

#Checking Doubles
p1 <- ggplot(sdohall, aes(x = diabetes_pct, y = P.diabetic)) +
  geom_point() +
  ggtitle("Comparing % Diabetes Measures")
p2 <- ggplot(sdohall, aes(x = race_estimate_total_american_indian_and_alaska_native_alone, y = P.american.indian.alaskan.native)) +
  geom_point() +
  ggtitle("Comparing % American Indian and Alaska Native Measures")
p3 <- ggplot(sdohall, aes(x = race_estimate_total_asian_alone, y = P.asian)) +
  geom_point() +
  ggtitle("Comparing % Asian Measures")
p4 <- ggplot(sdohall, aes(x = race_estimate_total_black_or_african_american_alone, y = P.african.american)) +
  geom_point() +
  ggtitle("Comparing % African American Measures")
p5 <- ggplot(sdohall, aes(x = race_estimate_total_native_hawaiian_and_other_pacific_islander_alone, 
                         y = P.native.hawaiian.other.pacific.islander)) +
  geom_point() +
  ggtitle("Comparing % Native Hawaiian Measures")
p6 <- ggplot(sdohall, aes(x = race_estimate_total_white_alone, y = P.non.hispanic.white)) +
  geom_point() +
  ggtitle("Comparing % African American Measures")
gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6, top = "Comparisons")

#Outcomes
list_of_outcomes <- c("P.obese", "diabetes_pct", "chronic.kidney.disease_pct", "chronic.kidney.disease_std_spend",
                      "diabetes_std_spend")

outcomes_index <- vector()
for (i in 1:length(list_of_outcomes)) {
  outcomes_index[i] <- which(colnames(sdoh_numeric) == list_of_outcomes[i])
}


p7 <- ggplot(sdoh_numeric, aes(x = P.obese)) +
  geom_density(fill = "dark blue", alpha = .5) +
  theme_bw()
p8 <- ggplot(sdoh_numeric, aes(x = diabetes_pct)) + 
  geom_density(fill = "dark blue", alpha = .5) +
  theme_bw()
p9 <- ggplot(sdoh_numeric, aes(x = chronic.kidney.disease_pct)) +
  geom_density(fill = "dark blue", alpha = .5) +
  theme_bw()
p10 <- ggplot(sdoh_numeric, aes(x = chronic.kidney.disease_std_spend)) +
  geom_density(fill = "dark blue", alpha = .5) +
  theme_bw()
gridExtra::grid.arrange(p7, p8, p9, p10, top = "Density Plots of Outcomes")


###Correlation

#correlation matrix
corr <- as.data.frame(cor(sdoh_numeric))

#all correlations
ggcorr(sdoh_numeric, hjust = 1, size = 3, layout.exp = 1)                            
ggcorr(sdoh_numeric, nbreaks = 4, hjust = 1, size = 3)


GraphCorr <- function(cdata, outcome) {
  #function to output bar graphs of correlations for any specific variable  
  
  #index of outcome of interest
  outcome_index <- which(colnames(cdata) == outcome)
  #indexes of other outcomes
  index <- vector()                                                                 
  for (i in 1:length(list_of_outcomes)) {
    index[i] <- which(colnames(corr) == list_of_outcomes[i])
  }
  
  #subset of correlations to outcome from full corr matrix
  outcome_corr <- as.data.frame(cdata[,outcome_index])
  #cleaning outputted data
  outcome_corr[,2] <- rownames(cdata)                                                
  names(outcome_corr)[1] <-"Corr"                                                     
  outcome_corr <- outcome_corr[-index,]                                              
  
  #plotting correlations
  graph <- ggplot(outcome_corr, aes(x = reorder(V2, Corr), y = Corr)) +
    geom_bar(stat = "identity") + 
    ggtitle(paste("Variable Correlations for Outcome:", outcome)) +
    xlab("SDOH Variables") +
    ylab("Correlation") +
    coord_flip() +
    ylim(-1,1) +
    theme_bw()
  
  print(graph)
}

#graphs for all outcomes
lapply(list_of_outcomes, GraphCorr, cdata = corr)


p16 <- ggcorr(Score1, hjust = 1, size = 4, layout.exp = 1) + 
  ggtitle("Score 1: Economic Stability")
p17 <- ggcorr(Score2, hjust = 1, size = 4, layout.exp = 1) + 
  ggtitle("Score 2: Neighborhood & Physical Environment")
p18 <- ggcorr(Score3, hjust = 1, size = 4, layout.exp = 1) + 
  ggtitle("Score 3: Education")
p19 <- ggcorr(Score4, hjust = 1, size = 4, layout.exp = 1) + 
  ggtitle("Score 4: Food")
p20 <- ggcorr(Score5, hjust = 1, size = 4, layout.exp = 1) + 
  ggtitle("Score 5: Community")
p21 <- ggcorr(Score6, hjust = 1, size = 4, layout.exp = 1) + 
  ggtitle("Score 6: Health Coverage")
gridExtra::grid.arrange(p16, p18, p19, top = "Correlations by Score", ncol = 2)

### Other Relationships of Interest

other_interests <- c("P.excessive.drinking", "P.children.in.poverty", "P.physically.inactive")
lapply(other_interests, GraphCorr, cdata = corr)

p11 <- ggplot(sdoh_numeric, aes(x = mentally.unhealthy.days, y = physically.unhealthy.days)) +                  
  geom_point() +
  ggtitle("% Mentally/Physically Unealthy Days") +
  theme_bw()
p12 <- ggplot(sdoh_numeric, aes(x = P.frequent.mental.distress, y = P.frequent.physical.distress)) +             
  geom_point() +
  ggtitle("% Frequent Mental/Physical Distress") +
  theme_bw()
p13 <- ggplot(sdoh_numeric, aes(x = mentally.unhealthy.days, y = P.frequent.mental.distress)) +
  geom_point() +
  ggtitle("Frequent Distress & Unhealthy Days") +
  theme_bw()
p14 <- ggplot(sdoh_numeric, aes(x = median_income, y = P.excessive.drinking)) +
  geom_point() +
  ggtitle("Median Income") +
  theme_bw()
p15 <- ggplot(sdoh_numeric, aes(x = median_value, y = P.excessive.drinking)) +
  geom_point() +
  ggtitle("Median Value") +
  theme_bw()
gridExtra::grid.arrange(p11, p12, p13, top = "Correlations", ncol = 3)
gridExtra::grid.arrange(p14, p15, top = "Excessive Drinking Collinearity", ncol = 2)

###Variable Importance Plots

#random forests for each outcome
obese_rf <- randomForest(P.obese ~ ., data = sdoh_numeric)
diabetes_rf <- randomForest(P.diabetic ~ ., data = sdoh_numeric)
diabetesCMS_rf <- randomForest(diabetes_pct ~ ., data = sdoh_numeric)
kidP_rf <- randomForest(chronic.kidney.disease_pct ~ ., data = sdoh_numeric)
kidSpend_rf <- randomForest(chronic.kidney.disease_std_spend ~ ., data = sdoh_numeric)
DiaSpend_rf <- randomForest(diabetes_std_spend ~ ., data = sdoh_numeric)

#variable importance plot for each outcome
varImpPlot(obese_rf, main = "VIP for % Obesity", pch = 20)
varImpPlot(diabetes_rf, main = "VIP for % Diabetes", pch = 20)
varImpPlot(diabetesCMS_rf, main = "VIP for CMS % Diabetes", pch = 20)
varImpPlot(kidP_rf, main = "VIP for % Chronic Kidney Disease", pch = 20)
varImpPlot(kidSpend_rf, main = "VIP for Standardized CMS Spend on Chronic Kidney Disease", pch = 20)
varImpPlot(DiaSpend_rf, main = "VIP for Standardized CMS Spend on Diabetes", pch = 20)


###Scatter Plots

GraphScatter <- function(data, varname, title) {
  #function to plot variable name against all outcomes
  p1 <- ggplot(data, aes(x = !!varname, y = P.obese)) +
    geom_point()
  p2 <- ggplot(data, aes(x = !!varname, y = P.diabetic)) +
    geom_point()
  p3 <- ggplot(data, aes(x = !!varname, y = chronic.kidney.disease_pct)) +
    geom_point()
  p4 <- ggplot(data, aes(x = !!varname, y = chronic.kidney.disease_std_spend)) +
    geom_point()
  #arranging plots in grid
  all <- gridExtra::grid.arrange(p1, p2, p3, p4, top = title)
  return(all)
}

#some variables of interest
GraphScatter(sdoh_numeric, quo(P.excessive.drinking), "% Exessive Drinking Scatter Plot for All Outcomes")
GraphScatter(sdoh_numeric, quo(P.physically.inactive), "% Physically Inactive Scatter Plot for All Outcomes")
GraphScatter(sdoh_numeric, quo(P.frequent.mental.distress), "% Frequent Mental Distress Scatter Plot for All Outcomes")
GraphScatter(sdoh_numeric, quo(P.insufficient.sleep), "% Insufficient Sleep Scatter Plot for All Outcomes")
GraphScatter(sdoh_numeric, quo(P.drive.alone), "% Drives Alone Scatter Plot for All Outcomes")
GraphScatter(sdoh_numeric, quo(graduation.rate), "% Graduation Rate Scatter Plot for All Outcomes")

#checking for non linear relationships
#for (i in 1:68) {
#  a <- ggplot(sdoh_numeric, aes(x = sdoh_numeric[,i], y = life.expectancy)) +
#    geom_point() +
#    ggtitle(print(i))
#  b <- ggplot(sdoh_numeric, aes(x = sdoh_numeric[,i], y = age.adjusted.mortality)) +
#    geom_point()
#  c <- ggplot(sdoh_numeric, aes(x = sdoh_numeric[,i], y = P.obese)) +
#    geom_point()
#  d <- ggplot(sdoh_numeric, aes(x = sdoh_numeric[,i], y = P.diabetic)) +
#    geom_point()
#  gridExtra::grid.arrange(a, b, c, d)
#}

###################################
###POST VARIABLE SELECTION PLOTS###
###################################

ggplot(sdoh_numeric, aes(x = median.income, y = pct.staying.in.same.tract.as.adults.rp.gp.pall)) +
  geom_point() + 
  geom_smooth(method = "lm") +
  ggtitle("Comparing Stay in Same Tract to Median Income") +
  theme_bw()

a <- ggplot(sdoh_numeric, aes(y = pct.alcohol.impaired, x = pct.excessive.drinking)) +
  geom_point() + 
  geom_smooth(method = "lm") +
  ggtitle("Comparing % Alcohol Impaired to % Excessive Drinking") +
  theme_bw()
b <- ggplot(sdoh_numeric, aes(y = pct.alcohol.impaired, x = median.income)) +
  geom_point() + 
  geom_smooth(method = "lm") +
  ggtitle("Comparing % Alcohol Impaired to Median Income") +
  theme_bw()
c <- ggplot(sdoh_numeric, aes(y = pct.alcohol.impaired, x = pct.unemployed)) +
  geom_point() + 
  geom_smooth(method = "lm") +
  ggtitle("Comparing % Alcohol Impaired to % Unemployed") +
  theme_bw()
d <- ggplot(sdoh_numeric, aes(y = pct.alcohol.impaired, x = ann.avg.job.growth.2004.2013)) +
  geom_point() + 
  geom_smooth(method = "lm") +
  ggtitle("Comparing % Alcohol Impaired to Annual Job Growth") +
  theme_bw()

grid.arrange(a,b,c,d, top = "Percent Alcohol Impaired")

e <- ggplot(sdoh_numeric, aes(x = budget.air, y = pct.good.air)) +
  geom_point() + 
  #geom_smooth(method = "lm") +
  ggtitle("Budget Air and % Good Air") +
  theme_bw()

f <- ggplot(sdoh_numeric, aes(x = budget.disease, y = pct.vaccinated)) +
  geom_point() + 
  #geom_smooth(method = "lm") +
  ggtitle("Budget Disease and % Vaccinated") +
  theme_bw()

g <- ggplot(sdoh_numeric, aes(x = budget.prevention, y = pct.vaccinated)) +
  geom_point() + 
  #geom_smooth(method = "lm") +
  ggtitle("Budget Prevention and % Vaccinated") +
  theme_bw()

i <- ggplot(sdoh_numeric, aes(x = budget.prevention, y = pct.smokers)) +
  geom_point() + 
  #geom_smooth(method = "lm") +
  ggtitle("Budget Prevention and % Smokers") +
  theme_bw()
grid.arrange(f, g, i, e)

j <- ggplot(sdoh_numeric, aes(x = budget.health.equity, y = pcp.rate)) +
  geom_point() + 
  #geom_smooth(method = "lm") +
  ggtitle("Budget Health Equity and PCP Rate") +
  theme_bw()
k <- ggplot(sdoh_numeric, aes(x = budget.health.equity, y = pct.uninsured)) +
  geom_point() + 
  #geom_smooth(method = "lm") +
  ggtitle("Budget Health Equity and % Uninsured") +
  theme_bw()
grid.arrange(j, k)


m <- ggplot(sdoh_numeric, aes(x = budget.health.svcs, y = hosp.pp.rate)) +
  geom_point() + 
  #geom_smooth(method = "lm") +
  ggtitle("Budget Health Facilities and Hosp. PP Rate") +
  theme_bw()
n <- ggplot(sdoh_numeric, aes(x = budget.health.svcs, y = comm.hlth.cntrs.pp.rate)) +
  geom_point() + 
  #geom_smooth(method = "lm") +
  ggtitle("Budget Health Facilities and Community Health Centers") +
  theme_bw()
grid.arrange(m, n)

ggplot(sdoh_numeric, aes(x = median.income, y = budget.environmental)) + geom_point()

##################
###DEMOGRAPHICS###
##################

demographics <- sdoh_numeric[,which(dictionary$demographic==1)]

ggcorr(demographics, hjust = 1, size = 3, layout.exp = 1)
ggcorr(demographics, geom = "blank", label = TRUE, hjust = 1, label_size = 3) +
  geom_point(size = 7, aes(color = coefficient > 0, alpha = abs(coefficient) > 0.75)) +
  scale_alpha_manual(values = c("TRUE" = 0.25, "FALSE" = 0)) +
  guides(color = FALSE, alpha = FALSE)



