setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd('robert_wood_johnson_data/')

list.files(".")

library(xlsx)
# install.packages("assertr")
library(assertr)
library(dplyr)

fix_header <- function(dataframe){
  dataframe <- dataframe %>% 
    mutate_all(as.character)
  names(dataframe) <- paste(names(dataframe), dataframe[1, ], sep = "_")
  dataframe <- dataframe[-c(1),]
  names(dataframe) <- gsub("%","pct",gsub(" ","_",gsub("-","_",gsub("_+","",gsub("^\\d+","",gsub("\\.","",gsub("NA","",names(dataframe))))))))
  names(dataframe) <- gsub("#","num",names(dataframe))
  dataframe <- dataframe[,names(dataframe)[!grepl("Rank",names(dataframe)) & !grepl("Z_Score.",names(dataframe)) & !grepl("CI",names(dataframe))]]
  return(dataframe)
}

rankings <- xlsx::read.xlsx("2019 County Health Rankings Massachusetts Data - v1_0 (1).xls", sheetName = "Outcomes & Factors Rankings")
rankings <- rankings[,-c(8)]
rankings <- fix_header(rankings)
rankings

subrankings <- xlsx::read.xlsx("2019 County Health Rankings Massachusetts Data - v1_0 (1).xls", sheetName = "Outcomes & Factors SubRankings")
subrankings <- subrankings[,-c(16)]
subrankings <- fix_header(subrankings)
subrankings

ranked_data <- xlsx::read.xlsx("2019 County Health Rankings Massachusetts Data - v1_0 (1).xls", sheetName = "Ranked Measure Data")
ranked_data <- fix_header(ranked_data)
ranked_data

addl <- xlsx::read.xlsx("2019 County Health Rankings Massachusetts Data - v1_0 (1).xls", sheetName = "Additional Measure Data")
addl <- fix_header(addl)
addl

# merge all data into one
names(rankings)
names(subrankings)
names(ranked_data)
names(addl)

data <- Reduce(function(x,y) merge(x, y, by = c("FIPS","State","County")), list(rankings, subrankings, ranked_data, addl))
# remove overall massachusetts
data <- data[!is.na(data$County),]

# cluster to find counties that are similar to one another
names(data)

clusVars <- !names(data) %in% names(data)[c(1:11)] & !grepl("Z_Score",names(data))
names(data)[clusVars]

# standardize variables first
clusData <- data[,names(data)[clusVars]]
clusData <- clusData %>% 
  mutate_all(as.numeric)
clusData <- data.frame(sapply(clusData, scale))

# missing value imputation
colSums(is.na(clusData))

# install.packages("Amelia")
library(Amelia)

Amelia::missmap(clusData)

amelia(clusData, m = 3, parallel = "multicore")
# remove columns accoring to error
data$LowbirthweightUnreliable
clusData_rem <- clusData[,!names(clusData) %in% c("LowbirthweightUnreliable", "PCP_Ratio",
                                                  "Dentist_Ratio", "MHP_Ratio", "DrinkingwaterviolationsPresence_of_violation",
                                                  "Other_PCP_Ratio", "Childreneligibleforfreeorreducedpricelunchpct_Free_or_Reduced_Lunch")]
Amelia::missmap(clusData_rem)
amelia(clusData_rem, m = 3, parallel = "multicore")
# we have too few observations with only data for massachusetts
# for now, drop any variables that have NAs, but in the future we will impute

nas <- data.frame(colSums(is.na(clusData_rem)))
nas
nas$var <- row.names(nas)
nas <- nas[nas$colSums.is.na.clusData_rem.. > 0,]
nas

clusData_final <- clusData_rem[,!names(clusData_rem) %in% unique(nas$var)]

set.seed(786)
dist_mat <- dist(clusData_final, method = 'euclidean')
dist_mat
hclust_avg <- hclust(dist_mat, method = 'average')
par(mfrow=c(1,1))
plot(hclust_avg)
# can also cut by h
rect.hclust(hclust_avg, k = 4, border = 2:6)

hclust_avg

cut_avg <- cutree(hclust_avg, k = 4)
cut_avg

# install.packages("dendextend")
library(dendextend)
dend_obj <- as.dendrogram(hclust_avg)
avg_col_dend <- color_branches(dend_obj, k = 4)
plot(avg_col_dend)

# plot them on a map
clusData_final$cluster <- as.factor(cut_avg)
final <- cbind(data, clusData_final[,c("cluster")])
final$row <- rownames(final)
names(final)[207] <- "cluster"
names(final)
unique(final[,c("FIPS","County","cluster")])

# within groups, show differences in health outcomes
plot(final$HealthOutcomesZ_Score, final$HealthFactorsZ_Score, col = final$cluster)

library(devtools)
# devtools::install_github("UrbanInstitute/urbnmapr")
library(urbnmapr)

# plot clusters as colors
# BUT there will likely be lots of groups
u %>% 
  filter(state_abbv == "MA") %>% 
  merge(final, by.y = "FIPS", by.x = "county_fips") %>% 
  ggplot(mapping = aes(long, lat, group = group, fill = cluster)) +
  geom_polygon(color = "#ffffff")

# or plot how similar counties are to X county?
# select cambridge
selection <- "Middlesex"

# find row of that county in the final dataset
r <- as.numeric(final[final$County %in% selection,"row"])
r <- r-1
# find the distances to other counties
matrix <- as.matrix(dist_mat)
mat_data <- data.frame(matrix[,r])
names(mat_data) <- paste0("dist_from_selection")
mat_data

graph_data <- cbind(final, mat_data)

colfunc <- colorRampPalette(c("purple","white"))
colfunc(14)

u %>% 
  filter(state_abbv == "MA") %>% 
  merge(graph_data, by.y = "FIPS", by.x = "county_fips") %>% 
  ggplot(mapping = aes(long, lat, group = group, fill = dist_from_selection)) +
  scale_fill_gradient(low = "purple", high = "white") +
  # geom_polygon()
  # geom_polygon(color = "#ffffff")
  geom_polygon(color = "#000000")

graph_data[,c("County","dist_from_selection")]
matrix
mat_data
graph_data

# looking at both counties in 3 cluster
# how do outcomes compare?
names(final)
