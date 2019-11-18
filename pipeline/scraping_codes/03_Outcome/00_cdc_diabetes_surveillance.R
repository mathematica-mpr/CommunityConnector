setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd('../../../data/raw/')

library(readxl)

# prevalence
diab_prev_2011 <- read_excel("DiabetesAtlasCountyData_Prev2011.xlsx")

diab_prev_2011=diab_prev_2011[-c(1,2,67),]
diab_prev_2011=diab_prev_2011[,-c(2,4,5)]


names(diab_prev_2011)<-c("county", "FIPS", "diabetes_prevalence_2011")

diab_prev_2016 <- read_excel("DiabetesAtlasCountyData_Prev2016.xlsx")


diab_prev_2016=diab_prev_2016[-c(1,2,67),]
diab_prev_2016=diab_prev_2016[,-c(2,4,5)]

names(diab_prev_2016)<-c("county", "FIPS", "diabetes_prevalence_2016")

## incidence
diab_inc_2011 <- read_excel("DiabetesAtlasCountyData_Inc2011.xlsx")

diab_inc_2011=diab_inc_2011[-c(1,2,67),]
diab_inc_2011=diab_inc_2011[,-c(2,4,5)]

names(diab_inc_2011)<-c("county", "FIPS","diabetes_incidence_2011")

diab_inc_2016 <- read_excel("DiabetesAtlasCountyData_Inc2016.xlsx")


diab_inc_2016=diab_inc_2016[-c(1,2,67),]
diab_inc_2016=diab_inc_2016[,-c(2,4,5)]

names(diab_inc_2016)<-c("county", "FIPS","diabetes_incidence_2016")


# obesity
obese_prev_2011 <- read_excel("DiabetesAtlasCountyData_Obes2011.xlsx")

obese_prev_2011=obese_prev_2011[-c(1,2,67),]
obese_prev_2011=obese_prev_2011[,-c(2,4,5)]

names(obese_prev_2011)<-c("county", "FIPS", "obesity_prevalence_2011")

obese_prev_2016 <- read_excel("DiabetesAtlasCountyData_Obes2016.xlsx")


obese_prev_2016=obese_prev_2016[-c(1,2,67),]
obese_prev_2016=obese_prev_2016[,-c(2,4,5)]

names(obese_prev_2016)<-c("county", "FIPS","obesity_prevalence_2016")

colorado<-merge(diab_inc_2011, diab_inc_2016, by=c("county", "FIPS"), all.x = T, all.y = T)
colorado<-merge(colorado, diab_prev_2011, by=c("county", "FIPS"), all.x = T, all.y = T)
colorado<-merge(colorado, diab_prev_2016, by=c("county", "FIPS"), all.x = T, all.y = T)
colorado<-merge(colorado, obese_prev_2011, by=c("county", "FIPS"), all.x = T, all.y = T)
colorado<-merge(colorado, obese_prev_2016, by=c("county", "FIPS"), all.x = T, all.y = T)

colorado[,2:8] <- sapply(colorado[,2:8],as.numeric)

colorado$diabetes_incidence_5_year_diff<-colorado$diabetes_incidence_2016-colorado$diabetes_incidence_2011
colorado$diabetes_prevalence_5_year_diff<-colorado$diabetes_prevalence_2016-colorado$diabetes_prevalence_2011
colorado$obesity_prevalence_5_year_diff<-colorado$obesity_prevalence_2016-colorado$obesity_prevalence_2011

## keep only 2016 and 5 year difference
colorado<-colorado[,c(2,4,6,8:11)]

head(colorado)

write.csv(colorado, "../cleaned/03_Outcome/CDC_Diabetes_Outcome_Data.csv", row.names=F)