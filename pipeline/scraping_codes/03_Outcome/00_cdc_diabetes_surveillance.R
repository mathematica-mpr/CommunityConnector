setwd("~/AHRQ SDOH")


# prevalence
diab_prev_2011 <- read.csv("DiabetesAtlasCountyData_Prev2011.csv", header=FALSE)

diab_prev_2011=diab_prev_2011[-c(1,2,3,68),]
diab_prev_2011=diab_prev_2011[,-c(2,5,6)]


names(diab_prev_2011)<-c("county", "FIPS", "diabetes_prevalence_2011")

diab_prev_2016 <- read.csv("DiabetesAtlasCountyData_Prev2016.csv", header=FALSE)


diab_prev_2016=diab_prev_2016[-c(1,2,3,68),]
diab_prev_2016=diab_prev_2016[,-c(2,5,6)]

names(diab_prev_2016)<-c("county", "FIPS", "diabetes_prevalence_2016")

## incidence
diab_inc_2011 <- read.csv("DiabetesAtlasCountyData_Inc2011.csv", header=FALSE)

diab_inc_2011=diab_inc_2011[-c(1,2,3,68),]
diab_inc_2011=diab_inc_2011[,-c(2,5,6)]

names(diab_inc_2011)<-c("county", "FIPS","diabetes_incidence_2011")

diab_inc_2016 <- read.csv("DiabetesAtlasCountyData_Inc2016.csv", header=FALSE)


diab_inc_2016=diab_inc_2016[-c(1,2,3,68),]
diab_inc_2016=diab_inc_2016[,-c(2,5,6)]

names(diab_inc_2016)<-c("county", "FIPS","diabetes_incidence_2016")


# obesity
obese_prev_2011 <- read.csv("DiabetesAtlasCountyData_Obes2011.csv", header=FALSE)

obese_prev_2011=obese_prev_2011[-c(1,2,3,68),]
obese_prev_2011=obese_prev_2011[,-c(2,5,6)]

names(obese_prev_2011)<-c("county", "FIPS", "obesity_prevalence_2011")

obese_prev_2016 <- read.csv("DiabetesAtlasCountyData_Obes2016.csv", header=FALSE)


obese_prev_2016=obese_prev_2016[-c(1,2,3,68),]
obese_prev_2016=obese_prev_2016[,-c(2,5,6)]

names(obese_prev_2016)<-c("county", "FIPS","obesity_prevalence_2016")

colorado<-merge(diab_inc_2011, diab_inc_2016, by=c("county", "FIPS"), all.x = T, all.y = T)
colorado<-merge(colorado, diab_prev_2011, by=c("county", "FIPS"), all.x = T, all.y = T)
colorado<-merge(colorado, diab_prev_2016, by=c("county", "FIPS"), all.x = T, all.y = T)
colorado<-merge(colorado, obese_prev_2011, by=c("county", "FIPS"), all.x = T, all.y = T)
colorado<-merge(colorado, obese_prev_2016, by=c("county", "FIPS"), all.x = T, all.y = T)

colorado[,2:8] <- sapply(colorado[,2:8],as.character)
colorado[,2:8] <- sapply(colorado[,2:8],as.numeric)

colorado$diabetes_incidence_5_year_diff<-colorado$diabetes_incidence_2016-colorado$diabetes_incidence_2011
colorado$diabetes_prevalence_5_year_diff<-colorado$diabetes_prevalence_2016-colorado$diabetes_prevalence_2011
colorado$obesity_prevalence_5_year_diff<-colorado$obesity_prevalence_2016-colorado$obesity_prevalence_2011

## keep only 2016 and 5 year difference
colorado<-colorado[,c(1,2,4,6,8:11)]

write.csv(colorado, "CDC_Diabetes_Outcome_Data.csv", row.names=F)

