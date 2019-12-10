setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd('../../../data/raw')

library(assertthat)

process_atlas <- function(csv, metric){
  data <- read.csv(csv, skip = 2)
  print(names(data))
  # remove junk rows
  rate_col <- ifelse("Percentage" %in% names(data),"Percentage","Rate.per.1000")
  data=data[!is.na(data$CountyFIPS),c("County","CountyFIPS",rate_col)]
  names(data)<-c("county", "FIPS", metric)
  print(head(data))
  print(assert_that(nrow(data) == 64))
  return(data)
}

diab_prev_2011 <- process_atlas("DiabetesAtlasCountyData_Prev2011.csv","diabetes_prevalence_2011")
diab_prev_2016 <- process_atlas("DiabetesAtlasCountyData_Prev2016.csv", "diabetes_prevalence_2016")
diab_inc_2011 <- process_atlas("DiabetesAtlasCountyData_Inc2011.csv", "diabetes_incidence_2011")
diab_inc_2016 <- process_atlas("DiabetesAtlasCountyData_Inc2016.csv", "diabetes_incidence_2016")
obese_prev_2011 <- process_atlas("DiabetesAtlasCountyData_Obes2011.csv", "obesity_prevalence_2011")
obese_prev_2016 <- process_atlas("DiabetesAtlasCountyData_Obes2016.csv", "obesity_prevalence_2016")

colorado<-merge(diab_inc_2011, diab_inc_2016, by=c("county", "FIPS"), all.x = T, all.y = T)
colorado<-merge(colorado, diab_prev_2011, by=c("county", "FIPS"), all.x = T, all.y = T)
colorado<-merge(colorado, diab_prev_2016, by=c("county", "FIPS"), all.x = T, all.y = T)
colorado<-merge(colorado, obese_prev_2011, by=c("county", "FIPS"), all.x = T, all.y = T)
colorado<-merge(colorado, obese_prev_2016, by=c("county", "FIPS"), all.x = T, all.y = T)
nrow(colorado)

head(colorado)
summary(colorado)

colorado$diabetes_incidence_5_year_diff<-colorado$diabetes_incidence_2016-colorado$diabetes_incidence_2011
colorado$diabetes_prevalence_5_year_diff<-colorado$diabetes_prevalence_2016-colorado$diabetes_prevalence_2011
colorado$obesity_prevalence_5_year_diff<-colorado$obesity_prevalence_2016-colorado$obesity_prevalence_2011
head(colorado)

## keep only 2016 and 5 year difference
colorado <- colorado %>% 
  select(-ends_with("2011"))
head(colorado)

setwd('../cleaned/')
write.csv(colorado, "CDC_Diabetes_Outcome_Data.csv", row.names=F)