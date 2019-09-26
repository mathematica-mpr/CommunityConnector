library(tidyverse)
library(foreign)

# Set working directory to unzipped folder containing census csv files
setwd("c:/APPAM/map_shiny/Input/colorado_census_tract_rent_perc_income")
temp = list.files(pattern = "*ann.csv")
myfiles = lapply(temp, read_csv)


clean_census <- function(data){
  
  
  for (i in 1:length(temp))
    data[[i]] <- data[[i]] %>%
      slice(2:n()) %>%
      # HD* is the default name for columns in ACS data
      mutate_at(vars(contains("HD")),as.numeric) %>%
      # Create total rent burdened by adding together number of individuals paying over 30% of rent on income
      mutate(rent_burdened = HD01_VD07 + HD01_VD08+ HD01_VD09 + HD01_VD10,
             # Divide by 'total' column
             HD01_VD01 = rent_burdened/as.numeric(HD01_VD01)) %>% 
      # Remove all other columns
      dplyr::select(-c("HD02_VD01", "HD02_VD02", "HD02_VD03", "HD02_VD04", "HD02_VD05", "HD02_VD06", "HD02_VD07", "HD02_VD08", 
                       "HD02_VD09", "HD02_VD10", "HD02_VD11", "HD01_VD02", "HD01_VD03", "HD01_VD04", "HD01_VD05", "HD01_VD06", 
                       "HD01_VD11", "HD01_VD07", "HD01_VD08", "HD01_VD09", "HD01_VD10", "rent_burdened")) %>%
      # Rename '% rent burdened variable'' by year of file
      dplyr::rename(!!paste('', i + 2008, sep = "") := "HD01_VD01")
  
  return(data)
}


# Apply function
myfiles <- clean_census(myfiles)
# join together elements of list
cost_burdened_tracts <- myfiles %>% reduce(right_join, by = c("GEO.id", "GEO.id2", "GEO.display-label"))

cost_burdened_tracts <- cost_burdened_tracts %>%
  dplyr::select(-c("GEO.id")) %>%
  rename(FIPS = "GEO.id2") %>%
  # Put in tidy format
  gather(`2009`:`2017`, key = "year", value = "rent_perc") %>%
  mutate(rent_perc = 100*round(rent_perc,digits=3))

