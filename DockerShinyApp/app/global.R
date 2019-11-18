library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(tibble)
library(stringr)
library(forcats)
library(ggplot2)
library(plotly)
library(shinyWidgets)
library(yaml)
library(DT)
library(viridis)
library(sf)
library(leaflet)

config <- yaml.load_file("./config.yaml")

source("./r/functions.R")


# will have to read this in from s3 bucket
dat <- read_csv("./data/final_data.csv", col_types = cols(fips = col_character()))
dd <- read_csv("./data/final_data_dictionary.csv") %>%
  mutate(descrip_new = str_wrap(description, 10))
dist_mat <- read_csv("./data/final_distance.csv") %>%
  column_to_rownames(var = "X1")

st_shp <- st_read("./data/county_shp.shp")
