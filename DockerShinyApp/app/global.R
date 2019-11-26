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
library(shinyBS)
library(shinycssloaders, quietly = TRUE, warn.conflicts = FALSE)
library(aws.signature)
library(aws.ec2metadata)
library(aws.s3)

# setting aws profile for credentials
if (!is_ec2() & !is_ecs()) {
  Sys.setenv("AWS_PROFILE"='ahrq')
}

# read in config files
config <- yaml.load_file("./config.yaml")
lang_cfg <- yaml.load_file("./lang_cfg.yaml")

# helper functions
source("./r/functions.R")

# read in data from s3
dat <- aws.s3::s3read_using(read.csv, object = "s3://community-connector/final_data.csv") %>%
  mutate_at(vars(c("fips", "county", "state")), as.character)
dd <- aws.s3::s3read_using(read.csv, object = "s3://community-connector/final_data_dictionary.csv") %>% 
  mutate(descrip_new = str_wrap(description, 10)) %>%
  mutate_at(vars(c("column_name", "description", "data_type", "source", "Notes")), as.character)
dist_mat <- aws.s3::s3read_using(read.csv, object = "s3://community-connector/final_distance.csv") %>% 
  column_to_rownames(var = "X")
names(dist_mat) <- gsub("^X", "", names(dist_mat))

# read in shape files from s3
tmp <- tempdir()
tmp_pth <- . %>% file.path(tmp, .)
t <- tmp_pth("county_shp.shp")
tp <- tmp_pth("county_shp.prj")
td <- tmp_pth("county_shp.dbf")
tx <- tmp_pth("county_shp.shx")

save_object(object = "s3://community-connector/county_shp.shp", file = t)
save_object(object = "s3://community-connector/county_shp.prj", file = tp)
save_object(object = "s3://community-connector/county_shp.dbf", file = td)
save_object(object = "s3://community-connector/county_shp.shx", file = tx)

st_shp <- st_read(t)

rm(t, tp, td, tx)

# data formatting for tables
demo_cols <- dd %>% 
  filter(demographic | used_sdoh_1 | used_sdoh_2 | used_sdoh_3 | used_sdoh_4 | used_sdoh_5 | used_sdoh_6) %>% 
  pull(column_name)

dat <- dat %>% 
  mutate_at(demo_cols, format_dat)
