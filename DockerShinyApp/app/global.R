library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(tibble)
library(stringr)
library(forcats)
library(shinyWidgets)
library(yaml)

config <- yaml.load_file("./config.yaml")

# will have to read this in from s3 bucket
dat <- read_csv("./data/sample_data.csv")
