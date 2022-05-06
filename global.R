library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(glue)
library(leaflet)
library(scales)
library(DT)

umr <- read.csv("data_input/umr_indo.csv")
wage_data <- read.csv("data_input/umr_indo.csv")
location_data <- read.csv("data_input/latlong.csv")

umr <- umr %>% 
  select(c("Region","Year","Salary")) %>% 
  filter(Region != "INDONESIA") 



