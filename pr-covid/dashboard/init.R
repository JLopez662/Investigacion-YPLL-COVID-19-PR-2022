# -- Libraries
library(tidyverse)
library(lubridate)
library(gridExtra)
library(scales)
source("functions.R")

## if on the server get the latest data
if(Sys.info()["nodename"] == "fermat.dfci.harvard.edu"){
  rda_path <- "/homes10/rafa/dashboard/pr-covid/dashboard/rdas"
} else{
  rda_path <- "rdas"
}
# -- Set locale
Sys.setlocale("LC_TIME", "es_ES")

# -- Loading population data 
pop <- read_csv("data/poblacion-municipios.csv") %>%
  slice(1) %>% unlist()
pop <- pop[-1]
names(pop)[names(pop)=="Comerio"]<- "Comerío"
poblacion_municipios <- tibble(patientCity = names(pop), poblacion = pop) %>%
  filter(patientCity != "Puerto Rico")

load(file.path(rda_path,"data.rda"))

lag_to_complete <- 3
last_day <- last_complete_day - days(lag_to_complete)