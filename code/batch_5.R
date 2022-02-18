### Loading packages------------------------------------------------------------
library(countrycode)
library(plyr)
library(tidyverse)
library(ggthemes)
library(tibble)
library(tidyr)
library(dplyr)
library(readxl)
library(janitor)
library(gtools)
library(stringi)
library(stringr)
library(xml2)
library(XML)
library(rvest)
library(maps)
library(uchardet)
library(ggmap)
# Setting working directory 
setwd("C:/Users/lenovo/Documents/BSE/RA/Data/Data/EVP")

#### Georiga ------------------------------------------------------------------
# First excel files
file = "Georgia/Copy of Georgia. Results from overseas Parl2008, Parl2004, Pres2008,.xlsx"
# Legislative 04
geol_04 = read_xlsx(file, sheet = 1)
geol_04 = renamer(geol_04, 1)
geol_04$country = gsub(",.*", "", geol_04$country)
geol_04$country = countryname(geol_04$country)
geol_04 = geol_04[-3]
names(geol_04)[2] = 'total_votes'
geol_04 = add_column(geol_04, valid_votes = rowSums(geol_04[3:22]), .after = 'total_votes') 
names(geol_04) = trimws(names(geol_04))
georgia_names = names(geol_04)
geol_04 = aggregate(c(geol_04[2:23]), by = geol_04[1], sum)
names(geol_04) = georgia_names
geol_04 = main_function(geol_04, 'Socialist Party of Georgia', 'Against everyone', 4, "National Movement Democrats")
geol_04 = extra_cols(geol_04, 'Georgia', '2004-03-24', 'Legislative')

# Legislative 08 
geol_08 = read_xlsx(file, sheet = 3)
## duplicated party ?? 
# Presidential 08
geop_08 = read_xlsx(file, sheet = 2)
geop_08 = renamer(geop_08, 1)
geop_08 = geop_08[-c(1,46),]
geop_08$country = gsub(",.*", "", geop_08$country)
geop_08$country = countryname(geop_08$country)


