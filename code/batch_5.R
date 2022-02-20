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


# Legislative 2012

geol_12 = read_xlsx("Georgia/copy_paste/georgia_2012.xlsx")
geol_12_parties = read_xlsx("Georgia/copy_paste/georgia_leg_12_dic.xlsx")
geol_12_co = read_xlsx("C:/Users/lenovo/Documents/BSE/RA/Data/Data/EVP/Georgia/copy_paste/georgia_leg_12_countrydic.xlsx")

str(geol_12)
geol_12 = geol_12[-24,]
geol_12$`recinct N` =  as.numeric(geol_12$`recinct N`)
georgia_names = as.numeric(names(geol_12)[2:17])

georgia_names = countrycode(georgia_names, "List number", "Party Name English", custom_dict = geol_12_parties)
georgia_names = trimws(gsub("\"", "", georgia_names))

geol_12$`recinct N` = countrycode(geol_12$`recinct N`, "Polling Station Number", "Country", custom_dict = geol_12_co)
geol_12 = aggregate(c(geol_12[2:17]), by = geol_12[1], sum)
names(geol_12)[2:17] = georgia_names
geol_12 = renamer(geol_12, 1)
geol_12 = add_column(geol_12, valid_votes = rowSums(geol_12[2:17]), .after = 'country')
geol_12 = main_function(geol_12, "Kakha Kukava - Free Georgia", "Bidzina Ivanishvili- Georgian dream", 3, "Bidzina Ivanishvili- Georgian dream")
geol_12 = extra_cols(geol_12, "Georgia", "2012-10-01", "Legislative")
geol_12$weird = countryname(geol_12$country)
geol_12$weird[geol_12$country == "Chech republic"] = "Czechia"
geol_12$country = countryname(geol_12$weird)
geol_12 = geol_12[-40]

#Coo results 

geol_12_coo = read_xlsx("Georgia/copy_paste/georgia_2012_coo.xlsx")
#drop abroad votes

geol_12_coo = geol_12_coo[-74,]
#clean this mess

geol_12_coo = geol_12_coo[-1]

geol_12_coo = lapply(geol_12_coo, function(y) gsub("\n.*", "", y))
geol_12_coo = lapply(geol_12_coo, function(y) as.numeric(y))
geol_12_coo = as.data.frame(geol_12_coo)
# this is annoying // 
geol_12_coo = as.data.frame(colSums(geol_12_coo))
geol_12_coo$party = georgia_names
rownames(geol_12_coo) <- NULL
geol_12_coo = geol_12_coo %>% pivot_wider(names_from = party, values_from = `colSums(geol_12_coo)`)
geol_12_coo = add_column(geol_12_coo, country = "Georgia", .before = 1)
geol_12_coo = add_column(geol_12_coo, valid_votes = rowSums(geol_12_coo[2:17]), .after = "country")
geol_12_coo = main_function(geol_12_coo, "Kakha Kukava - Free Georgia", "Bidzina Ivanishvili- Georgian dream", 3, "Bidzina Ivanishvili- Georgian dream")
geol_12_coo = extra_cols(geol_12_coo, "Georgia", "2012-10-01", "Legislative")

# Presidential 08
geop_08 = read_xlsx(file, sheet = 2)
geop_08 = renamer(geop_08, 1)
geop_08 = geop_08[-c(1,46),]
geop_08$country = gsub(",.*", "", geop_08$country)
geop_08$country = countryname(geop_08$country)
geop_08 = add_column(geop_08, valid_votes = rowSums(geop_08[11:17]), .after = 'Total number of voters')
georgia_names = names(geop_08)
geop_08 = aggregate(c(geop_08[2:18]), by = geop_08[1], sum)
names(geop_08) = georgia_names

# will drop the information on invalid votes as with ballots dont really add up all the times 
# so it is just more confusing 
geop_08 = geop_08[-c(2:5, 9:11)]
names(geop_08)[c(2,4)] = c('registered_voters', 'total_votes')
str(geop_08)
geop_08 = main_function(geop_08, "Levan Gachechiladze", "Irina Sarishvili Chanturia", 5, "Mikheil Saakashvili")
geop_08 = extra_cols(geop_08, "Georgia", "2008-01-05", "Presidential")

## Presidential 2013


