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


# Legislative 2016
geol_16 = read_xlsx("Georgia/copy_paste/georgia_leg_2016.xlsx")
geol_16_parties = read_xlsx("Georgia/copy_paste/georgia_leg_16_party.xlsx")
geol_16_country = read_xlsx("Georgia/copy_paste/georgia_leg_16_country.xlsx", col_names =  F)
geol_16_coo = read_xlsx("Georgia/copy_paste/georgia_leg_2016_coo.xlsx")

geol_16$precinct =  as.numeric(geol_16$precinct)
georgia_names = as.numeric(names(geol_16)[2:26])

georgia_names = countrycode(georgia_names, "List number", "Party Name", custom_dict = geol_16_parties)
georgia_names = trimws(gsub("\"", "", georgia_names))
geol_16$precinct = countrycode(geol_16$precinct, "...1", "...2", custom_dict = geol_16_country)

geol_16$weird = countryname(geol_16$precinct)
geol_16$weird[geol_16$precinct == "switherland"] = "Switzerland"
geol_16$precinct = countryname(geol_16$weird)
geol_16 = geol_16[-27]

geol_16 = aggregate(c(geol_16[2:26]), by = geol_16[1], sum)
names(geol_16)[2:26] = georgia_names
geol_16 = renamer(geol_16, 1)
geol_16 = add_column(geol_16, valid_votes = rowSums(geol_16[2:26]), .after = 'country')
str(geol_16)
geol_16 = main_function(geol_16, "Paata Burchuladze - for the state people", "Georgian Dream - Democratic Georgia", 3, "Georgian Dream - Democratic Georgia")
geol_16 = extra_cols(geol_16, "Georgia", "2016-10-08", "Legislative")

# country of origin
geol_16_coo = geol_16_coo[-1]
geol_16_coo = lapply(geol_16_coo, function(y) gsub("\n.*", "", y))
geol_16_coo = lapply(geol_16_coo, function(y) as.numeric(y))
geol_16_coo = as.data.frame(geol_16_coo)
geol_16_coo = as.data.frame(colSums(geol_16_coo))
geol_16_coo$party = georgia_names
rownames(geol_16_coo) <- NULL
geol_16_coo = geol_16_coo %>% pivot_wider(names_from = party, values_from = `colSums(geol_16_coo)`)
geol_16_coo = add_column(geol_16_coo, country = "Georgia", .before = 1)
geol_16_coo = add_column(geol_16_coo, valid_votes = rowSums(geol_16_coo[2:26]), .after = "country")
geol_16_coo = main_function(geol_16_coo, "Paata Burchuladze - for the state people", "Georgian Dream - Democratic Georgia", 3, "Georgian Dream - Democratic Georgia")
geol_16_coo = extra_cols(geol_16_coo,  "Georgia", "2016-10-08", "Legislative")


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
geop_13 = read_xlsx("Georgia/copy_paste/georgia_pres_2013.xlsx")
geop_13_parties = read_xlsx("Georgia/copy_paste/georgia_2013_pres_party.xlsx")
geop_13_country = read_xlsx("Georgia/copy_paste/georgia_2013_pres_country.xlsx")
geop_13_coo = read_xlsx("Georgia/copy_paste/georgia_pres_2013_coo.xlsx")


geop_13$district =  as.numeric(geop_13$district)
georgia_names = as.numeric(names(geop_13)[2:24])

georgia_names = countrycode(georgia_names, "List number", "Party Name", custom_dict = geop_13_parties)
georgia_names = trimws(gsub("\"", "", georgia_names))
geop_13$district = countrycode(geop_13$district, "Constitency Number", "Country", custom_dict = geop_13_country)

geop_13$weird = countryname(geop_13$district)
geop_13$district = geop_13$weird

geop_13 = geop_13[-25]

geop_13 = aggregate(c(geop_13[2:24]), by = geop_13[1], sum)
names(geop_13)[2:24] = georgia_names
geop_13 = renamer(geop_13, 1)
geop_13 = add_column(geop_13, valid_votes = rowSums(geop_13[2:24]), .after = 'country')
str(geop_13)
geop_13 = main_function(geop_13, "Tamaz Bibiluri- Independent", "Giorgi Margvelashvili- Georgian Dream", 3, "Giorgi Margvelashvili- Georgian Dream")
geop_13 = extra_cols(geop_13, "Georgia", "2013-10-27", "Presidential")


# Country of origin
#exlude abroad votes
geop_13_coo = geop_13_coo[-74,]
geop_13_coo = geop_13_coo[-1]
geop_13_coo = lapply(geop_13_coo, function(y) gsub("\n.*", "", y))
geop_13_coo = lapply(geop_13_coo, function(y) as.numeric(y))
geop_13_coo = as.data.frame(geop_13_coo)
geop_13_coo = as.data.frame(colSums(geop_13_coo))
geop_13_coo$party = georgia_names
rownames(geop_13_coo) <- NULL
geop_13_coo = geop_13_coo %>% pivot_wider(names_from = party, values_from = `colSums(geop_13_coo)`)
geop_13_coo = add_column(geop_13_coo, country = "Georgia", .before = 1)
geop_13_coo = add_column(geop_13_coo, valid_votes = rowSums(geop_13_coo[2:24]), .after = "country")
geop_13_coo = main_function(geop_13_coo, "Tamaz Bibiluri- Independent", "Giorgi Margvelashvili- Georgian Dream", 3, "Giorgi Margvelashvili- Georgian Dream")
geop_13_coo = extra_cols(geop_13_coo,  "Georgia", "2013-10-27", "Presidential")


## Presidential 
geop_18 = read_xlsx("Georgia/copy_paste/georgia_pres_2018.xlsx")
geop_18_parties = read_xlsx("Georgia/copy_paste/georgia_pres_18_parties.xlsx")
geop_18_country = read_xlsx("Georgia/copy_paste/georgia_pres_18_countries.xlsx")
geop_18_coo = read_xlsx("Georgia/copy_paste/georgia_pres_2018_coo.xlsx")


geop_18$precint =  as.numeric(geop_18$precint)
geop_18 = geop_18[-27]
georgia_names = as.numeric(names(geop_18)[2:26])

georgia_names = countrycode(georgia_names, "List number", "Candidate", custom_dict = geop_18_parties)
georgia_names = trimws(gsub("\"", "", georgia_names))
geop_18$precint = countrycode(geop_18$precint, "Constituency Number", "Country", custom_dict = geop_18_country)
#view(geop_18_country)
geop_18$weird = countryname(geop_18$precint)
geop_18$precint = geop_18$weird

geop_18 = geop_18[-27]

geop_18 = aggregate(c(geop_18[2:26]), by = geop_18[1], sum)
names(geop_18)[2:26] = georgia_names
geop_18 = renamer(geop_18, 1)
geop_18 = add_column(geop_18, valid_votes = rowSums(geop_18[2:26]), .after = 'country')
str(geop_18)
geop_18 = main_function(geop_18, "Mikheil Antadze", "Teimuraz Shashiashvili", 3, "Salome Zurabishvili")
geop_18 = extra_cols(geop_18, "Georgia", "2018-10-28", "Presidential")

## country of origin
geop_18_coo = geop_18_coo[-74,]
geop_18_coo = geop_18_coo[-1]
geop_18_coo = lapply(geop_18_coo, function(y) gsub("\n.*", "", y))
geop_18_coo = lapply(geop_18_coo, function(y) as.numeric(y))
geop_18_coo = as.data.frame(geop_18_coo)
geop_18_coo = as.data.frame(colSums(geop_18_coo))
geop_18_coo$party = georgia_names
rownames(geop_18_coo) <- NULL
geop_18_coo = geop_18_coo %>% pivot_wider(names_from = party, values_from = `colSums(geop_18_coo)`)
geop_18_coo = add_column(geop_18_coo, country = "Georgia", .before = 1)
geop_18_coo = add_column(geop_18_coo, valid_votes = rowSums(geop_18_coo[2:26]), .after = "country")
geop_18_coo = main_function(geop_18_coo, "Mikheil Antadze", "Teimuraz Shashiashvili", 3, "Salome Zurabishvili")
geop_18_coo = extra_cols(geop_18_coo, "Georgia", "2018-10-28", "Presidential")


### Macedonia


## Moldova 

