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


#### Macedonia -----------------------------------------------------------------


#### Moldova -------------------------------------------------------------------


#### Turkey --------------------------------------------------------------------

# Legislative 2015 June

tur_leg15 = read_xlsx("turkey/modified_files/turkey_leg_2015.xlsx")
# I'm going to include contested votes in valid_votes, but this can be changed
names(tur_leg15)[1:7] = c('country', 'registered_voters', 'total_votes', 'drop1','drop2', 
                          'valid_votes', 'invalid_votes')

tur_leg15 = tur_leg15[-c(4,5)]
names(tur_leg15) = iconv(names(tur_leg15), from = 'UTF-8', to = 'ASCII//TRANSLIT')
tur_leg15 = main_function(tur_leg15, "DYP", "BTP", 6, "AK PARTI")
tur_leg15 = extra_cols(tur_leg15, "Turkey", "2015-06-07", "Legislative")
custom_dict$turkish =tolower(countrycode::codelist$cldr.name.tr)
custom_dict$turkish = iconv(custom_dict$turkish, from = 'UTF-8', to = 'ASCII//TRANSLIT')
tur_leg15$country = iconv(tur_leg15$country, from = 'UTF-8', to = 'ASCII//TRANSLIT')
tur_leg15$weird = countrycode(tolower(tur_leg15$country), 'turkish', 'english', custom_dict = custom_dict)
turkish = unlist(str_split("amerika birlesik dev., birlesik arap emir., cek cumhuriyeti, cin halk cumhuriyeti, iran, irlanda, ispanya, israil, isvec, isvicre, italya, kuzey kibris turk cum., makedonya, rusya federasyonu", ", "))
english = c(
  "United States",
  "The United Arab Emirates",
  "czech republic",
  "People's Republic of China",
  "Iran",
  "Ireland",
  "Spain",
  "Israel",
  "Sweden",
  "Switzerland",
  "Italy",
  "north cyprus",
  "Macedonia",
  "russian federation"
)


for (i in seq_along(turkish)){
  tur_leg15$weird[tolower(tur_leg15$country) == turkish[i]] = english[i]
}

tur_leg15$country = countryname(tur_leg15$weird)
tur_leg15 = tur_leg15[-51]

# Legislative 2015 November

tur_leg15_nov = read_xlsx("turkey/modified_files/turkey_leg_2015_november.xlsx")
names(tur_leg15_nov)[1:7] = c('country', 'registered_voters', 'total_votes', 'drop1','drop2', 
                              'valid_votes', 'invalid_votes')

tur_leg15_nov = tur_leg15_nov[-c(4,5)]
names(tur_leg15_nov) = iconv(names(tur_leg15_nov), from = 'UTF-8', to = 'ASCII//TRANSLIT')
tur_leg15_nov = main_function(tur_leg15_nov, "MILLET", "DYP", 6, "AK PARTI")
tur_leg15_nov = extra_cols(tur_leg15_nov, "Turkey", "2015-11-01", "Legislative")
tur_leg15_nov$country = iconv(tur_leg15_nov$country, from = 'UTF-8', to = 'ASCII//TRANSLIT')

tur_leg15_nov$weird = countrycode(tolower(tur_leg15_nov$country), 'turkish', 'english', custom_dict = custom_dict)
turkish = unlist(str_split("amerika birlesik dev., birlesik arap emir., cek cumhuriyeti, cin halk cumhuriyeti, iran, irlanda, ispanya, israil, isvec, isvicre, italya, kuzey kibris turk cum., makedonya, rusya federasyonu", ", "))
# same as above, also same order
for (i in seq_along(turkish)){
  tur_leg15_nov$weird[tolower(tur_leg15_nov$country) == turkish[i]] = english[i]
}

tur_leg15_nov$country = countryname(tur_leg15_nov$weird)
tur_leg15_nov = tur_leg15_nov[-43]

## Legislative 2018

tur_leg18 = read_xlsx("turkey/modified_files/turkey_leg_2018.xlsx")
tur_leg18 = tur_leg18[-c(1,5,6)]
names(tur_leg18)[1:5] = c("country", "registered_voters", "total_votes", "valid_votes", "invalid_votes")
names(tur_leg18) = iconv(names(tur_leg18), from = 'UTF-8', to = 'ASCII//TRANSLIT')
str(tur_leg18)
tur_leg18 = main_function(tur_leg18, "AK PARTI", "MILLET ITTIFAKI", 6, "AK PARTI")
tur_leg18 = extra_cols(tur_leg18, "Turkey", "2018-06-24", "Legislative")
tur_leg18$weird = countryname(tur_leg18$country)
false_name = c("New Zeland", "United Arab Order.")
right_name = c("New Zealand", "United Arab Emirates")
for (i in seq_along(false_name)){
  tur_leg18$weird[tur_leg18$country == false_name[i]] = right_name[i]
}

tur_leg18$country = countryname(tur_leg18$weird)
tur_leg18 = tur_leg18[-31]

## Presidential 2014

tur_pres14 = read_xlsx("turkey/modified_files/turkey_pres_2014.xlsx")
tur_pres14 = tur_pres14[-c(1, 5,6)]

names(tur_pres14)[1:5] = c("country", "registered_voters", "total_votes", "valid_votes", "invalid_votes")
str(tur_pres14)
tur_pres14 = main_function(tur_pres14, "AKP", "CHP / MHP", 6, "AKP")
tur_pres14 = extra_cols(tur_pres14, "Turkey", "2014-08-10", "Presidential")
tur_pres14$weird = countryname(tur_pres14$country)
tur_pres14$weird[tur_pres14$country == "New Zeland"] = "New Zealand"
tur_pres14$country = countryname(tur_pres14$weird)
tur_pres14 = tur_pres14[-17]

## Presidential 2018
tur_pres18 = read_xlsx("turkey/modified_files/turkey_pres_2018.xlsx")
tur_pres18 = tur_pres18[-c(4,5)]
names(tur_pres18)[1:5] = c("country", "registered_voters", "total_votes", "valid_votes", "invalid_votes")
# that every country has a turnout of 100 % seems a bit unrealistic
names(tur_pres18) = iconv(names(tur_pres18), from = 'UTF-8', to = 'ASCII//TRANSLIT')
tur_pres18 = main_function(tur_pres18, "CHP", "VATAN", 6, "AKP")
tur_pres18 = extra_cols(tur_pres18, "Turkey", "2018-06-24", "Presidential")
tur_pres18$country = iconv(tur_pres18$country, from = 'UTF-8', to = 'ASCII//TRANSLIT')
tur_pres18$weird = countrycode(tolower(tur_pres18$country), 'turkish', 'english', custom_dict = custom_dict)
turkish = unlist(str_split("amerika birlesik dev., birlesik arap emir., cek cumhuriyeti, cin halk cumhuriyeti, iran, irlanda, ispanya, israil, isvec, isvicre, italya, kuzey kibris turk cum., makedonya, rusya federasyonu", ", "))
# again same as above 

for (i in seq_along(turkish)){
  tur_pres18$weird[tolower(tur_pres18$country) == turkish[i]] = english[i]
}

tur_pres18$country = countryname(tur_pres18$weird)
tur_pres18 = tur_pres18[-23]


## Serbia

#Legislative

file = "Serbia/Parliament 2012-2020 ENG.XLS"

# 2012

ser_leg12 = read_xls(file, sheet = 4) 
ser_leg12 = row_to_names(ser_leg12, 1)
ser_leg12 = ser_leg12[grepl('', names(ser_leg12))]
ser_leg12 = ser_leg12[-1,]
serbia_names = names(ser_leg12)
ser_leg12[2:27] = lapply(ser_leg12[2:27], function(y) as.numeric(y))
ser_leg12 = aggregate(c(ser_leg12[2:27]), by = ser_leg12[1], sum)
serbia_names = trimws(iconv(serbia_names, from = 'UTF-8', to = 'ASCII//TRANSLIT'))
names(ser_leg12) = serbia_names
ser_leg12 = ser_leg12[-c(2,5:7)]
names(ser_leg12)[1:5] = c('country', 'registered_voters', 'total_votes', 'invalid_votes',
                          'valid_votes')
ser_leg12$country = countryname(ser_leg12$country)
str(ser_leg12)
# this might be a bit weird -> wikipedia indicates a different party name for the winner (which is not in the list)
ser_leg12 = main_function(ser_leg12, "Choice for a Better Life - Boris Tadic", "None of The Above", 6, "Let's Get Serbia Moving - Tomislav Nikolic")
ser_leg12 = extra_cols(ser_leg12, "Serbia", "2012-05-06", "Legislative")


## 2014 legislative
ser_leg14 = read_xls(file, sheet = 3)
ser_leg14 = row_to_names(ser_leg14, 1)
ser_leg14 = ser_leg14[grepl('', names(ser_leg14))]
ser_leg14 = ser_leg14[-1,]
ser_leg14 = ser_leg14[-c(2, 5:7)]
serbia_names = names(ser_leg14)
ser_leg14[2:24] = lapply(ser_leg14[2:24], function(y) as.numeric(y))
ser_leg14 = aggregate(c(ser_leg14[2:24]), by = ser_leg14[1], sum)
serbia_names = trimws(iconv(serbia_names, from = 'UTF-8', to = 'ASCII//TRANSLIT'))
names(ser_leg14) = serbia_names
names(ser_leg14)[1:5] = c('country', 'registered_voters', 'total_votes', 'invalid_votes',
                          'valid_votes')

ser_leg14$country = countryname(ser_leg14$country)
ser_leg14 = main_function(ser_leg14, "ALEKSANDAR VUCIC\n- SNS, SDPS, NS, SPO, SP", "PARTY OF DEMOCRATIC ACTION -\nRIZA HALIMI", 6, "ALEKSANDAR VUCIC\n- SNS, SDPS, NS, SPO, SP")
ser_leg14 = extra_cols(ser_leg14, "Serbia", "2014-03-16", "Legislative")

# 2016 legislative
ser_leg16 = read_xls(file, sheet = 2) 
ser_leg16 = row_to_names(ser_leg16, 1)

ser_leg16 = ser_leg16[-c(2, 4,5,7)]
names(ser_leg16)[1:5] = c('country', 'registered_voters', 'total_votes', 'invalid_votes',
                          'valid_votes')

serbia_names = names(ser_leg16)
ser_leg16[2:25] = lapply(ser_leg16[2:25], function(y) as.numeric(y))
ser_leg16 = aggregate(c(ser_leg16[2:25]), by = ser_leg16[1], sum)
serbia_names = trimws(iconv(serbia_names, from = 'UTF-8', to = 'ASCII//TRANSLIT'))
names(ser_leg16) = serbia_names

ser_leg16$country = countryname(ser_leg16$country)
ser_leg16 = main_function(ser_leg16,  "ALEKSANDAR VUCIC - SERBIA IS WINNING", "IN DEFIANCE - UNITED FOR SERBIA - PEOPLE'S ALLIANCE", 6, 
                          "ALEKSANDAR VUCIC - SERBIA IS WINNING")

ser_leg16 = extra_cols(ser_leg16, "Serbia", "2016-04-24", "Legislative")


## Presidential

file = "C:/Users/lenovo/Documents/BSE/RA/Data/Data/EVP/Serbia/Presidental 2012-2017 ENG.XLS"

ser_pres12 = read_xls(file, sheet = 2)

ser_pres12 = row_to_names(ser_pres12, 1)
ser_pres12 = ser_pres12[grepl('', names(ser_pres12))]
ser_pres12 = ser_pres12[-1,]
ser_pres12 = ser_pres12[-c(2,5:7)]
names(ser_pres12)[1:5] = c('country', 'registered_voters', 'total_votes', 'invalid_votes',
                          'valid_votes')
serbia_names = names(ser_pres12)
ser_pres12[2:17] = lapply(ser_pres12[2:17], function(y) as.numeric(y))
ser_pres12 = aggregate(c(ser_pres12[2:17]), by = ser_pres12[1], sum)
serbia_names = trimws(iconv(serbia_names, from = 'UTF-8', to = 'ASCII//TRANSLIT'))
names(ser_pres12) = serbia_names
ser_pres12$country = countryname(ser_pres12$country)
ser_pres12 = main_function(ser_pres12, "Prof. Dr. Zoran Stankovic", "Tomislav Nikolic", 6, "Boris Tadic")
# note: Boris Tadic won first round (very close) but lost second round //
ser_pres12 = extra_cols(ser_pres12, "Serbia", "2012-05-06", "Presidential")

# Presidential 2017

ser_pres17 = read_xls(file, sheet = 1)
ser_pres17 = row_to_names(ser_pres17, 1)

ser_pres17 = ser_pres17[-c(2,4,5,7)]
names(ser_pres17)[1:5] = c('country', 'registered_voters', 'total_votes', 'invalid_votes',
                           'valid_votes')
serbia_names = names(ser_pres17)
ser_pres17[2:16] = lapply(ser_pres17[2:16], function(y) as.numeric(y))
ser_pres17 = aggregate(c(ser_pres17[2:16]), by = ser_pres17[1], sum)
serbia_names = trimws(iconv(serbia_names, from = 'UTF-8', to = 'ASCII//TRANSLIT'))
names(ser_pres17) = serbia_names
ser_pres17$country = countryname(ser_pres17$country)
ser_pres17 = main_function(ser_pres17, "Sasa Jankovic", "Nenad Canak", 6, "Aleksandar Vucic")
ser_pres17 = extra_cols(ser_pres17, "Serbia", "2017-04-02", "Presidential")



























