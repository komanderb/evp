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
library(splitstackshape)
# Setting working directory 
setwd("C:/Users/lenovo/Documents/BSE/RA/Data/Data/EVP")

#### Georgia ------------------------------------------------------------------
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
names(geol_08)[12:13] = c("United Opposition Party (National Council, Right)", "National Party of Radical Democrats of Georgia")
geol_08 = renamer(geol_08, 1)
geol_08$country = gsub(",.*", "", geol_08$country)
geol_08$country = gsub("\\..*", "", geol_08$country)
geol_08$country = countryname(geol_08$country)
# I will drop these as they don't make any sense
geol_08 = geol_08[-c(2,4)]
names(geol_08)[2:3] = c("total_votes", "null_votes")
geo_names = names(geol_08)
geo_names = gsub("\"", "", geo_names)
geol_08 = aggregate(c(geol_08[2:15]), by = geol_08[1], sum)
names(geol_08) = geo_names
geol_08 = add_column(geol_08, valid_votes = rowSums(geol_08[4:15]), .after = "total_votes")
geol_08 = main_function(geol_08, "Citizens' Political Union Georgian Politics", "Georgian Political Party Our Country", 5, "United National Movement - for Victorious Georgia")
geol_08 = extra_cols(geol_08, "Georgia", "2008-05-21", "Legislative")

## the whole valid/null/total votes does not make any sense

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

mac_1 = read.csv("macedonia_2016_1.csv", encoding = "UTF-8")
mac_2 = read.csv("macedonia_2016_2.csv", encoding = "UTF-8")

mac_polstat = unique(mac_1$pol_stat)
mac_polstat_2 = read.delim("macedonia_pol_stat.txt", sep = ",", header = F)
mac_polstat = mac_polstat[-1]
mac_polstat_2$original = mac_polstat

mac_party = unique(mac_1$Party)f
for (i in mac_party){
  print(i)
}

mac_party_en = c(
  "VMRO-DPMNE",
  "DUI",
  "SDSM",
  "VMRO for Macedonia"
)

for (i in seq_along(mac_party)){
  mac_1$Party[mac_1$Party == mac_party[i]] = mac_party_en[i]
}

mac_1 = mac_1[-c(1:4),]
mac_1$pol_stat = countrycode(mac_1$pol_stat, "original", "V2", custom_dict = mac_polstat_2)
mac_1 = mac_1[-c(1,3,4,6,8)]
mac_1$pol_stat = trimws(mac_1$pol_stat)
mac_1 = mac_1 %>% pivot_wider(names_from = Party, values_from = Votes, values_fn = sum)

mac_2 = mac_2[-c(1:7),-1]
mac_2$pol_stat = countrycode(mac_2$pol_stat, "original", "V2", custom_dict = mac_polstat_2)
mac_2$pol_stat = trimws(mac_2$pol_stat)
mac_2 = mac_2 %>% filter(!(X0 %in% c("Number/Percent of polling stations that have sent data (without ZRD)","Percent out of voters who voted in polling stations that have sent data")))
mac_2[3] = lapply(mac_2[3], function(y) as.numeric(y))
mac_2 =  mac_2 %>% pivot_wider(names_from = X0, values_from = X1, values_fn = sum)
mac_2 = mac_2[-3]
names(mac_2) = c("country", "registered_voters", "total_votes", "valid_votes", "invalid_votes")
mac_1 = renamer(mac_1, 1)
mac_16 = left_join(mac_2, mac_1)
mac_16 = main_function(mac_16, "VMRO-DPMNE", "VMRO for Macedonia", 6,  "VMRO-DPMNE")
mac_16 = extra_cols(mac_16, "North Macedonia", "2016-12-11", "Legislative")

#### Moldova -------------------------------------------------------------------
## Legislative July 2009
mol_leg09 = read.csv("moldova_leg_09_july.csv", encoding = "UTF-8")
mol_leg09 = mol_leg09[-c(1:29),]
mol_leg09$X1 = iconv(mol_leg09$X1, from = "UTF-8", to = 'ASCII//TRANSLIT')
mol_names = mol_leg09$X1
expressions = c(".* in ", ".* la ", ".* al ", ".* a ", ".* la ", "\n")
for (i in seq_along(expressions)){
  mol_names = gsub(expressions[i], "", mol_names)
}

for (i in mol_names)
  print(i)

english = c(
  "Republic of Austria",
  "Republic of Azerbaijan",
  "Republic of Belarus",
  "Kingdom of Belgium",
  "Republic of Bulgaria",
  "Czech Republic",
  "People's Republic of China",
  "Hellenic Republic",
  "Swiss Confederation",
  "Republic of Estonia",
  "French Republic",
  "French Republic",
  "Federal Republic of Germany",
  "Federal Republic of Germany",
  "State of Israel",
  "Italy",
  "Italy",
  "Republic of Latvia",
  "Republic of Lithuania",
  "Great Britain and Northern Ireland",
  "Republic of Poland",
  "Portugal",
  "Romania",
  "Romania",
  "Russian Federation",
  "United States of America",
  "United States of America",
  "Kingdom of Sweden",
  "Republic of Turkey",
  "Republic of Turkey",
  "Ukraine",
  "Ukraine",
  "Republic of Hungary"
)


english = countryname(english)
mol_leg09$english = english
mol_dic = mol_leg09[c(3,22)]
mol_leg09$X1 = mol_leg09$english
mol_leg09 = mol_leg09[-22]
mol_leg09 = mol_leg09[-1]
names(mol_leg09) = c('section_nr_1', 'country', 'Number of voters included in the electoral lists', 
                     'Number of voters included in the lists more', 'The number of voters received ballots', 
                     'The number of voters who participated in the voting', 'Difference of ballots received and voters who participated in the voting', 
                     'invalid_ballots', 'section_nr', 'valid_votes', 'Number of newslettersvote received', 'Number of ballots unused and canceled', 'Partidul Comunistilor din Republica Moldova', 
                     'Partidul Popular Crestin Democrat', 'Alianta MOLDOVA NOASTRA', 'Partidul Liberal', 'Partidul Liberal Democrat din Moldova', 'Partidul Democrat din Moldova', 'Partidul Social Democrat', 
                     'Partidul Ecologist Alianta Verde din Moldova')

# this might be a long shot
mol_leg09 = add_column(mol_leg09, registered_voters = rowSums(mol_leg09[3:4]), .after = "country")
mol_leg09 = mol_leg09[-c(1,4:6, 8, 10, 12,13)]
names(mol_leg09)[3:4] = c("total_votes", "invalid_votes")
mol_names = names(mol_leg09)
mol_leg09 = aggregate(c(mol_leg09[2:13]), by = mol_leg09[1], sum)
names(mol_leg09) = mol_names
mol_leg09 = main_function(mol_leg09, "Partidul Comunistilor din Republica Moldova", "Partidul Ecologist Alianta Verde din Moldova", 6, "Partidul Comunistilor din Republica Moldova")
mol_leg09 = extra_cols(mol_leg09, "Moldova", "2009-07-29", "Legislative")


## Legislative April 2009
mol_leg09april = read_xlsx("Moldova/moldova2009leg1.xlsx")
mol_leg09april = add_column(mol_leg09april, country = english, .after = "Sectia de votare") 
# passt
#names are very messy
mol_leg09april = mol_leg09april[-c(1,2,14)]
# and get rid of those 
mol_leg09april = mol_leg09april[-c(4,8)]
mol_leg09april[2:23] = lapply(mol_leg09april[2:23], function(y) as.numeric(y))
mol_leg09april = add_column(mol_leg09april, registered_voters = rowSums(mol_leg09april[2:3]), .after = "country")
mol_leg09april = mol_leg09april[-c(3,4)]
names(mol_leg09april)[3:5] = c("total_votes", "invalid_votes", "valid_votes")
mol_names = names(mol_leg09april)
mol_leg09april = aggregate(c(mol_leg09april[2:22]), by = mol_leg09april[1], sum)
names(mol_leg09april) = mol_names
mol_leg09april = main_function(mol_leg09april, "Partidul Social Democrat", "Lomakin Alexandr, candidat independent", 6, "Partidul Comunictilor din Republica Moldova")
mol_leg09april = extra_cols(mol_leg09april, "Moldova", "2009-04-05", "Legislative")


## Legislative 2010
mol_leg10 = read.csv("Moldova/moldova2010leg.xlsx - Sheet1.csv", encoding = "UTF-8", header = F)
mol_leg10 = row_to_names(mol_leg10, 1)
mol_leg10 = mol_leg10[-1,]
mol_leg10$Localitatea = iconv(mol_leg10$Localitatea, from = 'UTF-8', to = 'ASCII//TRANSLIT')
names(mol_leg10) = iconv(names(mol_leg10), from = 'UTF-8', to = 'ASCII//TRANSLIT')
expressions = c(".* in ", ".* la ", ".* al ", ".* a ", ".* la ", "\n", " or.*" )
for (i in seq_along(expressions)){
  mol_leg10$Localitatea = gsub(expressions[i], "", mol_leg10$Localitatea)
} 
for (i in mol_leg10$Localitatea)
  print(i)

english = c(
  "Republic of Austria",
  "Republic of Azerbaijan",
  "Republic of Belarus",
  "Kingdom of Belgium",
  "Republic of Bulgaria",
  "Czech Republic",
  "People's Republic of China",
  "Cyprus",
  "Hellenic Republic",
  "Hellenic Republic",
  "Swiss Confederation",
  "Republic of Estonia",
  "French Republic",
  "French Republic",
  "French Republic",
  "Federal Republic of Germany",
  "Federal Republic of Germany",
  "State of Israel",
  "Italy",
  "Italy",
  "Italy",
  "Italy",
  "Italy",
  "Italy",
  "Italy",
  "Italy",
  "Italy",
  "Italy",
  "Italy",
  "Italy",
  "Italy",
  "Italy",
  "Italy",
  "Italy",
  "Ireland",
  "Republic of Latvia",
  "Republic of Lithuania",
  "Great Britain and Northern Ireland",
  "Republic of Poland",
   "Portugal",
  "Republic of Puerto Rico",
  "Portugal",
  "Portugal",
  "Romania",
  "Romania",
  "Romania",
  "Romania",
  "Romania",
  "Romania",
  "Romania",
  "Romania",
  "Russian Federation",
  "Russian Federation",
  "Russian Federation",
  "Russian Federation",
  "Kingdom of Spain",
  "Kingdom of Spain",
  "Kingdom of Spain",
  "United States of America",
  "USA",
  "USA",
  "USA",
  "USA",
  "USA",
  "USA",
  "USA",
  "Canada",
  "Canada",
  "Kingdom of Sweden",
  "Republic of Turkey",
  "Turkey",
  "Ukraine",
  "Ukraine",
  "Ukraine",
  "Republic of Hungary"
)
english = countryname(english)
mol_leg10 = add_column(mol_leg10, country = english, .after = "Localitatea")
mol_leg10[4:51] = lapply(mol_leg10[4:51], function(y) as.numeric(y))
mol_leg10 = add_column(mol_leg10, registered_voters = rowSums(mol_leg10[4:5]), .after = "country")
mol_leg10 = mol_leg10[-c(1,2,5:7,9, 50:52)]
names(mol_leg10)[3:4] = c("total_votes", "invalid_votes")
mol_leg10 = add_column(mol_leg10, valid_votes = rowSums(mol_leg10[5:43]), .after = "total_votes")

names(mol_leg10) = gsub("\"", "", names(mol_leg10))
mol_names = names(mol_leg10)
mol_leg10 = aggregate(c(mol_leg10[2:44]), by = mol_leg10[1], sum)
names(mol_leg10) = mol_names
mol_leg10 = main_function(mol_leg10, "Partidul National Liberal", "Sergiu Banari, candidat independent", 6, "Partidul Comuni?tilor din Republica Moldova")
mol_leg10 = extra_cols(mol_leg10, "Moldova", "2010-11-28", "Legislative")


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


#### Serbia--------------------------------------------------------------------

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

## Presidential 2008

ser_pres08 = read_xlsx("Serbia/ser_pres08_abroad.xlsx")
# ignore polling station level data (it is aggregated already)
ser_pres08 = ser_pres08 %>% filter(is.na(Number))
identical(ser_pres08$`The number of voters who went to the polls`, ser_pres08$`Number of voters who voted`)
# both describe total votes so can drop one
ser_pres08 = ser_pres08[-c(2,3,5,6,8)]
names(ser_pres08)[1:3] = c("country", "registered_voters", "total_votes")
ser_pres08 = add_column(ser_pres08, valid_votes = rowSums(ser_pres08[4:12]), .after = "total_votes")
names(ser_pres08) = iconv(names(ser_pres08), from = 'UTF-8', to = 'ASCII//TRANSLIT')
ser_pres08 = main_function(ser_pres08, "Tomislav Nikolic", "Milanka Karic", 5, "Tomislav Nikolic")
ser_pres08 = extra_cols(ser_pres08, "Serbia", "2008-01-20", "Presidential")
ser_pres08$country =  countryname(ser_pres08$country)

ser_pres08_coo = read_xlsx("Serbia/ser_pres08_national.xlsx")
ser_pres08_coo = ser_pres08_coo[1,]
ser_pres08_coo = ser_pres08_coo[-c(2,3,5,6,8)]
names(ser_pres08_coo)[1:3] = c("country", "registered_voters", "total_votes")
ser_pres08_coo = add_column(ser_pres08_coo, valid_votes = rowSums(ser_pres08_coo[4:10]), .after = "total_votes")
names(ser_pres08_coo) = iconv(names(ser_pres08_coo), from = 'UTF-8', to = 'ASCII//TRANSLIT')
#this is a dead end
##### Ukraine ------------------------------------------------------------------

ukr_leg12 = read.csv("ukraine_leg_12.csv", encoding = "UTF-8")
ukr_leg12 = ukr_leg12 %>% filter(!(X4 == 'Number of votes "FOR" party'))
ukr_leg12 = ukr_leg12[-c(1,3,5,6)]
ukr_leg12[3] = lapply(ukr_leg12[3], function(y) as.numeric(y))
ukr_leg12$X4[is.na(ukr_leg12$X4)] = 0
ukr_leg12 = ukr_leg12 %>% pivot_wider(names_from = X1, values_from = X4)
ukr_leg12$weird = countryname(ukr_leg12$country)
wrong_names = unlist(str_split("Italian Republic, Lebanese Republic, Portuguese Republic", ", "))
right_names = c('Italy', 'Lebanon', 'Portugal')
for (i in seq_along(wrong_names)){
  ukr_leg12$weird[ukr_leg12$country == wrong_names[i]] = right_names[i]
}
ukr_leg12$country = countryname(ukr_leg12$weird)
ukr_leg12 = ukr_leg12[-c(23)]
ukr_leg12 = add_column(ukr_leg12, valid_votes = rowSums(ukr_leg12[2:22]), .after = 'country')
ukr_leg12 = main_function(ukr_leg12, 'PARTY OF REGIONS', 'politychna partiia Ukrainska Natsionalna Asambleia', 3, "PARTY OF REGIONS")
ukr_leg12 = extra_cols(ukr_leg12, "Ukraine", "2012-10-28", "Legislative")


### Legislative 2014
ukr_leg14 = read.csv("ukraine_leg_14.csv", encoding = "UTF-8")
ukr_leg14 = ukr_leg14 %>% filter(!(X4 == 'Number of votes "FOR" party'))
ukr_leg14 = ukr_leg14[-c(1,3,5,6)]
ukr_leg14[3] = lapply(ukr_leg14[3], function(y) as.numeric(gsub(" ", "", y)))
ukr_leg14$X4[is.na(ukr_leg14$X4)] = 0
ukr_leg14 = ukr_leg14 %>% pivot_wider(names_from = X1, values_from = X4)
unique(ukr_leg14$country)
# now this is a bit funny as some kyrillic names remained
kyrillic = c("Азербайджан",               
             "Беларусь",                                
             "Болгария",                               
             "Грузия",
             "Киргизская Республика",
             "Латвийская Республика",                 
             "Республика Армения",
             "Республика Молдова",                 
             "Республика Узбекистан",                
             "Республіка Казахстан",
             "Российская Федерация" ,
             "Туркменистан")
english = c("Azerbaijan",
            "Belarus",
            "Bulgaria",
            "Georgia",
            "Kyrgyz Republic",
            "Latvian republic" ,
            "Republic of Armenia",
            "The Republic of Moldova",
            "The Republic of Uzbekistan",
            "Republic of Kazakhstan",
            "The Russian Federation" ,
            "Turkmenistan")

for (i in seq_along(kyrillic)){
  ukr_leg14$country[ukr_leg14$country == kyrillic[i]] = english[i]
}
ukr_leg14$weird = countryname(ukr_leg14$country)
wrong_names = unlist(str_split("Italian Republic, Portuguese Republic, Republic of Makedonia, Republika Srbija", ", "))
right_names = c("Italy", "Portugal", "North Macedonia", "Serbia")
for (i in seq_along(wrong_names)){
  ukr_leg14$weird[ukr_leg14$country == wrong_names[i]] = right_names[i]
}
ukr_leg14$country = countryname(ukr_leg14$weird)
ukr_leg14 = ukr_leg14[-31]
ukr_leg14 = add_column(ukr_leg14, valid_votes = rowSums(ukr_leg14[2:30]), .after = "country")
ukr_leg14 = main_function(ukr_leg14, "Political party SAMOPOMICH Union", 'Politychna partiia "NOVA POLITYKA"', 3, 'Political party "NARODNYY FRONT"')
ukr_leg14 = extra_cols(ukr_leg14, "Ukraine", "2014-10-26", "Legislative")

### Legislative 2019

ukr_leg19 = read.csv("ukraine_leg_2019.csv", encoding = "UTF-8")
ukr_leg19 = ukr_leg19[-c(1,3,5,6)]
names(ukr_leg19)[2:3] = c("party", "votes")

kyrilic = unique(ukr_leg19$party)
for(i in kyrilic)
  print(i)

english = c("EUROPEAN SOLIDARITY POLITICAL PARTY",
            "POLITICAL PARTYSERVANT OF THE PEOPLE",
            "VOICE Political Party",
            "POLITICAL PARTY SHARIYA PARTY",
            "political party All-Ukrainian Union Freedom",
            "Mikheil Saakashvili's New Forces Movement Political Party",
            "POLITICAL PARTY POWER AND HONOR",
            "OPPOSITION BLOC Political Party",
            "POLITICAL PARTY SOCIAL JUSTICE",
            "POLITICAL PARTY PARTY OF THE GREEN OF UKRAINE",
            "People's Power Political Party",
            "Political Party PATRIOT",
            "Agrarian Party of Ukraine",
            "POLITICAL PARTY POWER OF LAW",
            "POLITICAL PARTY UKRAINIAN GROISMAN'S STRATEGY",
            "INDEPENDENCE POLITICAL PARTY",
            "Batkivshchyna All-Ukrainian Union Political Party",
            "POLITICAL PARTY ALL-UKRAINIAN ASSOCIATION TORCH",
            "POLITICAL PARTY RADICAL PARTY OF OLEH LYASHKO",
            "Civic Position political party",
            "Political Party OPPOSITION PLATFORM - FOR LIFE",
            "Political Party Association SELF-HELP")

for (i in seq_along(kyrilic)){
  ukr_leg19$party[ukr_leg19$party == kyrilic[i]] = english[i]
}

ukr_leg19[3] = lapply(ukr_leg19[3], function(y) as.numeric(gsub(" ", "", y)))
ukr_leg19$votes[is.na(ukr_leg19$votes)] = 0
ukr_leg19 = ukr_leg19 %>% pivot_wider(names_from = party, values_from = votes)
ukr_leg19 = add_column(ukr_leg19, valid_votes = rowSums(ukr_leg19[2:23]), .after = "country")
ukr_leg19$weird = countryname(ukr_leg19$country)
wrong_names = unlist(str_split("Italian Republic, Mexican United States, Portuguese Republic", ", "))
right_names = c("Italy", "Mexico", "Portugal")
for (i in seq_along(wrong_names)){
  ukr_leg19$weird[ukr_leg19$country == wrong_names[i]] = right_names[i]
}

ukr_leg19$country = countryname(ukr_leg19$weird)
ukr_leg19 = ukr_leg19[-25]
ukr_leg19 = main_function(ukr_leg19, "EUROPEAN SOLIDARITY POLITICAL PARTY", "Political Party Association SELF-HELP", 3, "POLITICAL PARTYSERVANT OF THE PEOPLE")
ukr_leg19 = extra_cols(ukr_leg19, "Ukraine", "2019-07-21", "Legislative")

#Legislative 2007
ukr_leg07 = read_xlsx("Ukraine/ukraine_leg_2007.xlsx")
ukr_leg07_pol = read_xlsx("Ukraine/ukraine_leg_2007_pol_stat.xlsx")
ukr_leg07_pol$polling_station = gsub("\\..*", "", ukr_leg07_pol$polling_station)
ukr_leg07_pol$polling_station[ukr_leg07_pol$Country == "Republic of Belarus"] = "8, 9"
ukr_leg07_pol$polling_station[ukr_leg07_pol$Country == "Republic of Bulgaria"] = "10, 11"
ukr_leg07_pol = cSplit(ukr_leg07_pol, "polling_station", sep=",", "long")
ukr_leg07$country = countrycode(ukr_leg07$country, "polling_station", "Country", custom_dict = ukr_leg07_pol)
names(ukr_leg07) = gsub("\"", "", names(ukr_leg07))
ukr_names = names(ukr_leg07)
ukr_leg07 = aggregate(c(ukr_leg07[2:24]), by = ukr_leg07[1], sum)

names(ukr_leg07) = ukr_names
ukr_leg07 = add_column(ukr_leg07, valid_votes = rowSums(ukr_leg07[5:24]), .after = "total_votes")
ukr_leg07 = main_function(ukr_leg07, "Communist Party of Ukraine (updated)", "Electoral bloc of political parties KUCHMA (Constitution - Ukraine - Honor - Peace - Anti-fascism)", 6, "Party of Regions")
ukr_leg07 = extra_cols(ukr_leg07, "Ukraine", "2007-09-30", "Legislative")
ukr_leg07$weird = countryname(ukr_leg07$country)
#wrong names again
for (i in seq_along(wrong_names)){
  ukr_leg07$weird[ukr_leg07$country == wrong_names[i]] = right_names[i]
}
ukr_leg07$country = countryname(ukr_leg07$weird)
ukr_leg07 = ukr_leg07[-51]

## Legislative 2006
ukr_leg06 = read_xlsx("Ukraine/ukraine_leg_2006.xlsx")
ukr_leg06_pol = read_xlsx("Ukraine/ukraine_leg_06_pol.xlsx")
ukr_leg06_pol$`Precinct numbers` = gsub("\\..*", "", ukr_leg06_pol$`Precinct numbers`)
ukr_leg06_pol$`Precinct numbers`[ukr_leg06_pol$Country == "Republic of Belarus"] = "8, 9"
ukr_leg06_pol$`Precinct numbers`[ukr_leg06_pol$Country == "Republic of Bulgaria"] = "10, 11"
ukr_leg06_pol = cSplit(ukr_leg06_pol, "Precinct numbers", sep=",", "long")

ukr_leg06$country = countrycode(ukr_leg06$country, "Precinct numbers", "Country", custom_dict = ukr_leg06_pol)
names(ukr_leg06) = gsub("\"", "", names(ukr_leg06))
ukr_names = names(ukr_leg06)
ukr_leg06 = aggregate(c(ukr_leg06[2:49]), by = ukr_leg06[1], sum)
names(ukr_leg06) = ukr_names
ukr_leg06 = add_column(ukr_leg06, valid_votes = rowSums(ukr_leg06[5:49]), .after = "total_votes")
ukr_leg06 = main_function(ukr_leg06, "All-Ukrainian Party of People's Trust", "Labor Ukraine Political Party", 6, "Party of Regions")
ukr_leg06 = extra_cols(ukr_leg06, "Ukraine", "2006-03-26", "Legislative")
ukr_leg06$weird = countryname(ukr_leg06$country)
for (i in seq_along(wrong_names)){
  ukr_leg06$weird[ukr_leg06$country == wrong_names[i]] = right_names[i]
}
ukr_leg06$country = countryname(ukr_leg06$weird)
ukr_leg06 = ukr_leg06[-101]

# leg 2002

ukr_leg02 = read.csv("ukraine_leg_2002.csv", encoding = "UTF-8")
ukr_leg02 = row_to_names(ukr_leg02, 1)

a = 1
vector = c(1)
while (a < 3060){
  a = a +34
  vector = append(vector, a)
  
}
vector = vector[-91]
ukr_polstat = ukr_leg02$`Ranking by site`[vector]
ukr_polstat = gsub(" Polling Station.*", "", ukr_polstat)
#ukr_polstat = gsub(" Precint.*", "", ukr_polstat)
ukr_polstat = gsub(" Polling station.*", "", ukr_polstat)
ukr_polstat = gsub(" Precinct.*", "", ukr_polstat)
ukr_polstat = gsub(" Constituency.*", "", ukr_polstat)
list_names = c()
for (i in ukr_polstat){
  list_names = append(list_names, rep(i, times = 34))
} 

ukr_leg02 = add_column(ukr_leg02, country = list_names, .before = "0")
ukr_leg02 = ukr_leg02[-vector,-c(2,3,6)]
names(ukr_leg02)[2:3] = c("party", "votes")
ukr_leg02$votes = gsub("th", "", ukr_leg02$votes)
ukr_leg02[3] = lapply(ukr_leg02[3], function(y) as.numeric(gsub(" ", "", y)))
ukr_leg02$weird = countryname(ukr_leg02$country)
names_02 = unlist(str_split("Italian Republic, Lebanese Republic, Mexican United States, Portuguese Republic", ", "))
names_02_right = c("Italy", "Lebanon", "Mexico","Portugal")
for (i in seq_along(names_02)){
  ukr_leg02$weird[ukr_leg02$country == names_02[i]] = names_02_right[i]
}

ukr_leg02$country = countryname(ukr_leg02$weird)
ukr_leg02 = ukr_leg02[-4]
ukr_leg02 = ukr_leg02 %>% pivot_wider(names_from = party, values_from = votes,  values_fn = sum)
names(ukr_leg02) = gsub("\"", "", names(ukr_leg02)) 
ukr_leg02 = add_column(ukr_leg02, valid_votes = rowSums(ukr_leg02[2:34]), .after = "country")
names(ukr_leg02) = trimws(names(ukr_leg02))
ukr_leg02 = main_function(ukr_leg02, "Electoral bloc of political parties Victor Yushchenko's bloc Our Ukraine", "Electoral bloc of political parties Russian bloc", 3, "Electoral bloc of political parties Victor Yushchenko's bloc Our Ukraine")
ukr_leg02 = extra_cols(ukr_leg02, "Ukraine", "2002-03-31", "Legislative")


### Presidential:

#2010

ukr_pres10 = read_xlsx("Ukraine/ukraine2010pres.xlsx")
ukr_pres10_polst = read.csv("ukraine_pres_2010_polstat.csv", encoding = 'UTF-8')
ukr_pres10_polst = row_to_names(ukr_pres10_polst, 1)
ukr_pres10_polst = cSplit(ukr_pres10_polst, "Precinct  numbers", sep=",", "long")
ukr_pres10_polst = ukr_pres10_polst[,-c(1,3)]
ukr_pres10$Precincts = countrycode(ukr_pres10$Precincts, "Precinct  numbers", "Country", custom_dict = ukr_pres10_polst)
ukr_pres10[2:23] = lapply(ukr_pres10[2:23], function(y) as.numeric(y))
ukr_pres10 = aggregate(c(ukr_pres10[2:23]), by = ukr_pres10[1], sum)
names(ukr_pres10)[c(1:4, 23)] = c("country", "registered_voters", "total_votes", "invalid_votes", "not_supported")
ukr_pres10 = add_column(ukr_pres10, valid_votes = rowSums(ukr_pres10[5:23]), .after = "total_votes")
ukr_pres10$weird = countryname(ukr_pres10$country)
wrong_names = unlist(str_split("Italian Republic, Mexican United States, Portuguese Republic", ", "))
right_names = c("Italy", "Mexico", "Portugal")
for (i in seq_along(wrong_names)){
  ukr_pres10$weird[ukr_pres10$country == wrong_names[i]] = right_names[i]
}
ukr_pres10$country = countryname(ukr_pres10$weird)
ukr_pres10 = ukr_pres10[-25]
ukr_pres10 = main_function(ukr_pres10, "Theological", "not_supported", 6, "Yanukovych")
ukr_pres10 = extra_cols(ukr_pres10, "Ukraine", "2010-01-17", "Presidential")

#2014
ukr_pres14 = read_xlsx("Ukraine/ukraine2014pres.xlsx")
ukr_pres14_polst = read.csv("ukraine_pres_2014_polstat.csv", encoding = 'UTF-8')
ukr_pres14_polst = row_to_names(ukr_pres14_polst, 1)
ukr_pres14_polst = cSplit(ukr_pres14_polst, "Precinct  numbers", sep=",", "long")
ukr_pres14_polst = ukr_pres14_polst[,-c(1,3)]
ukr_pres14$`N VD` = countrycode(ukr_pres14$`N VD`, "Precinct  numbers", "Foreign state", custom_dict = ukr_pres14_polst)
ukr_pres14 = ukr_pres14[-c(2,4:9)]
names(ukr_pres14)[1:4] = c("country", "registered_voters", "total_votes", "invalid_votes")
ukr_pres14[2:25] = lapply(ukr_pres14[2:25], function(y) as.numeric(y))
ukr_pres14 = aggregate(c(ukr_pres14[2:25]), by = ukr_pres14[1], sum)
ukr_pres14 = add_column(ukr_pres14, valid_votes = rowSums(ukr_pres14[5:25]), .after = "total_votes")
ukr_pres14 = main_function(ukr_pres14, "Hermit", "X.pom", 6, "Poroshenko")#
ukr_pres14 = extra_cols(ukr_pres14, "Ukraine", "2014-05-25", "Presidential")
ukr_pres14$weird = countryname(ukr_pres14$country)
# wrong names from above still the same
for (i in seq_along(wrong_names)){
  ukr_pres14$weird[ukr_pres14$country == wrong_names[i]] = right_names[i]
}

ukr_pres14$country = countryname(ukr_pres14$weird)
ukr_pres14 = ukr_pres14[-53]
## 2019

ukr_pres19 = read_xlsx("Ukraine/ukraine_pres_2019.xlsx")
ukr_pres19_polst = read.csv("ukraine_pres_2019_polstat.csv", encoding = 'UTF-8')
ukr_pres19_polst = ukr_pres19_polst[-c(1,3)]
ukr_pres19_polst = cSplit(ukr_pres19_polst, "Precinct.numbers", sep=",", "long")
ukr_pres19$country = countrycode(ukr_pres19$country, "Precinct.numbers", "Foreign.state", custom_dict = ukr_pres19_polst)
names(ukr_pres19)[17] = "Vasyl Zhuravlyov"
str(ukr_pres19)
ukr_names = names(ukr_pres19)
ukr_names = gsub("\n", " ", ukr_names)
ukr_pres19 = aggregate(c(ukr_pres19[2:43]), by = ukr_pres19[1], sum)
names(ukr_pres19) = ukr_names
ukr_pres19 = add_column(ukr_pres19, valid_votes = rowSums(ukr_pres19[5:43]), .after = "total_votes")
ukr_pres19 = main_function(ukr_pres19, "Balashov Gennady", "Shevchenko Alexander", 6, "Zelensky Vladimir")
ukr_pres19 = extra_cols(ukr_pres19, "Ukraine", "2019-03-31", "Presidential")
ukr_pres19$weird = countryname(ukr_pres19$country)
for (i in seq_along(wrong_names)){
  ukr_pres19$weird[ukr_pres19$country == wrong_names[i]] = right_names[i]
}
ukr_pres19$country = countryname(ukr_pres19$weird)
ukr_pres19 = ukr_pres19[-89]


#### Timor-Leste---------------------------------------------------------------
# Legislative 2017
tim_leg17 = read_xlsx("timor_leste/timorleste2017leg_mod.xlsx")
tim_leg17[2:18] = lapply(tim_leg17[2:18], function(y) as.numeric(gsub("\\.", "", y)))
tim_leg17 = add_column(tim_leg17, "Timor-Leste" = rowSums(tim_leg17[2:14]), .after = "party")
tim_leg17 = tim_leg17[-c(3:15)]
tim_names = tim_leg17$party
tim_leg17 = data.frame(t(tim_leg17[,-1]))
names(tim_leg17) = tim_names
tim_leg17 = rownames_to_column(tim_leg17)
tim_leg17 = renamer(tim_leg17, 1)
tim_leg17 = add_column(tim_leg17, valid_votes = rowSums(tim_leg17[2:22]), .after = "country")
tim_leg17 = main_function(tim_leg17, "BUP", "FRETILIN", 3, "FRETILIN")
tim_leg17 = extra_cols(tim_leg17, "Timor-Leste", "2017-07-22", "Legislative")
tim_leg17_coo = tim_leg17[4,]
tim_leg17 = tim_leg17[-4,]

#Legislative 2018
tim_leg18 = read_xlsx("timor_leste/timorleste2018leg_mod.xlsx")
tim_leg18[2:11] = lapply(tim_leg18[2:11], function(y) as.numeric(gsub("\\.", "", y)))
tim_leg18 = add_column(tim_leg18, "Timor-Leste" = rowSums(tim_leg18[2:7]), .after = "party")
tim_leg18 = tim_leg18[-c(3:8)]
tim_names = tim_leg18$party
tim_leg18 = data.frame(t(tim_leg18[,-1]))
names(tim_leg18) = tim_names
tim_leg18 = rownames_to_column(tim_leg18)
tim_leg18 = renamer(tim_leg18, 1)
tim_leg18 = add_column(tim_leg18, valid_votes = rowSums(tim_leg18[2:9]), .after = "country")
tim_leg18 = main_function(tim_leg18, "PEP", "AMP", 3, "AMP")
tim_leg18 = extra_cols(tim_leg18, "Timor-Leste", "2018-05-12", "Legislative")
tim_leg18_coo = tim_leg18[4,]
tim_leg18 = tim_leg18[-4,]

#### Saotome ------------------------------------------------------------------
file = "saotome/saotome20062011pres.xlsx"
# Pres 06
saot_pres06 = read_xlsx(file, sheet = 1)
saot_pres06[2:8] = lapply(saot_pres06[2:8], function(y) as.numeric(gsub("\\,", "", y)))
saot_pres06$District[1:7] = "São Tomé & Príncipe"
sao_names = names(saot_pres06)
saot_pres06 = aggregate(c(saot_pres06[2:8]), by = saot_pres06[1], sum)
names(saot_pres06) = sao_names
names(saot_pres06)[c(1,5:8)] = c("country", "valid_votes", "invalid_votes", "total_votes", "registered_voters")
saot_pres06 = main_function(saot_pres06, "Nilo Guimaraes", "Patrice Trovoada (ADI)", 6, "Fradique de Menezes (MDFM-PL) [MDFM-PCD]")
saot_pres06 = extra_cols(saot_pres06, "São Tomé & Príncipe", "2006-07-30", "Presidential")
saot_pres06_coo = saot_pres06[5,]
saot_pres06 = saot_pres06[-5,]

#Pres 11
saot_pres11 = read_xlsx(file, sheet = 2)
saot_pres11[2:12] = lapply(saot_pres11[2:12], function(y) as.numeric(gsub("\\,", "", y)))
saot_pres11 = add_column(saot_pres11, "São Tomé & Príncipe" = rowSums(saot_pres11[2:8]), .after = "Candidate (Party)")
saot_pres11 = saot_pres11[-c(3:9)]
saot_names = saot_pres11$`Candidate (Party)`
saot_pres11 = data.frame(t(saot_pres11[,-1]))
names(saot_pres11) = saot_names
saot_pres11 = rownames_to_column(saot_pres11)
saot_pres11 = renamer(saot_pres11, 1)

names(saot_pres11)[12:15] = c("valid_votes", "invalid_votes", "total_votes", "registered_voters")
saot_pres11 = main_function(saot_pres11, "Manuel Pinto da Costa", "Manuel de Deus Lima", 6, "Manuel Pinto da Costa")
saot_pres11 = extra_cols(saot_pres11, "São Tomé & Príncipe", "2011-07-17", "Presidential")
saot_pres11_coo = saot_pres11[5,]
saot_pres11 = saot_pres11[-5,]


#### Senegal -------------------------------------------------------------------
sen_pres00 = read_xlsx("Senegal/senegal2000pres.xlsx")
sen_pres00 = sen_pres00[-c(13:52),]
sen_pres00[2:16] = lapply(sen_pres00[2:16], function(y) as.numeric(gsub("\\,", "", y))) 
sen_names = sen_pres00$`Candidate (Party) [Coalition]`
sen_pres00 = data.frame(t(sen_pres00[,-1]))
names(sen_pres00) = sen_names
sen_pres00 = rownames_to_column(sen_pres00)
sen_pres00 = renamer(sen_pres00, 1)
names(sen_pres00)[10:13] = c("valid_votes", "invalid_votes", "total_votes", "registered_voters")
sen_pres00$country = countryname(sen_pres00$country)
sen_pres00 = main_function(sen_pres00, "Abdou Diouf (PS)", "Mademba Sock (RTA-S)", 6, "Abdou Diouf (PS)")
sen_pres00 = extra_cols(sen_pres00, "Senegal", "2000-02-27", "Presidential")

#### Venezuela -----------------------------------------------------------------

# Pres 2013

ven_13_1 = read.csv("ven_13_1.csv", encoding = "UTF-8")
ven_13_2 = read.csv("ven_13_2.csv", encoding = "UTF-8")

ven_13_1 = ven_13_1 %>% filter(!(is.na(Votos)))
ven_13_1$Candidato = gsub("NICOLAS MADURO  Adjudicado", "NICOLAS MADURO",ven_13_1$Candidato)
ven_13_1 = ven_13_1[-c(1,3,6)]
ven_13_2 = ven_13_2 %>% filter(!(X0 %in% c("Ficha Técnica", "PARTICIPACIÓN RELATIVA", "ACTAS TOTALES", "ACTAS ESCRUTADAS")))
ven_13_2 = ven_13_2[-c(1,4)]
ven_13_2 = ven_13_2 %>% pivot_wider(names_from = X0, values_from = X2)
ven_13_1 = ven_13_1 %>% pivot_wider(names_from = Candidato, values_from = Votos)
ven_13 = left_join(ven_13_2, ven_13_1)
ven_13[2:7] = lapply(ven_13[2:7], function(y) as.numeric(y))
# assume they both describe registered_voters
identical(ven_13$`ELECTORES ESPERADOS`, ven_13$`ELECTORES EN ACTAS TRANSMITIDAS`)
# also electores escrutados??
ven_13 = ven_13[-c(3,4)]
names(ven_13)[2:5] = c("registered_voters", "total_votes", "valid_votes", "null_votes")
ven_13$country = iconv(ven_13$country, from = 'UTF-8', to = 'ASCII//TRANSLIT')
ven_13$weird = countrycode(tolower(ven_13$country), "spanish2", "english", custom_dict = custom_dict)
spanish = unlist(str_split("arabia saudita, checolovaquia, corea, emiratos arabes unid, gran bretana, grenada, guayana, palestina, qatar, san kitts y nevis, san vicente y las gr, suriname, usa", ", "))
english = c( "saudi arabia",
             "Czechoslovakia",
             "Korea",
             "united arab emirates",
             "great britain",
             "Grenada",
            "Guiana",
            "Palestine",
            "Qatar",
            "Saint Kitts and Nevis",
            "St. Vincent & Grenadines",
            "Suriname",
            "US")

for (i in seq_along(spanish)){
  ven_13$weird[tolower(ven_13$country) == spanish[i]] = english[i]
}

ven_13$country = countryname(ven_13$weird)
ven_13 = ven_13[-12]
ven_13 = main_function(ven_13, "HENRIQUE CAPRILES RADONSKI", "JULIO MORA", 6, "NICOLAS MADURO")
ven_13 = extra_cols(ven_13, "Venezuela", "2013-04-14", "Presidential")

## Venezuela 2006
ven_1 = read.csv("ven_06_1.csv", encoding = "UTF-8")
ven_2 = read.csv("ven_06_2.csv", encoding = "UTF-8")
ven_1 = ven_1 %>%
  separate(X1, c("party", "votes"), "% ")

ven_1$votes = gsub(" votos Ver detalle", "", ven_1$votes)
ven_1[4] = lapply(ven_1[4], function(y) as.numeric(gsub("\\.", "", y)))
ven_1$party = trimws(gsub('[0-9]+', '', ven_1$party))
ven_1$party = gsub("\\,", "", ven_1$party)
ven_1$party = trimws(ven_1$party)
ven_1 = ven_1[-1]
# looks good

ven_2 = ven_2 %>% filter(!(X0 == "Ficha Técnica"))
ven_2 = ven_2 %>%
  separate(X0, c("type", "votes"), ": ")

ven_2$votes = gsub(" \\(.*", "", ven_2$votes)
ven_2[4] = lapply(ven_2[4], function(y) as.numeric(gsub("\\.", "", y)))
ven_2 = ven_2[-1]
ven_2 = ven_2 %>% pivot_wider(names_from = type, values_from = votes)
ven_2 = ven_2[-c(7,8)]
identical(ven_2$`Total Votantes Escrutados`, ven_2$`Total Votos Escrutados`)
# as this is true we can summarize the following as registered_voters
ven_2 = add_column(ven_2, registered_voters = rowSums(ven_2[2:3]), .after = "country")
ven_2 = ven_2[-c(3,4)]
names(ven_2)[3:5] = c("total_votes", "valid_votes", "null_votes")
ven_1 = ven_1 %>% pivot_wider(names_from = party, values_from = votes)
ven_06 = left_join(ven_2, ven_1)
ven_06$country = iconv(ven_06$country, from = 'UTF-8', to = 'ASCII//TRANSLIT')
ven_06$weird = countrycode(tolower(ven_06$country), "spanish2", "english", custom_dict = custom_dict)
spanish = unlist(str_split("arabia saudita, checolovaquia, corea, gran bretana, grenada, guayana, palestina, qatar, san kitts y nevis, san vicente y las gr, suriname, usa", ", "))
english = c( "saudi arabia",
             "Czechoslovakia",
             "Korea",
             "great britain",
             "Grenada",
             "Guiana",
             "Palestine",
             "Qatar",
             "Saint Kitts and Nevis",
             "St. Vincent & Grenadines",
             "Suriname",
             "US")
for (i in seq_along(spanish)){
  ven_06$weird[tolower(ven_06$country) == spanish[i]] = english[i]
}
ven_06$country = countryname(ven_06$weird)
ven_06 = ven_06[-20]
ven_06 = main_function(ven_06, "MANUEL ROSALES", "YUDITH SALAZAR", 6, "HUGO CHAVEZ")
ven_06 = extra_cols(ven_06, "Venezuela", "2006-12-03", "Presidential")
#### Mauretania ----------------------------------------------------------------

mau_pres_19 = read_xlsx("Mauretania/ResultatElection2019.xlsx")
mau_pres_19 =  mau_pres_19 %>% filter(libWilaya == "Etranger")
mau_pres_19 = mau_pres_19[-c(1:7, 19)]
mau_pres_19 = mau_pres_19 %>% pivot_wider(names_from = Candidat, values_from = nbVoix)
mau_pres_19 = mau_pres_19[-c(2:4)]
# Vote neutre? -> blanc votes
names(mau_pres_19)[1:6] = c("country", "registered_voters", "total_votes", 
                            "null_votes", "blanco_votes", "valid_votes")

mau_names = names(mau_pres_19)
mau_pres_19 = aggregate(c(mau_pres_19[2:12]), by = mau_pres_19[1], sum)
names(mau_pres_19) = mau_names
mau_pres_19$weird = countryname(mau_pres_19$country)
french = unlist(str_split("Arabie Saoudite, Emirats Arabes Unis", ", "))
english = c("Saudi Arabia", "United Arab Emirates")
for (i in seq_along(french)){
  mau_pres_19$weird[mau_pres_19$country == french[i]] = english[i]
}
mau_pres_19$country = countryname(mau_pres_19$weird)
mau_pres_19 = mau_pres_19[-13]
str(mau_pres_19)
mau_pres_19 = main_function(mau_pres_19, "Mohamed Cheïkh Mohamed Ahmed ElGHAZOUANI (Ghazouani)", 
                            "KANE Hamidou Baba (Kane)", 7, "Mohamed Cheïkh Mohamed Ahmed ElGHAZOUANI (Ghazouani)")
mau_pres_19 = extra_cols(mau_pres_19, "Mauritania", "2019-06-22", "Presidential")

# ah I forgot COO
mau_pres_19_coo = read_xlsx("Mauretania/ResultatElection2019.xlsx")
mau_pres_19_coo =  mau_pres_19_coo %>% filter(!(libWilaya == "Etranger"))
mau_pres_19_coo = mau_pres_19_coo[-c(1:7, 19)]
mau_pres_19_coo = mau_pres_19_coo %>% pivot_wider(names_from = Candidat, values_from = nbVoix)
mau_pres_19_coo = mau_pres_19_coo[-c(2:4)]

# Vote neutre? -> blanc votes
names(mau_pres_19_coo)[1:6] = c("country", "registered_voters", "total_votes", 
                            "null_votes", "blanco_votes", "valid_votes")

mau_pres_19_coo = mau_pres_19_coo[-1]
mau_pres_19_coo = as.data.frame(colSums(mau_pres_19_coo))
mau_pres_19_coo = rownames_to_column(mau_pres_19_coo)
mau_pres_19_coo = mau_pres_19_coo %>% pivot_wider(names_from = rowname, values_from = `colSums(mau_pres_19_coo)`)
mau_pres_19_coo = add_column(mau_pres_19_coo, country = "Mauritania", .before = 1)
mau_pres_19_coo = main_function(mau_pres_19_coo, "Mohamed Cheïkh Mohamed Ahmed ElGHAZOUANI (Ghazouani)", 
                            "KANE Hamidou Baba (Kane)", 7, "Mohamed Cheïkh Mohamed Ahmed ElGHAZOUANI (Ghazouani)")
mau_pres_19_coo = extra_cols(mau_pres_19_coo, "Mauritania", "2019-06-22", "Presidential")


#### Poland --------------------------------------------------------------------
pol_05_leg = read_xls("Poland/Poland Leg. 2005. All Results.xls", col_types = "text")
pol_05_leg = pol_05_leg %>% filter(Powiat == "Zagranica")

pol_05_leg$`Adres obwodu` = iconv(gsub("\\,.*", "", pol_05_leg$`Adres obwodu`),from = 'UTF-8', to = 'ASCII//TRANSLIT') 
pol_05_leg = pol_05_leg[-c(1:5)]
pol_05_leg[2:28] = lapply(pol_05_leg[2:28], function(y) as.numeric(y))
pol_05_leg = pol_05_leg[-c(25:28)]
pol_names = names(pol_05_leg)

pol_05_leg$weird = countrycode(tolower(pol_05_leg$`Adres obwodu`), "polish", "english", custom_dict = custom_dict)
polish = unlist(str_split("azerbajdzan, izrael tel aviv, korea, macedonia, rpa, serbia i czarnogora, stany zjednoczone chicago ii, tajpej, wielka brytania i irlandia, zimbabwe harare", ", "))

for (i in polish)
  print(i)

english = c(
  "azerbaijan",
  "Israel",
  "korea",
  "macedonia",
  "South Africa",
  "Serbia and Montenegro",
  "US",
  "taipei",
  "UK",
  "zimbabwe harare"
)

for (i in seq_along(polish)){
  pol_05_leg$weird[tolower(pol_05_leg$`Adres obwodu`) == polish[i]] = english[i]
}
pol_05_leg$`Adres obwodu` =  countryname(pol_05_leg$weird)
pol_05_leg = pol_05_leg[-25]
pol_05_leg = aggregate(c(pol_05_leg[2:24]), by = pol_05_leg[1], sum)

names(pol_05_leg) = iconv(pol_names, from = 'UTF-8', to = 'ASCII//TRANSLIT')
pol_05_leg = pol_05_leg[-3]
names(pol_05_leg)[1:5] = c("country", "registered_voters", "total_votes", "invalid_votes", "valid_votes")
pol_05_leg = main_function(pol_05_leg, "1 - Ruch Patriotyczny", "18 - Narodowe Odrodzenie Polski", 6, "6 - Prawo i Sprawiedliwosc")
pol_05_leg = extra_cols(pol_05_leg, "Poland", "2005-09-25", "Legislative")

## also forgot COO again
pol_leg05_coo = read_xls("Poland/Poland Leg. 2005. All Results.xls", col_types = "text")
pol_leg05_coo = pol_leg05_coo %>% filter(!(Powiat == "Zagranica"))
pol_leg05_coo = pol_leg05_coo[-c(1:6)]
pol_leg05_coo[1:27] = lapply(pol_leg05_coo[1:27], function(y) as.numeric(y))
pol_leg05_coo = pol_leg05_coo[-2]
names(pol_leg05_coo)[1:4] = c("registered_voters", "total_votes", 
                              "invalid_votes", "valid_votes")
pol_leg05_coo[is.na(pol_leg05_coo)] = 0
names(pol_leg05_coo) = iconv(names(pol_leg05_coo), from = 'UTF-8', to = 'ASCII//TRANSLIT')
#pol_names = names(pol_leg05_coo)
pol_leg05_coo = as.data.frame(colSums(pol_leg05_coo))


pol_leg05_coo = rownames_to_column(pol_leg05_coo)
pol_leg05_coo = pol_leg05_coo %>% pivot_wider(names_from = rowname, values_from = `colSums(pol_leg05_coo)`)
pol_leg05_coo = add_column(pol_leg05_coo, country = "Poland", .before = 1)
# this will have more parties than abroad //
pol_leg05_coo = main_function(pol_leg05_coo, "1 - Ruch Patriotyczny", "19 - Mniejszosc Niemiecka Slaska", 6, "6 - Prawo i Sprawiedliwosc")
pol_leg05_coo = extra_cols(pol_leg05_coo, "Poland", "2005-09-25", "Legislative")

#### Colombia 2010 -------------------------------------------------------------
setwd("C:/Users/lenovo/Documents/BSE/RA/Data/Data/EVP/Colombia/leg_10")

my_files <- list.files(pattern = "\\.xls$")
df_list = lapply(my_files, read_xls)
names(df_list) <- gsub("\\.xls$", "", my_files)

for(i in seq_along(df_list))
  df_list[[i]]$country = names(df_list)[i]

colleg_10 = do.call(rbind, df_list)
row.names(colleg_10) = NULL
colleg_10 = colleg_10[-c(1, 4:7)]
colleg_10 = colleg_10 %>% pivot_wider(names_from = NOMBRE, values_from = VOTACION, values_fill = 0)
colleg_10$country = countryname(colleg_10$country)
colleg_10$country[is.na(colleg_10$country)] = "Curaçao"
names(colleg_10)[13:16] = c("blanco_votes", "valid_votes", "null_votes", "unmarked_votes")
## unmarked votes again
colleg_10 = main_function(colleg_10, "PARTIDO LIBERAL COLOMBIANO", "MOVIMIENTO INDEPENDIENTE DE RENOVACION ABSOLUTA MIRA", 6, "PARTIDO SOCIAL DE UNIDAD NACIONAL PARTIDO DE LA U")
colleg_10 = extra_cols(colleg_10, "Colombia", "2010-03-14", "Legislative")

##### Dataframe for Batch ------------------------------------------------------

ukr_leg06 = add_column(ukr_leg06, blanco_votes = NA, .after = 'null_votes')
ukr_leg06 = add_column(ukr_leg06, invalid_votes = NA, .after = 'blanco_votes')


batch_5 = bind_rows(ukr_leg06, geol_04, geol_08, geol_12, geol_16, geop_08, geop_13, 
                    geop_18, mac_16, mol_leg09, mol_leg09april, mol_leg10,
                    tur_leg15, tur_leg15_nov, tur_leg18, tur_pres14, tur_pres18,
                    ser_leg12, ser_leg14, ser_leg16, ser_pres12, ser_pres17, ser_pres08, 
                    ukr_leg12, ukr_leg14, ukr_leg19, ukr_leg07, 
                    ukr_leg02, ukr_pres10, ukr_pres14, ukr_pres19, tim_leg17, 
                    tim_leg18, saot_pres06, saot_pres11, sen_pres00, ven_13,
                    pol_05_leg, mau_pres_19, ven_06, colleg_10
                    )


names(batch_5)[1] <- 'country_of_residence' 

batch_5 = add_column(batch_5, cor_iso3 = countrycode(batch_5$country_of_residence, 'country.name', 'iso3c'), .after = 'country_of_residence')
batch_5 = add_column(batch_5, coo_iso3 = countrycode(batch_5$country_of_origin , 'country.name', 'iso3c'), .after = 'country_of_origin')
number_list = c(13)
start = 13
while (start < 101){
  start = start +2
  number_list = append(number_list, start)
}
number_list
batch_5 = add_column(batch_5, valid_votes2 = rowSums(batch_5[number_list], na.rm = T), .after = "valid_votes")
batch_5 = batch_5[-106]
write.csv(batch_5, 'batch_5.csv', row.names = F)

## country of origing
geol_16_coo = add_column(geol_16_coo, registered_voters = NA, .before = 'valid_votes')
geol_16_coo = add_column(geol_16_coo, total_votes = NA, .before = 'valid_votes')
geol_16_coo = add_column(geol_16_coo, invalid_votes = NA, .after = 'valid_votes')
df_coo = bind_rows(geol_16_coo,geol_12_coo, geop_13_coo, geop_18_coo, tim_leg17_coo, 
                   tim_leg18_coo, saot_pres06_coo, saot_pres11_coo, pol_leg05_coo, mau_pres_19_coo)

write.csv(df_coo, 'batch_5_coo.csv', row.names = F)