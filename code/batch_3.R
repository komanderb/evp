# Batch 2 / 
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
library(uchardet)
library(googleLanguageR)
# Setting working directory 
setwd("C:/Users/lenovo/Documents/BSE/RA/Data/Data/EVP")

### Latvia --------------------------------------------------------------------
lat_02 = read.csv('lat_02.csv', encoding = 'UTF-8')
lat_02_names = read.csv('lat_02_party.csv', encoding = 'UTF-8')
lat_names = lat_02[,3]
lat_names = "Argentina, United States, United States of America, Australia, 
Australia, Austria, Belarus, Belarus, Belgium, Brazil, Czech Republic, Denmark, France, Greece, Estonia, Italy, 
Israel, Canada, Canada, Russian Federation, Russian Federation, Russian Federation, China, 
United Kingdom, Lithuania, Norway, Netherlands, Poland, Portuguese Republic, Finland, Spain, Ukraine, Germany, Venezuela, Sweden"
lat_names = unlist(strsplit(lat_names, ", "))
lat_02 = lat_02[-c(37,38), -c(1,2,4)]
lat_02 = row_to_names(lat_02, 1)
names(lat_02)[c(1,22)] = c('country', 'valid_votes')
lat_02$country = lat_names
lat_02$country = countryname(lat_02$country)
lat_02$country[is.na(lat_02$country)] = 'Portugal'
lat_02[22] = lapply(lat_02[22], function(y) as.numeric(y))
lat_02 = aggregate(c(lat_02[2:22]), by = lat_02[1], sum)

lat_02_names$X1 =  gsub("\"", "",lat_02_names$X1)
lat_02_names$X1 = iconv(lat_02_names$X1, from = 'UTF-8', to = 'ASCII//TRANSLIT')
lat_names = lat_02_names[,3]
names(lat_02)[2:21] = lat_names
lat_02 = main_function(lat_02, 'Apvieniba Tevzemei un Brivibai/LNNK', 'Zalo un Zemnieku savieniba', 3, 'Jaunais laiks')
lat_02 = extra_cols(lat_02, 'Latvia', '2002-10-05', 'Legislative')

# lat 06
lat_06 = read.csv('lat_06.csv', encoding = 'UTF-8')
lat_06_names = read.csv('lat_06_party.csv', encoding = 'UTF-8')
lat_06 = lat_06[-55, -c(1,2,4)]
lat_06 = row_to_names(lat_06, 1)
lat_06 = renamer(lat_06, 1)
lat_06$country =  gsub(".*\\(", "", lat_06$country)
lat_06$country =  gsub("\\)", "", lat_06$country)
lat_06[21] = lapply(lat_06[21], function(y) as.numeric(y))
lat_06 = aggregate(c(lat_06[2:21]), by = lat_06[1], sum)
names(lat_06)[21] = 'valid_votes'
lat_06_names$party_name = gsub("\"", "",lat_06_names$party_name)
lat_06_names$party_name = iconv(lat_06_names$party_name, from = 'UTF-8', to = 'ASCII//TRANSLIT')
lat_names = lat_06_names[,3]
names(lat_06)[2:20] = lat_names
lat_06 = main_function(lat_06, 'MARAS ZEME', 'Apvieniba Tevzemei un Brivibai/LNNK', 3, 'Tautas partija')
lat_06 = extra_cols(lat_06, 'Latvia', '2006-10-07', 'Legislative')
#names here please 
custom_dict$latvian <- tolower(countrycode::codelist$cldr.name.lv)
custom_dict$latvian <- iconv(custom_dict$latvian, from = 'UTF-8', to = 'ASCII//TRANSLIT')
lat_06$country = iconv(lat_06$country, from = 'UTF-8', to = 'ASCII//TRANSLIT')

lat_06$weird_1 = gsub("s republika", "", tolower(lat_06$country))
lat_06$weird_1 = gsub("s karaliste", "", lat_06$weird_1)
lat_06$weird = countrycode(lat_06$weird_1, 'latvian', 'english', custom_dict = custom_dict)
latvian = "amerikas savienotajas valstis, australijas savieniba, brazilijas federativaja republika, izraelas valsti, kinas tauta, krievijas federacija, lielbritanijas un ziemelirijas apvienotaja karaliste, sveices konfederacija, vacijas federativaja republika, venecuelas bolivara republika"
latvian = unlist(strsplit(latvian, ", "))
english = c( "United States of America",
            "Australia",
             "Brazil",
            "Israel",
           "China",
            "Russia",
           "UK",
            "Switzerland",
            "Germany",
           "Bolivarian Republic of Venezuela")



for (i in 1:10){
  lat_06$weird[tolower(lat_06$weird_1) == latvian[i]] = english[i]
}

lat_06$weird = countryname(lat_06$weird)
lat_dic = lat_06[c(1,46)]
lat_06$country = lat_06$weird
lat_06 = lat_06[-c(46,47)]

## problematic as I used test and thus countrynames don't match anymore 
# lat 10: 

lat_10 = read.csv('lat_leg_10.csv', encoding = 'UTF-8')
lat_10 = lat_10[-1]
lat_10$polling_station = gsub("\\(.*", "", lat_10$polling_station)
lat_10$polling_station = trimws(lat_10$polling_station)
lat_10 = renamer(lat_10, 1)
names(lat_10) =  iconv(names(lat_10), from = 'UTF-8', to = 'ASCII//TRANSLIT')
lat_10 = aggregate(c(lat_10[2:14]), by = lat_10[1], sum)
lat_10 = add_column(lat_10, valid_votes = rowSums(lat_10[2:14]), .after = 'country')
names(lat_10) <- gsub("[.]*$|[.]*(?=[.])", "",names(lat_10), perl = TRUE)
names(lat_10) <- gsub("\\.", " ", names(lat_10))
lat_10 = main_function(lat_10, 'PCTVL', 'Kristigi demokratiska savieniba', 3, 'VIENOTIBA')
lat_10 = extra_cols(lat_10, 'Latvia', '2010-10-02', 'Legislative')
lat_10$country = iconv(lat_10$country, from = 'UTF-8', to = 'ASCII//TRANSLIT')
lat_10$weird = countrycode(lat_10$country, 'country', 'weird', custom_dict = lat_dic)
latvian = "AUSTRALIJAS VALSTU SAVIENIBA, CILE, EGIPTES ARABU REPUBLIKA, GRUZIJA, IZRAELAS VALSTS, JAUNZELANDE, LUKSEMBURGAS LIELHERCOGISTE, PORTUGALE, UNGARIJAS REPUBLIKA, UZBEKISTANAS REPUBLIKA"
latvian = unlist(strsplit(latvian, ", "))
english = c("AUSTRALIA",
            "CHILE",
            "EGYPT",
            "GEORGIA",
            "ISRAEL",
            "NEW ZEALAND",
            "LUXEMBOURG",
            "PORTUGAL",
           "HUNGARY",
            "UZBEKISTAN")


for (i in 1:10){
  lat_10$weird[lat_10$country == latvian[i]] = english[i]
}

lat_10$weird = countryname(lat_10$weird)
lat_dic = lat_10[c(1,34)]
lat_10$country = lat_10$weird
lat_10 = lat_10[-34]
# 2011
lat_11 = read.csv('lat_leg_2011.csv', encoding = 'UTF-8')
lat_11 = lat_11[-1]
lat_11 = renamer(lat_11, 1)
lat_11$country = gsub("\\(.*", "", lat_11$country)
lat_11$country = trimws(lat_11$country)
names(lat_11) =  iconv(names(lat_11), from = 'UTF-8', to = 'ASCII//TRANSLIT')
lat_11 = aggregate(c(lat_11[2:17]), by= lat_11[1], sum)
names(lat_11) <- gsub("[.]*$|[.]*(?=[.])", "",names(lat_11), perl = TRUE)
names(lat_11) <- gsub("\\.", " ", names(lat_11))
names(lat_11)[9] = 'LPP-LC'
lat_11 = main_function(lat_11, 'VIENOTIBA', 'Briviba Brivs no bailem naida un dusmam', 5,  'Saskanas Centrs')
lat_11 = extra_cols(lat_11, 'Latvia', '2011-09-17', 'Legislative')
lat_11$country = iconv(lat_11$country, from = 'UTF-8', to = 'ASCII//TRANSLIT')
lat_11$weird = countrycode(lat_11$country, 'country', 'weird', custom_dict = lat_dic)
latvian = "AMERIKAS SAVIENOTAS VALSTIS, AZERBAIDZANAS REPUBLIKA, BRAZILIJAS FEDERATIVA REPUBLIKA, KAZAHSTANAS REPUBLIKA, LIELBRITANIJAS UN ZIEMELIRIJAS APVIENOTA KARALISTE, PORTUGALES REPUBLIKA, SLOVENIJAS REPUBLIKA, TURCIJAS REPUBLIKA, VACIJAS FEDERATIVA REPUBLIKA"
latvian = unlist(strsplit(latvian, ", "))
english = c("UNITED STATES",
            "THE REPUBLIC OF AZERBAIJAN",
           "BRAZIL",
            "REPUBLIC OF KAZAKHSTAN",
            "UNITED KINGDOM OF GREAT BRITAIN AND NORTHERN IRELAND",
            "PORTUGAL",
            "SLOVENIA",
            "TURKEY",
            "GERMANY")

for (i in 1:9){
  lat_11$weird[lat_11$country == latvian[i]] = english[i]
}

lat_dic = lat_11[c(1,36)]
lat_11$country = countryname(lat_11$weird)
lat_11 = lat_11[-36]
#2014
lat_14 = read.csv('lat_leg_14.csv', encoding = 'UTF-8')
lat_party = read.csv('lat_14_dic.csv', encoding = 'UTF-8')
lat_14 = lat_14[-1]
names(lat_14)[1:3] = c('country', 'envelopes', 'some_votes')
lat_14$country = gsub("\\(.*", "", lat_14$country)
lat_14$country = trimws(gsub('[0-9]+\\.', '', lat_14$country))
lat_14 = aggregate(c(lat_14[2:16]), by = lat_14[1], sum)
lat_14 = add_column(lat_14, valid_votes = rowSums(lat_14[4:16]), .after = 'some_votes')
lat_14 = lat_14[-3]
names(lat_14)[2] = 'total_votes'
lat_party$party_name =  gsub("\"", "",lat_party$party_name)
lat_party$party_name = iconv(lat_party$party_name, from = 'UTF-8', to = 'ASCII//TRANSLIT')
lat_names = lat_party[,3]
names(lat_14)[4:16] = lat_names
lat_14 = main_function(lat_14, 'Latvijas attistibai', 'No sirds Latvijai', 4, 'Saskana socialdemokratiska partija')
lat_14 = extra_cols(lat_14, 'Latvia', '2014-10-04', 'Legislative')
lat_14$country =  iconv(lat_14$country, from = 'UTF-8', to = 'ASCII//TRANSLIT')
lat_14$weird = countrycode(lat_14$country, 'country', 'weird', custom_dict = lat_dic)
latvian = "APVIENOTIE ARABU EMIRATI, CILE, INDIJAS REPUBLIKA, ISLANDES REPUBLIKA, KARAVIRI UN ZEMESSARGI STARPTAUTISKA OPERACIJA AFGANISTANA, KARAVIRI UN ZEMESSARGI? STARPTAUTISKA OPERACIJA CENTRALAFRIKA, UNGARIJA"
latvian = unlist(strsplit(latvian, ", "))
english = c("UNITED ARAB EMIRATES",
            "CHILE",
            "INDIA",
            "ICELAND",
            "MILITARY Base Afghanistan",
            "MILITARY BASE CENTRAL AFRICA",
            "HUNGARY")

for (i in 1:7){
  lat_14$weird[lat_14$country == latvian[i]] = english[i]
}
lat_14$country = countryname(lat_14$weird)
lat_14$country[23:24] = c("MB Afghanistan", "MB Central Africa")
# MB for military basis
lat_14 = lat_14[-35]

##Legislative 2018
lat_18 = read_xlsx("Latvia/leg_2018/Latvia Leg. 2018 abroad raw data.xlsx")
lat_18 = lat_18[-c(1,125:134), -c(1,22)]
lat_18 = renamer(lat_18, 1)
lat_18$country = trimws(gsub("\\(.*", "", lat_18$country))
lat_18$weird = countryname(lat_18$country)
latvian = c('PORTUGUESE REPUBLIC', 'THE ITALIAN REPUBLIC')
english = c('Portugal', 'Italy')
for (i in 1:2){
  lat_18$weird[lat_18$country == latvian[i]] = english[i]
} 

lat_18$country = countryname(lat_18$weird)
lat_18$country[is.na(lat_18$country)] = 'Military Base'
lat_names = names(lat_18)[5:20]
lat_18[2:20] = lapply(lat_18[2:20], function(y) as.numeric(as.character(y)))
lat_18 = aggregate(c(lat_18[2:20]), by = lat_18[1], sum)
# tbf I don't think the second column is registered_voters
# but but but 
names(lat_18)[2:4] = c('registered_voters', 'total_votes', 'valid_votes')
lat_names = trimws(gsub("\"","", lat_names))
names(lat_18)[5:20] = lat_names
lat_18 = main_function(lat_18, 'Russian Federation of Latvia', "Union of Greens and Farmers", 5, 'Harmony Social Democratic Party')
lat_18 = extra_cols(lat_18, 'Latvia', '2018-10-06', 'Legislative')

###Poland -----------------------------------------------------------------------
# Legislative 2001
pol_01 <- read_xlsx("Poland/2001zagrobw.xlsx")
keep = c(grep("Lista", names(pol_01)))
pol_01 <- pol_01[c(1:8, keep)]
names(pol_01) = gsub(".*-", "", names(pol_01))
names(pol_01) <- trimws(names(pol_01))
pol_01 = pol_01[-c(2,3)]
names(pol_01)[2:6] = c('registered_voters', 'total_votes', 'bla', 'blu', 'valid_votes')
pol_01 = pol_01[-c(4,5)]
pol_01 <- renamer(pol_01, 1)
pol_01 = aggregate(c(pol_01[2:15]), by = pol_01[1], sum)
# the gsub function above shortened one party abbrevation: 
names(pol_01)[5] = "SLD-UP"
pol_01 = main_function(pol_01, 'SLD-UP', 'PPS', 5, 'SLD-UP')
pol_01 = extra_cols(pol_01, 'Poland', '2001-09-23', 'Legislative')
custom_dict$polish = tolower(countrycode::codelist$cldr.name.pl)
custom_dict$polish = iconv(custom_dict$polish, from = 'UTF-8', to = 'ASCII//TRANSLIT')
pol_01$country = iconv(pol_01$country, from = 'UTF-8', to = 'ASCII//TRANSLIT')
pol_01$weird = countrycode(tolower(pol_01$country), 'polish', 'english', custom_dict = custom_dict)
polish = "jugoslawia, macedonia, niderlandy, tajpej"
polish = unlist(strsplit(polish, ', '))
english = c("Yugoslavia", "North Macedonia", "Netherlands", "Taiwan")
for (i in 1:4){
  pol_01$weird[tolower(pol_01$country) == polish[i]] <- english[i]
}

pol_01$country = countryname(pol_01$weird)
pol_01 = pol_01[-c(32)]

# Legislative 2007: 

pol_07 = read_xlsx("Poland/sejm2007zagrobw.xlsx")
keep = c(grep('Komitet', names(pol_07)))
pol_07 = pol_07[c(1:7, keep)]
names(pol_07) = gsub(".*Komitet Wyborczy ", "", names(pol_07))
names(pol_07)[4:7] <- c('registered_voters', 'total_votes', 'total_votes_2', 'valid_votes')
names(pol_07) <- iconv(names(pol_07), from = 'UTF-8', to = 'ASCII//TRANSLIT')
# it is named valid_votes 2 because the distinction is not quite clear -> 
# as the so far understanding total_votes is the number of voters that voted including 
# invalid votes -> for turnout important
pol_07 <- pol_07[-c(2:3,6)]
pol_07 = renamer(pol_07,1)
pol_07 = aggregate(c(pol_07[2:12]), by = pol_07[1], sum)
names(pol_07) <- gsub("[.]*$|[.]*(?=[.])", "",names(pol_07), perl = TRUE)
names(pol_07) <- gsub("\\.", " ", names(pol_07))
pol_07 = main_function(pol_07, 'Polska Partia Pracy', 'Partii Kobiet', 5,  "Platforma Obywatelska RP")
pol_07 = extra_cols(pol_07, 'Poland', '2007-10-21', 'Legislative')
pol_07$country =  iconv(pol_07$country, from = 'UTF-8', to = 'ASCII//TRANSLIT')
pol_07$weird = countrycode(tolower(pol_07$country), 'polish', 'english', custom_dict = custom_dict)
polish = unlist(strsplit("macedonia, miasto tajpej, stany zjednoczone ameryki", ', '))
english = c('North Macedonia', "Taiwan", "US")
for (i in 1:3){
  pol_07$weird[tolower(pol_07$country) == polish[i]] <- english[i]
}
pol_07$country = countryname(pol_07$weird)
pol_07 = pol_07[-26]
# Legislative 2011:

pol_11 = read.csv('pol_leg_2011.csv', encoding = 'UTF-8')
# I will drop valid cards
pol_11 = pol_11[-c(1,2,4,7)]
pol_11 = aggregate(c(pol_11[2:11]), by = pol_11[1], sum)

names(pol_11) <- gsub("[.]*$|[.]*(?=[.])", "",names(pol_11), perl = TRUE)
names(pol_11) <- gsub("\\.", " ", names(pol_11))

pol_11 = main_function(pol_11, "Prawo i Sprawiedliwosc", "Obywatelska RP", 5, "Obywatelska RP")
pol_11 = extra_cols(pol_11, 'Poland', '2011-10-09', 'Legislative')

pol_11$country = iconv(pol_11$country, from = 'UTF-8', to = 'ASCII//TRANSLIT')
pol_11$weird = countrycode(tolower(pol_11$country), 'polish', 'english', custom_dict = custom_dict)
polish = "georgia, koreanska republika ludowo-demokratyczna, macedonia, republika korei"
polish = unlist(strsplit(polish, ", "))
english = c("Georgia", 'North Korea', "North Macedonia", 'South Korea')
for (i in 1:4){
  pol_11$weird[tolower(pol_11$country) == polish[i]] = english[i]
}
pol_11$country = countryname(pol_11$weird)
pol_11$country[is.na(pol_11$country)] = 'Military Bases'
pol_11 = pol_11[-24]


## Legislative 2015
pol_15 = read_xlsx("Poland/Raw Data (Warsaw constituency, includes abroad).xlsx")
pol_15 = pol_15 %>% filter(Gmina == "Zagranica")
## the xlsx is useless
pol_15 = read.csv('pol_leg_15.csv', encoding = 'UTF-8')
pol_15 = pol_15[-1]
names(pol_15)[1:4] = c('country', 'registered_voters', 'total_votes', 'valid_votes')
# here total votes is infered from valid cards -> assuming that means all the votes received (but unsure)
names(pol_15)[5:14] = gsub("Komitet.Wyborczy.", "", names(pol_15)[5:14])
names(pol_15)[5:14] = gsub("Wyborców.", "", names(pol_15)[5:14])
names(pol_15)[5:14] <- gsub("[.]*$|[.]*(?=[.])", "",names(pol_15)[5:14], perl = TRUE)
names(pol_15)[5:14] <- gsub("\\.", " ", names(pol_15)[5:14])
pol_15 = main_function(pol_15, 'Prawo i Sprawiedliwosc', 'Ruch Spoleczny Rzeczypospolitej Polskiej', 5, 'Prawo i Sprawiedliwosc')
pol_15 = extra_cols(pol_15, 'Poland', '2015-10-25', 'Legislative')
pol_15$country = iconv(pol_15$country, from = 'UTF-8', to = 'ASCII//TRANSLIT')
# not using a dictionary now that was created with the translation of legislative 2019 country names below!
pol_15$weird = countrycode(pol_15$country, 'country', 'weird', custom_dict = pol_dic)
polish = "Boliwarianska Republika Wenezueli, Republika Irlandii, Republika Kosowa, Republika Macedonii, Zjednoczone Emiraty Arabskie"
polish = unlist(strsplit(polish, ", "))
english = c('Venezuela', 'Ireland', 'Kosovo', 'Republic of Macedonia', 'United Arab Emirates')
for (i in 1:5){
  pol_15$weird[pol_15$country == polish[i]] = english[i]
}
pol_15$weird = countryname(pol_15$weird)
new_dic = pol_15[c(1,30)]
pol_15$country = pol_15$weird
pol_15 = pol_15[-30]
# names missing


## Legislative 2019

pol_19 = read.csv("Poland/Poland Leg. 2019 Raw Data File.csv", sep = ";", encoding = "UTF-8")
pol_19 = pol_19 %>% filter(Gmina == "zagranica")
# get country name (after last comma of every polling station)
pol_19$country = regmatches(pol_19$Siedziba ,gregexpr(",[^,]*$",pol_19$Siedziba,perl=TRUE))
pol_19$country = gsub(".*, ", "", pol_19$country)
pol_city_dic = pol_19[c(7,43)]
pol_19 = pol_19[-c(1:10, 41,42)]
pol_19[1:30] = lapply(pol_19[1:30], function(y) as.numeric(y))
# translating column names into english using google translate and common sene
#thus irrelevant columns are labeled with one letter. There might be a few problems as there are 20 columns describing 
#overall elections results 
names(pol_19)[c(2, 17, 18, 22)] <- c('registered_voters', "total_votes", "invalid_votes", "valid_votes")
# thing here is -> there are potentially several columns that could count as total_votes
pol_19 = pol_19[c(2, 17, 18, 22:31)]
# removing na party columns
pol_19 = pol_19[-c(6,9,11)]
# aggregate for country
pol_19 = aggregate(c(pol_19[1:9]), by = pol_19[10], sum)

names(pol_19)[6:10] <- c("KOALICJA OBYWATELSKA", "KONFEDERACJA WOLNOSC I NIEPODLEGLOSC", 
                        "POLSKIE STRONNICTWO LUDOWE", "PRAWO I SPRAWIEDLIWOSC", "SOJUSZ LEWICY DEMOKRATYCZNEJ")

pol_19 = main_function(pol_19, "KOALICJA OBYWATELSKA", "SOJUSZ LEWICY DEMOKRATYCZNEJ", 6, "PRAWO I SPRAWIEDLIWOSC")
pol_19 = extra_cols(pol_19, "Poland", "2019-10-13", "Legislative")
pol_19$country = iconv(pol_19$country, from = 'UTF-8', to = 'ASCII//TRANSLIT')

for (i in pol_19$country){
  print(i)
}
english = c("Algeria",
            "Arab Republic of Egypt",
 "Bosnia and Herzegovina",
 "People's Republic of China",
 "Montenegro",
 "Russian Federation",
 "Federative Republic of Brazil",
 "Federal Democratic Republic of Ethiopia",
 "Federal Republic of Nigeria",
 "Georgia",
 "Ireland",
 "Islamic Republic of Iran",
 "Islamic Republic of Pakistan",
 "Japan",
 "Jordanian Hashemite Kingdom",
 "Canada",
 "Swiss Confederation",
 "Korean People's Democratic Republic",
 "Kingdom of Saudi Arabia",
 "Kingdom of Belgium",
 "Kingdom of Denmark",
 "Kingdom of Spain",
 "Morocco",
 "Kingdom of the Netherlands",
 "The Kingdom of Norway",
 "Kingdom of Sweden",
 "Kingdom of Thailand",
 "Malaysia",
 "Mexico",
 "Taipei City",
 "New Zealand",
 "The State of Israel",
 "State of Qatar",
 "State of Kuwait",
 "State of the United Arab Emirates",
 "Republic of Albania",
 "Republic of Angola",
 "Argentine Republic",
 "Republic of Armenia",
 "Republic of Austria",
 "Republic of Azerbaijan",
 "Republic of Belarus",
 "Republic of Bulgaria",
 "Republic of Chile",
 "Republic of Croatia",
 "Republic of Cyprus",
 "Czech Republic",
 "Republic of Estonia",
 "Federal Republic of Germany",
 "Republic of the Philippines",
 "Republic of Finland",
 "French Republic",
 "Hellenic Republic",
 "Republic of India",
 "Republic of Indonesia",
 "Republic of Iceland",
 "Republic of Kazakhstan",
 "Republic of Kenya",
 "Republic of Colombia",
 "Republic of Korea",
 "Republic of Cuba",
 "Republic of Lebanon",
 "Republic of Lithuania",
 "Republic of Latvia",
 "Republic of North Macedonia",
 "Republic of Malta",
 "Republic of Moldova",
 "Republic of Panama",
 "Republic of Peru",
 "South Africa",
 "Portugal",
 "Republic of Senegal",
 "Republic of Serbia",
 "Republic of Singapore",
 "Slovak Republic",
 "Republic of Slovenia",
 "Republic of Tunisia",
 "Republic of Turkey",
 "Republic of Uzbekistan",
 "Italy",
 "Romania",
 "Socialist Republic of Vietnam",
 "United States of America",
 "Ukraine",
 "Hungary",
 "Grand Duchy of Luxembourg",
 "United Republic of Tanzania",
"United Kingdom of Great Britain and Northern Ireland",
 "Australian Union")

pol_19$weird = countryname(english)
pol_dic = pol_19[c(1,21)]
pol_19$country = pol_19$weird
pol_19 = pol_19[-21]
# Presidential 2000
polp_00 = read_xls("Poland/pol_2000/Poland Pres. 2000.xls")
names(polp_00)[c(1,3,5:7)] <- c("country", "registered_voters", "total_votes", "null_votes", "valid_votes")
polp_00 = polp_00[-c(1:2), -c(2,4)]
polp_00[2:17] = lapply(polp_00[2:17], function(y) as.numeric(y))
names(polp_00) = iconv(names(polp_00), from = 'UTF-8', to = 'ASCII//TRANSLIT')
polp_00 = main_function(polp_00, "Dariusz Maciej GRABOWSKI", "Tadeusz Adam WILECKI", 6, "Aleksander KWASNIEWSKI")
polp_00 = extra_cols(polp_00,"Poland" ,"2000-10-08", "Presidential")
polp_00$country = iconv(polp_00$country, from = 'UTF-8', to = 'ASCII//TRANSLIT')
polp_00$weird = countrycode(tolower(polp_00$country), 'polish', 'english', custom_dict = custom_dict)
polish = "jugoslawia, krld, macedonia, republika federalna niemiec, stany zjednoczone ameryki"
polish = unlist(strsplit(polish, ", "))
english = countryname(c("Yugoslavia", "DPRK", "Macedonia",
                            "federal republic of germany", "united states of america"))
for (i in 1:5){
  polp_00$weird[tolower(polp_00$country) == polish[i]] = english[i]
}

polp_00$country =  countryname(polp_00$weird)
polp_00 = polp_00[-c(35)]

# Presidential 2005
polp_05 = read_xls("Poland/pres_2005/Raw Data. Polling Station Level.xls", sheet = 2)
polp_05 = polp_05 %>% filter(`Typ obwodu` == "G")
polp_05 = polp_05[-c(1:5, 7 ,8)]
polp_05$`Adres obwodu` = gsub("(.*?),.*", "\\1", polp_05$`Adres obwodu`)
#names are again for total_votes a bit problematic // 
names(polp_05)[c(1:2, 6:8)] = c("country", "registered_voters", 'total_votes', "valid_votes", 'invalid_votes')
polp_05 = polp_05[-c(3:5)]
name = names(polp_05)[6:17]
polp_05$country = gsub(" Chicago II", "", polp_05$country)
polp_05 = aggregate(c(polp_05[2:17]), by = polp_05[1], sum)
names(polp_05)[6:17] = name
names(polp_05) = iconv(names(polp_05), from = 'UTF-8', to = 'ASCII//TRANSLIT')
polp_05 = main_function(polp_05, "Bochniarz Henryka Teodora", "Tyminski Stanislaw", 6, "Tusk Donald Franciszek")
# national winner first round ofc
polp_05 = extra_cols(polp_05, "Poland",  "2005-10-09", 'Presidential')
polp_05$country = iconv(polp_05$country, from = 'UTF-8', to = 'ASCII//TRANSLIT')
polp_05$weird = countrycode(tolower(polp_05$country), 'polish', 'english', custom_dict = custom_dict)
polish = "azerbajdzan, izrael tel aviv, korea, macedonia, rpa, serbia i czarnogora, tajpej, wielka brytania i irlandia, zimbabwe harare"
polish = unlist(strsplit(polish, ", "))
english = countryname(c("Azerbaijan", "Israel", "Korea", "Macedonia",
              "South Africa", "Serbia and Montenegro", "Taipei", "Great Britain",
              "Zimbabwe harare"))
for (i in 1:9){
  polp_05$weird[tolower(polp_05$country) == polish[i]] = english[i]
}

polp_05$country = countryname(polp_05$weird)
polp_05 = polp_05[-c(35)]

## Presidential 2010
polp_10 = read.csv("Poland/pres_2010/Raw Data File. Country Level.csv", sep = ";", encoding = "UTF-8", header = F)
polp_10 = row_to_names(polp_10, 1)
names(polp_10)[c(1,4, 6,7)] = c("country", "registered_voters", "total_votes", "valid_votes")
polp_10 = polp_10[-c(2,3,5)]
names(polp_10)[c(6,7,14)] = c("KACZYNSKI Jaroslaw Aleksander", "KOMOROWSKI Bronislaw Maria", "ZIETEK Boguslaw Zbigniew")
polp_10[2:14] = lapply(polp_10[2:14], function(y) as.numeric(y))
polp_10 = main_function(polp_10, "JUREK Marek", "ZIETEK Boguslaw Zbigniew", 5, "KOMOROWSKI Bronislaw Maria")
polp_10 = extra_cols(polp_10, "Poland", "2010-06-20", "Presidential")
polp_10$country = countrycode(polp_10$country, "iso2c", "country.name")
polp_10$country[is.na(polp_10$country)] = "Kosovo"
# just for continuity: 
polp_10$country = countryname(polp_10$country)


## Presidential 2015
polp_15 = read_xls("Poland/pres_2015/PollingStation Codes wyniki_tura1-1.xls")
polp_15 = polp_15 %>% filter(Gmina == "Zagranica")
polp_15$country =  regmatches(polp_15$Siedziba ,gregexpr(",[^,]*$",polp_15$Siedziba,perl=TRUE))
polp_15 = polp_15[-c(1),]
polp_15$country = gsub(".*, ", "", polp_15$country)
polp_15 = polp_15[-c(1:7)]
names(polp_15)[c(1,17:19)] = c("registered_voters", "total_votes", "invalid_votes", "valid_votes")
polp_15 = polp_15[c(1,17:19, 21:32)]
name = names(polp_15)[5:15]
polp_15 = aggregate(c(polp_15[1:15]), by = polp_15[16], sum)
names(polp_15)[6:16] = iconv(name, from = 'UTF-8', to = 'ASCII//TRANSLIT')
polp_15 = main_function(polp_15, "Grzegorz Michal Braun", "Jacek Wilk", 6, "Andrzej Sebastian Duda")
polp_15 = extra_cols(polp_15, "Poland", "2015-05-10","Presidential")
polp_15$country = iconv(polp_15$country, from = 'UTF-8', to = 'ASCII//TRANSLIT')

polp_15$country = countrycode(polp_15$country, 'country', 'weird', custom_dict = new_dic)
polp_15$country[is.na(polp_15$country)] = "Iraq"
#### Czechia ===================================================================
##Presidential 2013
czp_13 = read.csv("Czechia/pres_2013/result (5).csv")
czp_13 = czp_13[-c(1:7,9, 12:15, 22:45)]
names(czp_13) = c('country', 'party_code', 'votes', 'registered_voters', 'total_votes', 'turnout', 
                       'also_total_votes', 'valid_votes', 'valid_votes_perc')
czp_13 = czp_13[-c(6,7,9)]

czp_13_codes = read.csv("Czechia/pres_2013/result (6).csv", encoding = "UTF-8")
czp_13_codes = czp_13_codes[c(5:7)]
czp_13_codes$candidate <- str_c(czp_13_codes$CR__KANDIDAT__JMENO," ", czp_13_codes$CR__KANDIDAT__PRIJMENI)
czp_13_codes = czp_13_codes[-c(2,3)]
names(czp_13_codes)[1] = "party_code"
czp_13_codes$candidate = iconv(czp_13_codes$candidate, from = 'UTF-8', to = 'ASCII//TRANSLIT')
czp_13$party = countrycode(czp_13$party_code, "party_code", "candidate", custom_dict = czp_13_codes)
write.csv(czp_13, "cz_pres_2013.csv")

czp_13 = czp_13[-2]
czp_13 = czp_13 %>% pivot_wider(names_from = party, values_from = votes)
czp_13 = czp_13[-14]
czp_13[is.na(czp_13)] = 0
czp_13 = main_function(czp_13, "Jan Fischer", "Tatana Fischerova", 5, "Milos Zeman")
czp_13 = extra_cols(czp_13, "Czechia", "2013-01-11","Presidential")
czp_13$country = countrycode(czp_13$country, "iso2c", "country.name")
czp_13$country[is.na(czp_13$country)] = "Kosovo"
czp_13$country = countryname(czp_13$country)

## Presidential 2018
czp_18 = read.csv("Czechia/pres_2018/result (4).csv")
czp_18 = czp_18[-c(1:7,9, 12:15, 22:45)]
names(czp_18) = c('country', 'party_code', 'votes', 'registered_voters', 'total_votes', 'turnout', 
                  'also_total_votes', 'valid_votes', 'valid_votes_perc')
czp_18 = czp_18[-c(6,7,9)]
czp_18_cd = read.csv("Czechia/pres_2018/result (7).csv", encoding = "UTF-8")
czp_18_cd = czp_18_cd[c(5:7)]
czp_18_cd$candidate <- str_c(czp_18_cd$CR__KANDIDAT__JMENO," ", czp_18_cd$CR__KANDIDAT__PRIJMENI)
czp_18_cd = czp_18_cd[-c(2,3)]
names(czp_18_cd)[1] = "party_code"
czp_18_cd$candidate = iconv(czp_18_cd$candidate, from = 'UTF-8', to = 'ASCII//TRANSLIT')
czp_18$party = countrycode(czp_18$party_code, "party_code", "candidate", custom_dict = czp_18_cd)
write.csv(czp_18, "cz_pres_2018.csv")

czp_18 = czp_18[-2]
czp_18 = czp_18 %>% pivot_wider(names_from = party, values_from = votes)
czp_18 = czp_18[-14]
czp_18[is.na(czp_18)] = 0
czp_18 = main_function(czp_18, "Michal Horacek", "Petr Hannig", 5, "Milos Zeman")
czp_18 = extra_cols(czp_18, "Czechia", "2018-01-12", "Presidential")
czp_18 = czp_18[-1,]
czp_18$country = countrycode(czp_18$country, "iso2c", "country.name")
czp_18$country[is.na(czp_18$country)] = "Kosovo"
czp_18$country = countryname(czp_18$country)


## Legislative 2006 
cz_06 = read.csv("Czechia/leg_2006/result (6).csv", encoding = 'UTF-8')
cz_06_names = read.csv("Czechia/leg_2006/result (7).csv", encoding = 'UTF-8')
cz_06 = cz_06[-c(1:7, 9:11, 21:44)]
names(cz_06)[c(1:3,6,8,9)] = c('country', 'registered_voters', 'total_votes', 'valid_votes',
                               'party_number', 'party_votes')
cz_06 = cz_06[c(1:3,6,8,9)]
cz_06_names = cz_06_names[c(17,18)]
cz_06_names = cz_06_names %>% filter(!duplicated(KRAJ__STRANA__KSTRANA))
cz_06_names$KRAJ__STRANA__NAZ_STR = iconv(cz_06_names$KRAJ__STRANA__NAZ_STR, from = 'UTF-8', to = 'ASCII//TRANSLIT')
names(cz_06_names) = c('party_code', 'party_name')
cz_06$party_number = countrycode(cz_06$party_number, 'party_code', 'party_name', custom_dict = cz_06_names)
cz_06$country = iconv(cz_06$country, from = 'UTF-8', to = 'ASCII//TRANSLIT')
names(cz_06)[5] = 'party_name'
#write.csv(cz_06, 'cz_leg_06.csv')

cz_06 = cz_06 %>% pivot_wider(names_from = party_name,values_from = party_votes)
cz_06 = main_function(cz_06, 'Strana zdraveho rozumu', 'STRANA ROVNOST SANCI', 5, 'Obcanska demokraticka strana')
cz_06 = extra_cols(cz_06, 'Czechia', '2006-06-02', 'Legislative')

custom_dict$czech = tolower(countrycode::codelist$cldr.name.cs)
custom_dict$czech = iconv(custom_dict$czech, from = 'UTF-8', to = 'ASCII//TRANSLIT')
cz_06$weird = countrycode(tolower(cz_06$country), 'czech', 'english', custom_dict = custom_dict)
czech = "korejska lid.demokr.r, korejska republika, spojene arabske emira, srbsko a cerna hora, svaty stolec"
czech = unlist(strsplit(czech, ", "))

english = "Democratic People's Republic of Korea, Republic of Korea, United Arab Emirates, Serbia and Montenegro, Holy See"
english = unlist(strsplit(english, ", "))
english = countryname(english)
for (i in 1:5){
  cz_06$weird[tolower(cz_06$country) == czech[i]] = english[i]
}

cz_06$country = countryname(cz_06$weird)
cz_06 = cz_06[-48]

# big problems -> all the votes have the same value!! for each country


## legislative 2010

cz_10 = read.csv("Czechia/leg_2010/result (4).csv", encoding = 'UTF-8')
cz_10 = cz_10[-c(1:6, 8:11, 21:44)]
names(cz_10)[c(1:3,6,8,9)] = c('country', 'registered_voters', 'total_votes', 'valid_votes',
                               'party_number', 'party_votes')
cz_10 = cz_10[c(1:3,6,8,9)]
cz_10 = cz_10 %>% filter(!(is.na(registered_voters)))

cz_10_names = read.csv("Czechia/leg_2010/result (5).csv", encoding = 'UTF-8')
cz_10_names = cz_10_names[c(17,18)]
cz_10_names = cz_10_names %>% filter(!duplicated(KRAJ__STRANA__KSTRANA))
names(cz_10_names) = c('party_code', 'party_name')
cz_10_names$party_name = iconv(cz_10_names$party_name, from = 'UTF-8', to = 'ASCII//TRANSLIT')
cz_10$party_number = countrycode(cz_10$party_number, 'party_code', 'party_name', custom_dict = cz_10_names)
names(cz_10)[5] = 'party_name'
write.csv(cz_10, 'cz_leg_10.csv')

cz_10 = cz_10 %>% pivot_wider(names_from = party_name,values_from = party_votes)
cz_10[is.na(cz_10)] = 0
cz_10 = main_function(cz_10, 'Ceska str.socialne demokrat.', 'STOP', 5, 'Ceska str.socialne demokrat.')
cz_10 = extra_cols(cz_10, 'Czechia', '2010-05-28', 'Legislative')
cz_10$country = countrycode(cz_10$country, 'iso2c', 'country.name')
cz_10$country = countryname(cz_10$country)
cz_10$country[is.na(cz_10$country)] = 'Kosovo'

#### Indonesia =================================================================
#Legislative 2019
ind_19 = read.csv('indo_leg_19.csv')
ind_19$WILAYAH =  gsub("\\(.*", "", ind_19$WILAYAH)
ind_19$WILAYAH = trimws(ind_19$WILAYAH)
ind_19$WILAYAH = gsub(".*,", "", ind_19$WILAYAH)
ind_19 = ind_19[-c(1,19)]
ind_19 = ind_19 %>% filter(!(PKB == 'Data belum tersedia'))
ind_19[2:17] = lapply(ind_19[2:17], function(y) as.numeric(y))
ind_19 = renamer(ind_19, 1)
ind_19 = aggregate(c(ind_19[2:17]), by = ind_19[1], sum)
ind_19 = add_column(ind_19, valid_votes = rowSums(ind_19[2:17]), .after = 'country')
ind_19 = main_function(ind_19, 'PKB', 'PKPI', 3, 'PDIP')
ind_19 = extra_cols(ind_19, 'Indonesia', '2019-04-17', 'Legislative')
# country missing // 
ind_19$country = trimws(ind_19$country)
custom_dict$indonesian = tolower(countrycode::codelist$cldr.name.id)
ind_19$weird = countrycode(tolower(ind_19$country), 'indonesian', 'english', custom_dict = custom_dict)

indonesian = "brunei darussalam, chile, ethiopia, hongaria, inggris, kazakhstan, libya, mexico, morocco, myanmar, perancis, republik ceko, republik rakyat tiongkok, sabah, slowakia, sri langka, ukrania"
indonesian = unlist(strsplit(indonesian, ", "))
english = "Brunei Darussalam, Chile, Ethiopia, Hungary, United Kingdom, Kazakhstan, Libya, Mexico, Morocco, Myanmar, France, Czech Republic, People's Republic of China, Sabah, Slovakia, Sri Lanka, Ukraine"
english = unlist(strsplit(english, ", "))

for (i in 1:17){
  ind_19$weird[tolower(ind_19$country) == indonesian[i]] = english[i]
}

ind_19$weird = countryname(ind_19$weird)
# not matched Sabah (whatever that is)
ind_dic = ind_19[c(1,40)]
ind_19$country = ind_19$weird
ind_19$country[is.na(ind_19$country)] = "Sabah (?)"
ind_19 = ind_19[-c(40)]

# Presidential 2019
indp_19 = read.csv('indo_pres_19.csv')
indp_19$WILAYAH =  gsub("\\(.*", "", indp_19$WILAYAH)
indp_19$WILAYAH = trimws(indp_19$WILAYAH)
indp_19$WILAYAH = gsub(".*,", "", indp_19$WILAYAH)
indp_19 = indp_19[-1]
# I will translate candidate names into party straight away -> easy here 
names(indp_19) = c('country', 'PDIP', 'Gerindra')
indp_19 = aggregate(c(indp_19[2:3]), by = indp_19[1], sum)
indp_19 = add_column(indp_19, valid_votes = rowSums(indp_19[2:3]), .after = 'country')
indp_19 = main_function(indp_19, 'PDIP', 'Gerindra', 3, 'PDIP')
indp_19 = extra_cols(indp_19, 'Indonesia', '2019-04-17', 'Presidential')

indp_19$weird = countrycode(trimws(indp_19$country), 'country', 'weird', custom_dict = ind_dic)
indonesian = " Bosnia-Herzegovina, Kroasia, Kuba, Madagaskar, Sabah, Suriah, Zimbabwe"
indonesian = unlist(strsplit(indonesian, ", "))
english = c("Bosnia", "Croatia", "Cuba", "Madagascar", "Sabah", "Syria", "Zimbabwe")
for (i in 1:7){
  indp_19$weird[trimws(indp_19$country) == indonesian[i]] = english[i]
}

indp_19$country = countryname(indp_19$weird)
indp_19 = indp_19[-12]
# again Sabah not matched // 
### Ecuador:--------------------------------------------------------------------
# Legislative 2009
setwd("C:/Users/lenovo/Documents/BSE/RA/Data/Data/EVP/Ecuador/Leg. 2009 National MPs")
my_files <- list.files(pattern = "\\.xlsx$")
my_files_2 = my_files[grep("-2.xlsx", my_files)]
my_files_3 = my_files[-grep("-2.xlsx", my_files)]
df_list <- lapply(my_files_3, read_xlsx, range = cell_rows(1:19))
my_files_3 = gsub("\\.xlsx$", "", my_files_3)
my_files_3 = gsub("PAIS_", "", my_files_3)
my_files_3 = gsub("S_", "", my_files_3)
names(df_list) = my_files_3
for(i in seq_along(df_list))
  df_list[[i]]$country = names(df_list)[i]
ec_09 <- do.call(rbind, df_list)
rownames(ec_09) = NULL

df_list = lapply(my_files_2, read_xlsx, range = cell_rows(1:7))
for (i in 1:41){
  df_list[[i]] = df_list[[i]][-c(3:4)]
}

my_files_2 = gsub("\\-2.xlsx$", "", my_files_2)
my_files_2 = gsub("PAIS_", "", my_files_2)
my_files_2 = gsub("S_", "", my_files_2)

names(df_list) = my_files_2
for(i in seq_along(df_list))
  df_list[[i]]$country = names(df_list)[i]
ec_09_e <- do.call(rbind, df_list)
rownames(ec_09_e) = NULL

ec_09 = ec_09[-3]
ec_09 = ec_09 %>% pivot_wider(names_from = `ORGANIZACIÓN POLÍTICA`, values_from = VOTOS)
ec_09_e = ec_09_e %>% pivot_wider(names_from = INFORMACIÓN, values_from = TOTAL)

ec_09 = left_join(ec_09, ec_09_e)
# how can be the number of voters half as big as the number of valid votes? that will be dropped then 
ec_09 = ec_09[-c(20,21,25)]
#also values for cuba are missing but I'm unable to find them // 
ec_09 = main_function(ec_09, 'MPAIS', 'MNCS', 5, 'MPAIS')
ec_09 = extra_cols(ec_09, 'Ecuador', '2009-04-26', 'Legislative')
ec_09$weird = countrycode(tolower(ec_09$country), 'spanish', 'english', custom_dict = custom_dict)
spanish = 'belgica, canada, corea (sur) republica de, espaaa, estados unidos de america, federacion de rusia, hungria, japon, mexico, paises bajos (holanda), panama, peru, reino unido de gran breta, republica dominicana'
spanish = unlist(strsplit(spanish, ", "))
english = c("belgium", "canada", "korea (south) republic of", "spain", "united states of america", "russian federation",
            "hungary", "japan", "mexico", "netherlands",
            "panama", "peru", "uk", "dominican republic")

for (i in 1:14){
  ec_09$weird[tolower(ec_09$country) == spanish[i]] = english[i]
}

ec_09$country = countryname(ec_09$weird)
ec_09 = ec_09[-46]
names(ec_09)[5:7] = c('valid_votes', 'blanco_votes', 'null_votes')


## Legislative 2013
# italy is missing
setwd("C:/Users/lenovo/Documents/BSE/RA/Data/Data/EVP/Ecuador/Leg. 2013 National MPs")
my_files <- list.files(pattern = "\\.xlsx$")
my_files_2 = my_files[grep("-2.xlsx", my_files)]
my_files_3 = my_files[-grep("-2.xlsx", my_files)]

df_list <- lapply(my_files_3, read_xlsx, range = cell_rows(1:12))
my_files_3 = gsub("\\.xlsx$", "", my_files_3)
my_files_3 = gsub("PAIS_", "", my_files_3)
names(df_list) = my_files_3
for(i in seq_along(df_list))
  df_list[[i]]$country = names(df_list)[i]
ec_13 <- do.call(rbind, df_list)
rownames(ec_13) = NULL

df_list = lapply(my_files_2, read_xlsx, range = cell_rows(1:7))
for (i in 1:46){
  df_list[[i]] = df_list[[i]][-c(3:4)]
}

my_files_2 = gsub("\\-2.xlsx$", "", my_files_2)
my_files_2 = gsub("PAIS_", "", my_files_2)
names(df_list) = my_files_2
for(i in seq_along(df_list))
  df_list[[i]]$country = names(df_list)[i]
ec_13_e <- do.call(rbind, df_list)
rownames(ec_13_e) = NULL

ec_13 = ec_13[-3]
ec_13 = ec_13 %>% pivot_wider(names_from = `ORGANIZACIÓN POLÍTICA`, values_from = VOTOS)
ec_13_e = ec_13_e %>% pivot_wider(names_from = INFORMACIÓN, values_from = TOTAL)
ec_13 = left_join(ec_13, ec_13_e)
ec_13 = ec_13[-c(13,14,18)]
ec_13 = main_function(ec_13, 'MPAIS', 'AVANZA', 5, 'MPAIS')
ec_13 = extra_cols(ec_13, 'Ecuador', '2013-02-17', 'Legislative')
ec_13$weird = countrycode(tolower(ec_13$country), 'spanish', 'english', custom_dict = custom_dict)
spanish = "belgica, canada, corea (sur) republica de, espaaa, estados unidos de america, federacion de rusia, hungria, japon, mexico, paises bajos (holanda), panama, peru, qatar, reino unido de gran bretaaa e irlanda del norte, republica dominicana, sudafrica, turquia"
spanish = unlist(strsplit(spanish, ", "))
english = c('Belgium', 'Canada', 'South Korea', 'Spain', 'US', 'Russia', 'Hungary', 'Japan', 'Mexico', 'Netherlands',
            'Panama', 'Peru', 'Qatar', 'UK', 'Dominican Republic', 'South Africa', 'Turkey')

for (i in 1:17){
  ec_13$weird[tolower(ec_13$country) == spanish[i]] = english[i]
}

ec_13$country = countryname(ec_13$weird)
ec_13 = ec_13[-32]
names(ec_13)[5:7] = c('valid_votes','blanco_votes', 'null_votes')

## Presidential Election:
direct = c("C:/Users/lenovo/Documents/BSE/RA/Data/Data/EVP/Ecuador/Sources Ecuador Pres. 2006",
           "C:/Users/lenovo/Documents/BSE/RA/Data/Data/EVP/Ecuador/Sources Ecuador Pres. 2009",
           "C:/Users/lenovo/Documents/BSE/RA/Data/Data/EVP/Ecuador/Sources Ecuador Pres. 2013")

df_names = c('ecp_06', 'ecp_09', 'ecp_13')

number_range = c(14, 9,9)
for (i in 1:3){
  setwd(direct[i])
  my_files <- list.files(pattern = "\\.xlsx$")
  my_files_2 = my_files[grep("-2.xlsx", my_files)]
  my_files_3 = my_files[-grep("-2.xlsx", my_files)]
  df_list <- lapply(my_files_3, read_xlsx, range = cell_rows(1:number_range[i]))
  my_files_3 = gsub("\\.xlsx$", "", my_files_3)
  my_files_3 = gsub("PAIS_", "", my_files_3)
  names(df_list) = my_files_3
  for(j in seq_along(df_list))
    df_list[[j]]$country = names(df_list)[j]
  df <- do.call(rbind, df_list)
  rownames(df) = NULL
  df_list = lapply(my_files_2, read_xlsx, range = cell_rows(1:7))
  for (k in seq_along(df_list)){
    df_list[[k]] = df_list[[k]][-c(3:4)]
  }
  my_files_2 = gsub("\\-2.xlsx$", "", my_files_2)
  my_files_2 = gsub("PAIS_", "", my_files_2)
  names(df_list) = my_files_2
  for(l in seq_along(df_list)){
    df_list[[l]]$country = names(df_list)[l]
  }
  df_e <- do.call(rbind, df_list)
  rownames(df_e) = NULL
  df = df[-3]
  df = df %>% pivot_wider(names_from = `ORGANIZACIÓN POLÍTICA`, values_from = VOTOS)
  df_e = df_e %>% pivot_wider(names_from = INFORMACIÓN, values_from = TOTAL)
  df = left_join(df, df_e)
  assign(df_names[i], df)
}

## Presidential 06
ecp_06 = ecp_06[-c(15,16,20)]
ecp_06 = main_function(ecp_06, 'ID/RED', 'INA', 5, 'PRIAN')
ecp_06 = extra_cols(ecp_06, 'Ecuador', '2006-10-15', 'Presidential')
names(ecp_06)[5:7] = c('valid_votes', 'blanco_votes', 'null_votes')
ecp_06$weird = countrycode(tolower(ecp_06$country), 'spanish', 'english', custom_dict = custom_dict)
# actually same names did not match as above // 
for (i in 1:14){
  ecp_06$weird[tolower(ecp_06$country) == spanish[i]] = english[i]
}

ecp_06$country = countryname(ecp_06$weird)
ecp_06 = ecp_06[-36]
## Presidential 2009
ecp_09 = ecp_09[-c(10,11,15)]
ecp_09 = main_function(ecp_09, 'MPAIS', 'MIJS', 5, 'MPAIS')
ecp_09 = extra_cols(ecp_09, 'Ecuador', '2009-04-26', 'Presidential')
names(ecp_09)[5:7] = c('valid_votes', 'blanco_votes', 'null_votes')
# it is the same names in the same order as 2006
ecp_09$country = ecp_06$country

##Presidential 2013
ecp_13 = ecp_13[-c(10,11,15)]
ecp_13 = main_function(ecp_13, 'MPAIS', 'RUPTURA', 5, 'MPAIS')
ecp_13 = extra_cols(ecp_13, 'Ecuador', '2013-02-17', 'Presidential')
names(ecp_13)[5:7] = c('valid_votes', 'blanco_votes', 'null_votes')
ecp_13$weird = countrycode(tolower(ecp_13$country), 'spanish', 'english', custom_dict = custom_dict)
spanish = "belgica, canada, corea (sur) republica de, espaaa, estados unidos de america, federacion de rusia, hungria, japon, mexico, paises bajos (holanda), panama, peru, qatar, reino unido de gran bretaaa e irlanda del norte, republica dominicana, something, sudafrica, turquia"
spanish = unlist(strsplit(spanish, ", "))
english = c('Belgium', 'Canada', 'South Korea', 'Spain', 'US', 'Russia', 'Hungary', 'Japan', 'Mexico', 'Netherlands',
            'Panama', 'Peru', 'Qatar', 'UK', 'Dominican Republic', 'something', 'South Africa', 'Turkey')
for (i in 1:18){
  ecp_13$weird[tolower(ecp_13$country) == spanish[i]] = english[i]
}

ecp_13$country = countryname(ecp_13$weird)
ecp_13 = ecp_13[-26]
## there is one NA country -> where the excel had just the continent (Caribik, South America, Africa but no name)


#### Croatia ------------------------------------------------------------------
# Pres 200
cro <- read_xlsx("Croatia/pres_2000/Predsjednik 2000 po BM - 1. krug.xlsx", sheet = 2)
names(cro)[1:5] = c('country', 'registered_voters', 'total_votes', 'valid_votes', 'invalid_votes')
names(cro)[6:14] <- iconv(names(cro)[6:14], from = 'UTF-8', to = 'ASCII//TRANSLIT')
cro = main_function(cro, 'Drazen Budisa', 'Dr. Zvonimir Separovic', 6, 'Stjepan Mesic')
cro = extra_cols(cro, 'Croatia', '2000-01-24', 'Presidential')
# Transforming names with information from other croatian elections
cro$weird = countrycode(cro$country, '...5', 'english', custom_dict = croatia_names)
croatia = "DIJASPORA U RH, SR JUGOSLAVIJA, ŠVICARSKA KONFEDERACIJA, U.K.V.BRITANIJE I S.IRSKE, VENEZUELA"
croatia = unlist(strsplit(croatia , ", "))
english = c('Diaspora', "Yugoslavia", 'Switzerland', 'UK', 'Venezuela')
for (i in 1:5){
  cro$weird[cro$country == croatia[i]] = english[i]
}
cro$country = countryname(cro$weird)
cro$country[cro$weird == 'Diaspora'] = 'Diaspora'
cro = cro[-29]

### Moldova ===================================================================
# Legislative 2014
mol_14 = read_xls("Moldova/leg_2014/RawData File. Moldova Leg. 2014..xls")
mol_14 = row_to_names(mol_14, 1)
mol_14 = mol_14[-c(1:310),]
names(mol_14)[1:8] = c('bla', 'country', 'bla_1', 'bla_2', 'registered_voters', 'total_votes', 'bla_3', 'invalid_votes')
names(mol_14)[34:36] = c('valid_votes', 'blaa', 'blaaa')
mol_14 = mol_14[-c(1,3,4,7, 35:36)]
# Removing polling station (city)  from name for aggregation
mol_14$country =  gsub("(.*),.*", "\\1", mol_14$country)
mol_14 = mol_14[-96,]
#removing special characters
names(mol_14) = iconv(names(mol_14), from = 'UTF-8', to = 'ASCII//TRANSLIT')
mol_14$country = iconv(mol_14$country, from = 'UTF-8', to = 'ASCII//TRANSLIT')
mol_14[2:30] = lapply(mol_14[2:30], function(y) as.numeric(y))
mol_14 = aggregate(c(mol_14[2:30]), by = mol_14[1], sum)
names(mol_14)[5:29] = gsub("[.]*$|[.]*(?=[.])", "",names(mol_14)[5:29], perl = TRUE)
names(mol_14)[5:29] <- gsub("\\.", " ", names(mol_14)[5:29])
mol_14 = main_function(mol_14, "Partidul Democrat din Moldova", "Partidul Politic Pentru Neam si Tara", 6, "Partidul Politic Partidul Socialistilor din Republica Moldova")
mol_14 = extra_cols(mol_14, 'Moldova', "2014-11-30", 'Legislative')

mol_14$weird_2 = trimws(gsub('republica', "", tolower(mol_14$country)))
mol_14$weird = countrycode(mol_14$weird_2, 'romanian', 'english', custom_dict = custom_dict)
romanian = "ceha, confederatia elvetiana, elena, federala germania, federatia rusa, franceza, italiana, populara chineza, portugheza, principatul monaco, regatul belgiei, regatul spaniei, regatul suediei, regatul unit al marii britaniei ?i irlandei de nord, statul israel, statul qatar, tarile de jos"
romanian = unlist(strsplit(romanian, ", "))
english = c("Czechia",
            "Swiss Confederation",
            "Greece", 
            "Germany",
            "Russian Federation",
            "France",
            "italy", 
            "China",
            "Portugal",
            "Monaco",
            "Belgium",
            "Spain",
            "Sweden",
            "UK",
            "Israel",
            "Qatar",
            "Netherlands")

for (i in 1:17){
  mol_14$weird[mol_14$weird_2 == romanian[i]] = english[i]
}

mol_14$country = countryname(mol_14$weird)
mol_14 = mol_14[-c(61,62)]

# Presidential 2016
mol_16 = read_xlsx("Moldova/pres_2016/1stRound. finalallt1prez20175034720_6383668.xlsx", sheet = 2)
mol_16 = row_to_names(mol_16, 2)
mol_16 = mol_16[-c(1:326),]
mol_16 = mol_16[-c(101, 102), -c(1,3,4,20,21)]
mol_16 = mol_16[-c(4)]
names(mol_16)[c(1:4, 15)] = c('country', 'registered_voters', 'total_votes', 'invalid_votes', 'valid_votes') 
names(mol_16) = gsub(", Candidat independent", "", names(mol_16))
names(mol_16) = gsub(".*,", "", names(mol_16))
names(mol_16) = gsub("Partidul Politic ", "", names(mol_16))
#problem with the encoding

mol_16$country =  gsub("(.*)-.*", "\\1", mol_16$country)
mol_16$country = gsub("-.*", "", mol_16$country)
mol_16[2:15] = lapply(mol_16[2:15], function(y) as.numeric(y))
names(mol_16) = iconv(names(mol_16), from = "UTF-8", to = 'ASCII//TRANSLIT')
# a bit problematic -> will translate names by hand //
mol_16 = aggregate(c(mol_16[2:15]), by = mol_16[1], sum)
names(mol_16)[5:14] = c('Partidul Democrat din Moldova','Partidul Liberal', 'Partidul Popular European din Moldova','Partidul Nostru',
                        'Partidul Actiune si Solidaritate','Partidul Socialistilor din Republica Moldova',
                        'Silvia Radu', 'Maia Laguta', 'Partidul Dreapta', "Valeriu Ghiletchi")

mol_16 = main_function(mol_16, 'Partidul Democrat din Moldova', "Valeriu Ghiletchi", 6, 'Partidul Socialistilor din Republica Moldova')
mol_16 = extra_cols(mol_16, 'Moldova', '2016-10-30', 'Presidential')
mol_16$country = iconv(mol_16$country, from = "UTF-8", to = 'ASCII//TRANSLIT')
mol_16$country = trimws(mol_16$country)
custom_dict$romanian = iconv(custom_dict$romanian, from = "UTF-8", to = 'ASCII//TRANSLIT')
mol_16$weird = countrycode(tolower(mol_16$country), 'romanian', 'english', custom_dict = custom_dict)
romanian = "elvetia, franta, marea britanie, olanda, sua"
romanian = unlist(strsplit(romanian, ", "))
english = c('Switzerland', 'France', 'UK', 'Netherlands', 'US')
for (i in 1:5){
  mol_16$weird[tolower(mol_16$country) == romanian[i]] = english[i]
}
mol_16$country = countryname(mol_16$weird)
mol_16 = mol_16[-31]


## Legislative 2019
paths = c("Moldova/Raw/Circumscription 49 Proportional Republica Moldova.xlsx", 
          "Moldova/Raw/Circumscription 50 Proportional Republica Moldova.xlsx",
          "Moldova/Raw/Circumscription 51 Proportional Republica Moldova.xlsx")

df_list = lapply(paths, read_xlsx, skip = 5)

df_list[[3]] = df_list[[3]][-28]
df_list[[2]] = df_list[[2]][-c(1:2),]
df_list[[1]] = df_list[[1]][-1,]
df_list[[3]] = df_list[[3]][-c(1:5),]
df_list[[1]] = add_column(df_list[[1]], country = NA, .after = "Sectia de votare")
for (i in 2:3){
  names(df_list[[i]])[3] = "country"
}

mol_19 = do.call(rbind, df_list)
mol_19$country[1:27] = c("Azerbaijan", 'Belarus', 'Belarus', 'China', "United Arab Emirates",
                         "Georgia", 'Israel', 'Israel', 'Japan', 'Qatar', 'Russia', 'Russia', 'Russia',
                         'Russia', 'Russia', 'Russia', 'Russia', 'Russia', 'Russia', 'Russia', 'Russia', 
                         'Turkey', 'Turkey', 'Turkey', 'Ukraine', 'Ukraine', 'Ukraine')

mol_19 = mol_19[-c(1:2)]
# forgot something // 
mol_19 = mol_19[-c(2:3, 6,24,25)]
names(mol_19)[2:5] = c('registered_voters', 'total_votes', 'invalid_votes', 'valid_votes')
mol_19[2:20] = lapply(mol_19[2:20], function(y) as.numeric(y))
mol_names = names(mol_19)

mol_19 = aggregate(c(mol_19[2:20]), by = mol_19[1], sum)
mol_names = iconv(mol_names, from = 'UTF-8', to =  'ASCII//TRANSLIT')
mol_names = gsub("\"", "", mol_names)
names(mol_19) = mol_names
mol_19 = main_function(mol_19, "Partidul Democrat din Moldova", "Partidul Liberal", 6, "Partidul Politic Partidul Socialistilor din Republica Moldova")
mol_19 = extra_cols(mol_19, "Moldova", "2019-02-24", 'Legislative')
mol_19$country = countryname(mol_19$country)


#### Paraguay =================================================================
df_par = read.csv("Paraguay/resultados-1996-2018-municipales-y-generales.csv", sep = ";")
df_par = df_par %>% filter(depdes == "EXTERIOR")
# drop unused columns // (that's not in the csv - added later)
df_par = df_par[-c(2:5, 7,9,11, 13,15)]
par_13 = df_par %>% filter(año == 2013 & cand_desc == "PRESIDENTE Y VICEPRESIDENTE DE LA RCA.")
par_18 = df_par %>% filter(año == 2018 & cand_desc == "PRESIDENTE Y VICEPRESIDENTE DE LA RCA.")
parl_13 = df_par %>% filter(año == 2013 & cand_desc == "PARLAMENTARIOS DEL MERCOSUR")
parl_18 = df_par %>% filter(año == 2018 & cand_desc == "PARLAMENTARIOS DEL MERCOSUR")

write.csv(par_13, 'par_pres_2013.csv')
write.csv(par_18, "par_pres_2018.csv")
write.csv(parl_13, "par_leg_2013.csv")
write.csv(parl_18, "par_leg_2018.csv")


df_list = list(par_13, par_18, parl_13, parl_18)

for (i in 1:4){
  df_list[[i]] = df_list[[i]][-c(1,5)]
  names(df_list[[i]])[c(1,6:8)] = c('country', 'null_votes', 'blanco_votes', 'total_votes')
  df_list[[i]] = df_list[[i]] %>% pivot_wider(names_from = siglas_lista, values_from = votos)
}

# Presidential 2013
par_13 = df_list[[1]]
par_13 = add_column(par_13, valid_votes = rowSums(par_13[7:17]), .after = 'total_votes')
par_13 = aggregate(c(par_13[4:18]), by = par_13[1], sum)
par_13 = main_function(par_13, 'ANR', 'MKP', 6, 'ANR')
par_13 = extra_cols(par_13, "Paraguay", "2013-04-21", 'Presidential')
par_13$country = c('Argentina', 'US', "Spain")

# Presidential 2018
par_18 = df_list[[2]]
par_18 = add_column(par_18, valid_votes = rowSums(par_18[7:16]), .after = 'total_votes')
par_18 = aggregate(c(par_18[4:17]), by = par_18[1], sum)
par_18 = main_function(par_18, "ANR", "UNAMONOS", 6, "ANR")
par_18 = extra_cols(par_18, 'Paraguay', "2018-04-22", "Presidential")
par_18$country = c("Argentina", "Brazil", "US", "Spain")

# Legislative 2013
parl_13 = df_list[[3]]
parl_13 = add_column(parl_13, valid_votes = rowSums(parl_13[7:25]), .after = 'total_votes')
parl_13 = aggregate(c(parl_13[4:26]), by = parl_13[1], sum)
parl_13 = main_function(parl_13, 'ANR', 'MICA', 6, 'ANR')
parl_13 = extra_cols(parl_13, "Paraguay", "2013-04-21", 'Legislative')
parl_13$country = c('Argentina', 'US', "Spain")

# Legislative 2018
parl_18 = df_list[[4]]
parl_18 = add_column(parl_18, valid_votes = rowSums(parl_18[7:35]), .after = 'total_votes')
parl_18 = aggregate(c(parl_18[4:36]), by = parl_18[1], sum)
parl_18 = main_function(parl_18, "ANR", "CN", 6, "ANR")
parl_18 = extra_cols(parl_18, 'Paraguay', "2018-04-22", "Legislative")
parl_18$country = c("Argentina", "Brazil", "US", "Spain")



#### Binding everything together ===============================================
# Choosing the df with most columns to start the binding process
parl_18 = add_column(parl_18, registered_voters = NA, .after = 'election_type')
parl_18 = add_column(parl_18, invalid_votes = NA, .after = 'valid_votes')
batch_3 = bind_rows(parl_18, parl_13, par_18, par_13, mol_19, mol_16, mol_14, cro, 
                    ecp_13, ecp_09, ecp_06, ec_13, ec_09, indp_19, ind_19, cz_10, cz_06,
                    czp_18, czp_13, polp_15, polp_10, polp_05, polp_00, pol_19, pol_15, 
                    pol_11, pol_07, pol_01, lat_18, lat_14, lat_11, lat_10, lat_06, lat_02)

# there are two columns total_voters and correct_votes(last two) where I was unsure
# about the meaning 
names(batch_3)[1] <- 'country_of_residence' 
batch_3 = add_column(batch_3, cor_iso3 = countrycode(batch_3$country_of_residence, 'country.name', 'iso3c'), .after = 'country_of_residence')
# Problems here for several countries -> either military base or non existent or old countries with no Iso3c or so
write.csv(batch_3,"batch_3.csv", row.names = FALSE)
