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
# Setting working directory 
setwd("C:/Users/lenovo/Documents/BSE/RA/Data/Data/EVP")

### Latvia --------------------------------------------------------------------
lat_02 <- read.csv("Latvia/leg_2002/lat_leg_02_1.csv")
lat_02_2 <- read.csv("Latvia/leg_2002/lat_leg_02_2.csv", header = F)

# lat 05 

# lat 10: 
# scraping // 
link = "https://www.cvk.lv/cgi-bin/wdbcgiw/base/komisijas2010.GalRezs10?nr1=1&nr2=10200"
bytes <- readLines(link)
#> Warning in readLines(url): incomplete final line found on 'https://
#> www.post.japanpost.jp/kitte_hagaki/stamp/kogata/index.php?p=4'
utf8 <- iconv(bytes, from="EUC-JP", to="UTF-8")  
html <- read_html(charToRaw(paste(utf8, collapse="\n")), encoding="UTF-8")
page = read_html(link)
lat_10 = html %>% html_nodes('table') %>% .[5] %>%
  html_table() %>% .[[1]]
html %>% html_nodes('table') %>% htmltab(which = 5)
lat_10 = html %>% html_nodes('table')

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
# 2011:
link = "https://wybory2011.pkw.gov.pl/wyn/140000/pl/149901.html#tabs-1"
page = read_html(link)
pol_l_2011 <- page %>% html_nodes('table') %>% .[2] %>%
  html_table() %>% .[[1]]
## that worked!! -> but it is only voter data 
link = 'https://wybory2011.pkw.gov.pl/rfl/pl/a37acdd2b4996b08222e85815b6da20a.html'
page = read_html(link)
# now there is a problem.. -> website is dynamic //

#d ChromeDriver 96.0.4664.45
rD <- rsDriver(browser = 'chrome', port = 1234L, chromever = "96.0.4664.45") # not working
driver <- remoteDriver()
driver$open()

## Legislative 2015

pol_15 = read_xlsx("Poland/Raw Data (Warsaw constituency, includes abroad).xlsx")
pol_15 = pol_15 %>% filter(Gmina == "Zagranica")


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
# makes no sense
pol_19$weird = gsub("Krolestwo ", "", pol_19$country)
pol_19$weird = gsub("koreanska")
countrycode(tolower(pol_19$country), 'polish', 'english', custom_dict = custom_dict)
## also not working // leave that polish election for last 


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

#### Indonesia =================================================================

#trying to scrape indonesia
link = "https://pemilu2019.kpu.go.id/#/dprri/hitung-suara/"
page = read_html(link)
indo = page %>% html_nodes('data-table')
# problematic as dynamic website
### Ecuador:--------------------------------------------------------------------
setwd("C:/Users/lenovo/Documents/BSE/RA/Data/Data/EVP/Ecuador/Leg. 2009 National MPs/USA + Canada")
my_files <- list.files(pattern = "\\.xlsx$")
df_list <- lapply(my_files, read_xlsx)
for (i in 1:2){
  df_list[[i]] <- df_list[[i]][-5]
}
ita_06 <- do.call(rbind, df_list)


# trying scraping: 
link = 'https://app01.cne.gob.ec/resultados2017/frmResultados.aspx'
page = read_html(link)
name = page %>% html_nodes('#tablaSuf , #tablaBlancosNulos , #tablaCandi , #datosmesashabilitadas' %>% html_text())
ec_table = page %>% html_nodes("#tablaSuf") %>%
  html_table(dec = ",", header = T) # %>% .[[1]]
data <- link %>%
  read_html() %>%
  html_nodes(xpath = '//*[(@id = "tablaCandi")]') %>%
  html_table()
data <- data[[1]]

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
custom_dict$romanian #names problematic 


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
mol_16[2:15] = lapply(mol_16[2:15], function(y) as.numeric(y))
names(mol_16) = iconv(names(mol_16), from = "UTF-8", to = 'ASCII//TRANSLIT')
mol_16 = aggregate(c(mol_16[2:15]), by = mol_16[1], sum)
# problems with encoding 
readr::guess_encoding("Moldova/pres_2016/1stRound. finalallt1prez20175034720_6383668.xlsx")


detect_str_enc(names(mol_16))
detect_str_enc(mol_16$country)
#encoding problematic

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






