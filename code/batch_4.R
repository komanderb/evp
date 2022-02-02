### batch 4

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
#### Bolvia ====================================================================
bo_19 = read.csv('bolivia_2019.csv', encoding = 'UTF-8')
bo_19 = bo_19[-1]
bo_19$country = trimws(bo_19$country)
bo_19$X0 = gsub("\n", "", bo_19$X0)
bo_19$X0 = gsub("-", "", bo_19$X0)
#bo_19$X0 = gsub("(.*?),.*", "\\1", bo_19$X0)
bo_19 = bo_19 %>% pivot_wider(names_from = X0, values_from = X1)
bo_19 = bo_19[-c(2,8,18)]

bo_19 = bo_19 %>% filter(!country %in% c('total', 'total_national', 'total_abroad'))
## maybe that is interesting for a different purpose
names(bo_19)[2:6] = c('registered_voters', "total_votes", "valid_votes", "blanco_votes", "null_votes")

bo_19[2:15] = lapply(bo_19[2:15], function(y) as.numeric(gsub("\\.", "", y)))
bo_19 = main_function(bo_19, "COMUNIDAD CIUDADANA (CC)", "PARTIDO DE ACCION NACIONAL BOLIVIANO (PANBOL)", 7, "MOVIMIENTO AL SOCIALISMOINSTRUMENTO POLÍTICO POR LA SOBERANÍA DE LOS PUEBLOS (MASIPSP)")
bo_19 = extra_cols(bo_19, "Bolivia", "2019-10-20", "General")
bo_19$country = countryname(bo_19$country)

#### Indonesia =================================================================
ind_14 = read.csv('indo_14.csv', encoding = 'UTF-8')
ind_14 = ind_14[-c(1,2)]
ind_14 = row_to_names(ind_14, 1)
ind_14 = ind_14 %>% filter(!(NEGARA == "NEGARA"))
ind_14[2:3] = lapply(ind_14[2:3], function(y) gsub("	suara", "", y))
ind_14[2:3] = lapply(ind_14[2:3], function(y) gsub("	Suara", "", y))
ind_14[2:3] = lapply(ind_14[2:3], function(y) gsub("\\.", "", y))
ind_14$NEGARA = gsub(".*, ", "", ind_14$NEGARA)
ind_14[2:3] = lapply(ind_14[2:3], function(y) as.numeric(y))
indo_names = names(ind_14)
ind_14 = aggregate(c(ind_14[2:3]), by = ind_14[1], sum)
indo_names[1] = 'country'
indo_names = gsub("\n.*", "", indo_names)
names(ind_14) = indo_names
ind_14$country = trimws(ind_14$country)
ind_14$country = gsub("\t", " ", ind_14$country)
ind_14$weird = countrycode(tolower(ind_14$country), 'indonesian', 'english', custom_dict = custom_dict)
indonesian  = "afghanistan, as, azerbaizan, bosnia, chili, ethiopia, filiphina, inggris, libya, myanmar, republik ceko, serawak, malaysia, sri langka, urbekiztan, usa, vatican"
indonesian = unlist(strsplit(indonesian, ", "))
for (i in indonesian)
  print(i)
english = c(
  "Afghanistan",
  "USA",
 "Azerbaijan",
 "bosnia",
 "Chile",
 "ethiopia",
 "philippines",
 "UK",
 "libya",
 "myanmar",
 "Czech Republic",
 "Malaysia",
 "malaysia",
 "Sri Lanka",
 "Uzbekistan",
 "usa",
 "vatican")

for (i in seq_along(english)){
  ind_14$weird[tolower(ind_14$country) == indonesian[i]] = english[i]
}
ind_14$country = countryname(ind_14$weird)
ind_14 = ind_14[-4]
#in this case serawak belongs to malaysia
ind_14 = aggregate(c(ind_14[2:3]), by = ind_14[1], sum)
names(ind_14) = indo_names
ind_14 = add_column(ind_14, valid_votes = rowSums(ind_14[2:3]), .after = 'country')
ind_14 = main_function(ind_14, "Prabowo Subianto", "Joko Widodo", 3, "Joko Widodo")
ind_14 = extra_cols(ind_14, 'Indonesia', '2014-07-09', 'Presidential')

## Romania -------------------------------------------------------------------#

#ro_leg_00 = read.csv('ro_leg_00_votes.csv', encoding = 'UTF-8')

setwd("C:/Users/lenovo/Documents/BSE/RA/Data/Data/EVP/Romania/scraped")

files = list.files(pattern = "\\csv$")
gen = files[grep("general", files)]
votes = files[-grep("general", files)]

df_list = lapply(gen, read.csv, encoding = 'UTF-8')


for (i in seq_along(df_list)){
  df_list[[i]] = df_list[[i]] %>% filter(!(Mediul.de.desfasurare.al.procesului.electoral == 'Numerotarea SV' | Mediul.de.desfasurare.al.procesului.electoral== 'Numerotarea NSV'))
}
for (i in seq_along(df_list)){
  df_list[[i]] = df_list[[i]][-c(1,3,4)]
}


for (i in seq_along(df_list)){
  df_list[[i]] = df_list[[i]] %>% pivot_wider(
    names_from = Mediul.de.desfasurare.al.procesului.electoral.2, 
    values_from =  Mediul.de.desfasurare.al.procesului.electoral.3,
  )
}



df_list_2 = lapply(votes, read.csv, encoding = 'UTF-8')

for (i in seq_along(df_list_2)){
  df_list_2[[i]] = df_list_2[[i]][-c(1,6)]
}
for (i in 1:3){
  df_list_2[[i]] = df_list_2[[i]][-c(2)]
  df_list_2[[i]] = df_list_2[[i]] %>%
    pivot_wider(names_from = Initiale, values_from = Voturi)
} 

for (i in 4:5){
  df_list_2[[i]] = df_list_2[[i]][-c(3)]
} 
## messed that up: 
for (i in 4:5){
  df_list_2[[i]] = df_list_2[[i]] %>%
    pivot_wider(names_from = Candidat, values_from = Voturi)
} 

## rename columnns
for (i in seq_along(df_list)){
  names(df_list[[i]])[2:6] = c('registered_voters', 'total_votes', 'percent', 'valid_votes', 'null_votes')
}

# get rid of unnec. columns -> like registered voters 4/5 is always zero 1/5 
# is always 100 
# filter countries with more than 0 total_votes

for (i in seq_along(df_list)){
  df_list[[i]] = df_list[[i]][-c(2,4)]
  df_list[[i]][2:4] = lapply(df_list[[i]][2:4], function(y) as.numeric(y))
  df_list[[i]] = df_list[[i]] %>% filter(total_votes > 0)
}


for (i in seq_along(df_list_2)){
  df_list_2[[i]][is.na(df_list_2[[i]])] = 0
}

## Romania Legislative 2000: 
ro_leg_00g = df_list[[1]]
ro_leg_00v = df_list_2[[1]]

ro_leg_00 = left_join(ro_leg_00g, ro_leg_00v)

ro_leg_00$pol_stat = gsub(" -.*", "", ro_leg_00$pol_stat)
# some problems
ro_leg_00$pol_stat = gsub("UNITE-.*", "UNITE", ro_leg_00$pol_stat)
ro_leg_00$pol_stat = gsub("EMIRATE ", "EMIRATELE ", ro_leg_00$pol_stat) 
ro_leg_00$pol_stat = gsub("AUSTRALIA .*", "AUSTRALIA", ro_leg_00$pol_stat)
names_ro = names(ro_leg_00)
ro_leg_00 = aggregate(c(ro_leg_00[2:59]), by = ro_leg_00[1], sum)
names_ro[1] = 'country'
names(ro_leg_00) = names_ro

ro_leg_00 = main_function(ro_leg_00, 'CDR 2000', 'UCAR', 5, 'PDSR-PUR-PSDR')
ro_leg_00 = extra_cols(ro_leg_00, 'Romania', '2000-11-26', 'Legislative')
ro_leg_00$weird = countrycode(tolower(ro_leg_00$country), 'romanian', 'english', custom_dict = custom_dict)

romanian = "bosnia-hertegovina, croatia, elvetia, federatia rusa, franta, iugoslavia, kuwait, luxembourg, macedonia, malayezia, marea britanie, moldova, olanda, s.u.a."
romanian = unlist(str_split(romanian, ", "))
for (i in romanian){
  print(i)
}


english = c( "Bosnia and Herzegovina",
             "Croatia",
             "Switzerland",
             "Russian Federation",
             "France",
             "Yugoslavia",
             "Kuwait",
             "Luxembourg",
             "macedonia",
             "Malaysia",
             "UK",
             "moldova",
             "Netherlands",
             "US")

for (i in seq_along(english)){
  ro_leg_00$weird[tolower(ro_leg_00$country) == romanian[i]] = english[i]
}
  
ro_leg_00$country = countryname(ro_leg_00$weird)
ro_leg_00 = ro_leg_00[-120]


# Legislative 2004
ro_leg_04 = left_join(df_list[[2]], df_list_2[[2]])
ro_leg_04$pol_stat = gsub("-.*", "", ro_leg_04$pol_stat)
ro_leg_04$pol_stat = gsub("R.A.EGIPT", "R.A. EGIPT", ro_leg_04$pol_stat)
names_ro = names(ro_leg_04)
ro_leg_04 = aggregate(c(ro_leg_04[2:53]), by = ro_leg_04[1], sum)
names_ro[1] = 'country'
names(ro_leg_04) = names_ro
names_ro

ro_leg_04 = main_function(ro_leg_04,  "D.A. PNL-PD", "AC-MACEDONENI", 5, "PSD+PUR")
ro_leg_04 = extra_cols(ro_leg_04, 'Romania', '2004-11-28', 'Legislative')
countrycode(tolower(ro_leg_04$country), 'romanian', 'english', custom_dict = custom_dict)
# too many unknown names // 
# can't be asked 

## Presidential 2000
ro_pres00 = left_join(df_list[[4]], df_list_2[[4]])
ro_pres00 = ro_pres00 %>% filter(valid_votes > 0)
# upsi its actually 2000 
ro_pres00$pol_stat = gsub(" -.*", "", ro_pres00$pol_stat)
# some problems
ro_pres00 $pol_stat = gsub("UNITE-.*", "UNITE", ro_pres00$pol_stat)
ro_pres00$pol_stat = gsub("EMIRATE ", "EMIRATELE ", ro_pres00$pol_stat) 
ro_pres00$pol_stat = gsub("AUSTRALIA .*", "AUSTRALIA", ro_pres00$pol_stat)
names_ro = names(ro_pres00)
ro_pres00 = aggregate(c(ro_pres00[2:16]), by = ro_pres00[1], sum)
names(ro_pres00) = names_ro
names(ro_pres00)[1] = 'country'
ro_pres00 = main_function(ro_pres00, "CONSTANTIN MUGUREL ISARESCU", "ION SASU", 5, "ION ILIESCU")
ro_pres00 = extra_cols(ro_pres00, 'Romania', '2000-11-26', 'Presidential')
ro_pres00$weird = countrycode(tolower(ro_pres00$country), 'romanian', 'english', custom_dict = custom_dict)
romanian = "bosnia-hertegovina, croatia, elvetia, federatia rusa, franta, iugoslavia, kuwait, luxembourg, macedonia, malayezia, marea britanie, moldova, olanda, s.u.a."
romanian = unlist(str_split(romanian, ", "))

for (i in seq_along(english)){
  ro_pres00$weird[tolower(ro_pres00$country) == romanian[i]] = english[i]
}

ro_pres00$country = countryname(ro_pres00$weird)
ro_pres00 = ro_pres00[-34]

## Presidential 2004 
# here most annoying ever happened -> forgot to delete one column thus I will have to redo it again
ro_votes = read.csv("Romania/scraped/ro_pres_04_votes.csv", encoding = 'UTF-8')
ro_votes= ro_votes[-c(1,4,6)]
ro_votes =  ro_votes %>%
  pivot_wider(names_from = Candidat, values_from = Voturi)
ro_votes[is.na(ro_votes)] = 0
ro_pres04 = left_join(df_list[[5]], ro_votes)
ro_pres04$pol_stat = gsub("-.*", "", ro_pres04$pol_stat)
ro_pres04$pol_stat = gsub("R.A.EGIPT", "R.A. EGIPT", ro_pres04$pol_stat)
names_ro = names(ro_pres04)
ro_pres04 = aggregate(c(ro_pres04[2:16]), by = ro_pres04[1], sum)
names_ro[1] = 'country'
names(ro_pres04) = names_ro
names_ro

ro_pres04 = main_function(ro_pres04, "TRAIAN BASESCU", "AUREL RADULESCU", 5, "ADRIAN NASTASE")
ro_pres04 = extra_cols(ro_pres04, 'Romania', '2004-11-28', 'Presidential')
#names missing 

#Legislative 2008
ro_leg08 = left_join(df_list[[3]], df_list_2[[3]])
# can't be asked due to city names // 
ro_dic = ro_votes[1]
ro_dic = ro_dic %>% separate(pol_stat, c('country', 'city'), sep = "-")
ro_dic[1:2] = tolower(ro_dic[1:2])

countrycode(toupper(ro_leg08$pol_stat), 'city', 'country', custom_dict = ro_dic)
ro_dic = ro_dic %>% filter(!(duplicated(city)))
# also not doing that // 


##
#
## Pres 2019
# file is actually in the wrong folder and the wrong name
ro_16 = read.csv("Romania/leg_16/Abroad only. RAW. pv_SR_PRSD_FINAL (1).csv", encoding = "UTF-8", sep = ";")
ro_16 = ro_16[-c(1,2,4:9, 10, 12:14, 17, 18)]
names(ro_16)[1:4] = c('country', 'total_votes', 'valid_votes', 'null_votes')
ro_16 = aggregate(c(ro_16[2:18]), by = ro_16[1], sum)

names(ro_16)[5:18] = c("Klaus-Werner IOHANNIS", "Theodor PALEOLOGU", 'Ilie-Dan BARNA',
                       'Hunor KELEMEN', 'Vasilica-Viorica DANCILA', 'Catalin-Sorin IVAN', 'Ninel PEIA',
                       'Sebastian-Constantin POPESCU', 'John-Ion BANU', 'Mircea DIACONU', 
                       'Bogdan STANOEVICI', 'Ramona-Ioana BRUYNSEELS', 'Viorel CATARAMA', 
                       'Alexandru CUMPANASU')
ro_16 = main_function(ro_16, "Klaus-Werner IOHANNIS", "Alexandru CUMPANASU", 5, "Klaus-Werner IOHANNIS")
ro_16 = extra_cols(ro_16, 'Romania', "2019-11-10", "Presidential")
ro_16$weird = countrycode(tolower(ro_16$country), 'romanian', 'english', custom_dict = custom_dict)
romanian = "bosnia si hertegovina, croatia, elvetia, federatia rusa, franta, malaezia, olanda, palestina, regatul thailandei, regatul unit al marii britanii si irlandei de nord, republica ceha, republica coreea, republica indonezia, republica mali, republica populara democrata coreeana, republica singapore, sua, sultanatul oman"
romanian = unlist(strsplit(romanian, ", "))
for (i in romanian)
  print(i)
english = c(
   "Bosnia and Herzegovina",
   "Croatia",
   "Switzerland",
   "Russian Federation",
   "France",
   "Malaysia",
   "Netherlands",
   "Palestine",
   "Thailand",
   "United Kingdom",
  "Czech Republic",
   "Republic of Korea",
   "Republic of Indonesia",
   "Republic of Mali",
   "Democratic People's Republic of Korea",
   "Republic of Singapore",
   "USA",
   "Oman"
)

for (i in seq_along(english)){
  ro_16$weird[tolower(ro_16$country) == romanian[i]] = english[i]
}
ro_16$country =  countryname(ro_16$weird)
ro_16 = ro_16[-38]


#### Bulgaria -----------------------------------------------------------------
## 2009 wasn't working so its below (hopefully)

# Leg 2013

bu_cities = read.delim("C:/Users/lenovo/Downloads/cities.txt", sep = ";", header = F)
bu_cities = bu_cities[-c(1,4:6)]

bu_cities$V3 = countryname(bu_cities$V3)
bu_cities[93,1] = 323300093
buleg_13 = read.delim("C:/Users/lenovo/Downloads/pe2013_pe_votes.txt", sep = ";", header = F)

buleg_13 = buleg_13 %>% filter(V2 >= 320100001)

buleg_13$V2 = countrycode(buleg_13$V2, "V2", "V3", custom_dict = bu_cities)

bu_parties = read.delim("New/Bulgaria/leg_2013/party_dic.txt", sep = ";", header = F)
number = c()
for (i in c(3:72)){
  if (i %% 2 != 0){
    number = append(number, buleg_13[1,i])
  }
}

parties = countrycode(number, 'V2', 'V3', custom_dict = bu_parties)

number = c()
for (i in c(3:72)){
  if (i %% 2 != 0){
    number = append(number, i)
  }
}

buleg_13 = buleg_13[-c(number)]
parties = trimws(parties)
names(buleg_13)[3:37] = parties
buleg_13 = buleg_13[-1]
buleg_13 = aggregate(c(buleg_13[2:36]), by = buleg_13[1], sum)


bu_gen = read.delim("New/Bulgaria/leg_2013/pe2013_pe_protocols.txt", sep = ";", header = F)
bu_gen = bu_gen %>% filter(V2 >= 320100001)

bu_gen = bu_gen[c(2:16)]

bu_gen$V2 = countrycode(bu_gen$V2, "V2", "V3", custom_dict = bu_cities)
bu_gen = aggregate(c(bu_gen[2:15]), by = bu_gen[1], sum)
# I can't really make sense of any of this -> in the read me not clear which is which

buleg_13 = renamer(buleg_13, 1)
buleg_13 = add_column(buleg_13, valid_votes = rowSums(buleg_13[2:36]), .after = 'country')
# once more iinstert the naes

names(buleg_13)[3:37] = parties

buleg_13 = main_function(buleg_13, "PE New Alternative", "PP United People's Party", 3, "PE GERB")
buleg_13 = extra_cols(buleg_13, "Bulgaria", "2013-05-12", "Legislative")

# Legislative 2009

buleg_09 = read.delim("New/Bulgaria/Leg. 2009 (usabel)/pe2009_partyvotes.txt", sep = ";", header = F)
buleg_09 = buleg_09 %>% filter(V1 >= 320100001)


countrycode(buleg_09$V1, "V2", "V3", custom_dict = bu_cities)
## not working so trying with 2013
## also not working --> leave it for now //#


# none of the presidentials are working // 








