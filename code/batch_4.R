### batch 4
#install.packages('maps')
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

##### Romania ==================================================================

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
ro_leg_04$weird = countrycode(tolower(ro_leg_04$country), 'romanian', 'english', custom_dict = custom_dict)
romanian = "anglia, bosnia, bosnia si hertegovina, confederatia elvetiana, federatia rusa, kuwait, macedonia, malaezia, marea britanie, r.a. egipt, r.f.germana, r.f.nigeria, r.i.iran, r.p.d. coreeana, r.s.vietnam, regatul arabiei saudite, regatul belgiei, regatul danemarcei, regatul tarilor de jos, rep.indonezia, republica africa de sud, republica algeriana, republica angola, republica argentina, republica armenia, republica austria, republica azerbaidjan, republica belarus, republica bulgaria, republica ceha, republica chile, republica cipru, republica columbia, republica coreea, republica croatia, republica cuba, republica elena, republica filipine, republica finlanda, republica franceza, republica georgia, republica india, republica irak, republica irlanda, republica italiana, republica libaneza, republica populara chineza, serbia si muntenegru, sua, uzbeckistan"
romanian = unlist(str_split(romanian, ", "))
write.table(as.data.frame(romanian), file = "ro_04_countries.txt", sep = "\t", row.names = F, col.names = F)
#reload translated names //
another_dic = read.delim("ro_04_countries.txt", sep = "\t", header = F)
english = another_dic$V1

for (i in seq_along(romanian)){
  ro_leg_04$weird[tolower(ro_leg_04$country) == romanian[i]] = english[i]
}
# save as I will need this for presidential
another_dic = ro_leg_04[c(1,108)]
ro_leg_04$country = countryname(ro_leg_04$weird)
ro_leg_04 = ro_leg_04[-108]

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
ro_pres04$weird = countrycode(ro_pres04$country, 'country', 'weird', custom_dict = another_dic)
ro_pres04$country = countryname(ro_pres04$weird)
ro_pres04 = ro_pres04[-34]

#Legislative 2008
ro_leg08 = left_join(df_list[[3]], df_list_2[[3]])
# can't be asked due to city names // 
ro_dic = ro_votes[1]
ro_dic = ro_dic %>% separate(pol_stat, c('country', 'city'), sep = "-")
ro_dic[1:2] = tolower(ro_dic[1:2])

countrycode(toupper(ro_leg08$pol_stat), 'city', 'country', custom_dict = ro_dic)
ro_dic = ro_dic %>% filter(!(duplicated(city)))
ro_leg08$pol_stat
# tried a few things, here is a new approach //
geocode(ro_leg08$pol_stat, output = 'more')$country
data(world.cities)
try = world.cities %>% filter(pop  > 100000)
try = try %>% filter(!duplicated(name))
ro_leg08$weird = countrycode(ro_leg08$pol_stat, 'name', 'country.etc', custom_dict = try)

# this is also not really working because some countries are in english while others are in romanian //
ro_dic = ro_leg08[c(1,30)]
ro_dic = ro_dic %>% filter(!is.na(weird))

romania = 'Alcazar de San Juan, Algarve, Alger, Alicante, Amman, Anvers, Ashgabat, Atena, Bagdad, Beirut, Belfast-Ballymena, Belgrad, Berna, Bruxelles, Budapesta, Carolina de Nord, Castellon de la Plan, Cernauti, Ciudad Real, Copenhaga, Cosalda, Damasc, Djakarta, Erevan, Florenta, Genova, Gyula, Haga, Hanoi, Havana, Hong Kong, Ieraklio, Ierusalim, Kuweit, Larose, Limassol, Lisabona, Londra, Londra 5, Luxemburg, Marsilia, Milano, Mineapolis, Moscova, Munchen, Napoli, New Delhi, Nisa, Odessa, Orange County, Palma de Mallorca, Phenian, Podgorita, Praga, Riad, Roma, Roquetas de mar, Rostov pe Don, Salonic, San Marino, Sankt Petersburg, Santiago de Chile, Santiago de Composte, Seul, Taskent, Teheran, Tel Aviv, Torino, Torring, Treviso, Valetta, Varset, Varsovia, Viena'
romania = unlist(str_split(romania, ", "))
romania = as.data.frame(romania)
write.table(romania, file = "roleg08_cities.txt", sep = "\t", row.names = F, col.names = F)
# then I threw all the above cities into google translate and now reload them to see if they match 
romania_2 = read.delim("roleg08_cities.txt", sep = "\t", header = F)
romania$english = romania_2$V1
romania$weird = countrycode(romania$english, 'name', 'country.etc', custom_dict = try)
not_translated = "Alcazar de San Juan, Algarve, Alicante, Amman, Ashgabat, Beirut, Belfast-Ballymena, Castellon de la Plan, Cernauti, Ciudad Real, Cosalda, Gyula, Hague, Hanoi, Heraklion, Hong Kong, Kuwait, Larose, Limassol, Luxembourg, New Delhi, Nisa, North Carolina, Odessa, Orange County, Palma de Mallorca, Phenian, Podgorita, Roquetas de Mar, Rostov on Don, San Marino, Santiago de Chile, Santiago de Compostela, St. Petersburg, Tel Aviv, Torring, Treviso, Valletta, Varset"
not_translated = unlist(str_split(not_translated, ", "))
not_translated = as.data.frame(not_translated)
write.table(not_translated, file = "roleg08_cities2.txt", sep = "\t", row.names = F, col.names = F)
romania = romania %>% filter(!is.na(weird))
names(romania)[1] = "pol_stat"
ro_dic = rbind(ro_dic, romania)


romania_2 = read.delim("roleg08_cities2.txt", sep = "\t", header = F)
not_translated$weird = romania_2$V1
romania$weird[is.na(romania$weird)] = countrycode(romania$english[is.na(romania$weird)], 'not_translated', 'weird', custom_dict = not_translated)
# messed something up here // -> like really messed up / but if I'm smart I should be able to get it back
# mismatched London (and potentially more but not as far as I can see)
romania$weird[romania$english == 'London'] = 'UK'
# just need romanian names now
romania = romania[c(1,3)]
names(romania)[1] = 'pol_stat'
ro_dic = rbind(ro_dic, romania)
ro_leg08$weird = countrycode(ro_leg08$pol_stat, 'pol_stat', 'weird', custom_dict = ro_dic)
ro_leg08$pol_stat = countryname(ro_leg08$weird)
names(ro_leg08)[1] = 'country'
rom_names = names(ro_leg08)
ro_leg08= ro_leg08[-30]
ro_leg08 = aggregate(c(ro_leg08[2:29]), by = ro_leg08[1], sum)

names(ro_leg08) = rom_names[-30]
ro_leg08 = main_function(ro_leg08, 'PNL', 'U-Polonezi', 5, 'PSD+PC')
ro_leg08 = extra_cols(ro_leg08, 'Romania', "2008-11-30", "Legislative")

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

# Now properly legislative 2016
# had some problems with the file so basically just copy pasted abroad results into
# new file 
rol16 = read.csv("Romania/leg_16/modified_leg16.csv", encoding = "UTF-8")
# no dictionary for the additonal variables
# remove empty columns
allmisscols <- sapply(rol16, function(x) all(is.na(x) | x == '' ))
rol16 = rol16[!allmisscols]
# remove unneccarsy columns
rol16 = rol16[-c(1,3:6, 44)]
rol16 = aggregate(c(rol16[2:38]), by = rol16[1], sum)
#first the countries and then wait until better idea for the missing columns//
rol16$weird = countrycode(tolower(rol16$X.U.021A.ara), 'romanian', 'english', custom_dict = custom_dict)
romanian = "bosnia si hertegovina, croatia, elvetia, franta, hong kong, kenia, macedonia, malaezia, olanda, palestina, republica ceha, statele unite, tailanda"
romanian = unlist(str_split(romanian, ", "))
english = c("Bosnia and Herzegovina", "Croatia", "Switzerland", "France",
            "Hong Kong", "Kenya", "Macedonia", "Malaysia", "Netherlands", "Palestine",
            "Czechia", "US", "Thailand")

for (i in seq_along(romanian)){
  rol16$weird[tolower(rol16$X.U.021A.ara) == romanian[i]] = english[i]
}
rol16$X.U.021A.ara = countryname(rol16$weird)
rol16 = rol16[-39]
detect_str_enc("UNIUNEA.ELENA.DIN.ROMÂNIA")
## no modifying names //
names(rol16) =  iconv(names(rol16), from = "ISO-8859-13", to = 'ASCII//TRANSLIT')
names(rol16) <- gsub("[.]*$|[.]*(?=[.])", "",names(rol16), perl = TRUE)
names(rol16) <- gsub("\\.", " ", names(rol16))
names(rol16)[1] = "country"
rol16 = main_function(rol16, "PARTIDUL NA U 021A IONAL LIBERAL", "UNIUNEA DEMOCRATA TURCA DIN ROMANIA", 15, "PARTIDUL SOCIAL DEMOCRAT")
rol16 = extra_cols(rol16, 'Romania', "2016-12-11", "Legislative")
rol16 = rol16[-c(6:8, 10:14, 17)]
names(rol16)[5:8] = c('registered_voters', 'total_votes', 'valid_votes', 'blanco_votes')

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
#' not working 
#' but for each section code the first 4 characters uniquely identify a country
buleg_09$V1 = substr(buleg_09$V1, 1, 4)
buleg_09 = buleg_09[-20]
buleg_09 = aggregate(c(buleg_09[2:19]), by = buleg_09[1], sum)
# but then no party dictionary??
bu_dic = buleg_09[1]
write.table(bu_dic, file = "bu09_countrydic.txt", sep = "\t", row.names = F, col.names = F)
bu_dic = read.delim("bu09_countrydic.txt", sep = ";", header = F)
bu_dic = bu_dic[-2]
bu_dic$V3 = trimws(bu_dic$V3)
buleg_09$V1 =  countrycode(buleg_09$V1, "V1", "V3", custom_dict = bu_dic)
buleg_09 = add_column(buleg_09, valid_votes = rowSums(buleg_09[2:19]), .after = "V1")
## there is data missing, as valid votes from this data are not equal to valid votes from website 
# it is off by 5: Website (153534), Data: 153529
party_identifyer = (colSums(buleg_09[3:20]) / 153529)
party_identifyer * 100#

# will be using english google translation -> hopefully helpful for EVP party dictionary
# basically I'm comparing the column sums divided by valid votes with total
#results on the webpage and then insert the party names accordingly


bulgarian_parties = c("Order, Law and Justice", "PE LEADER", "COAT OF ARMS", 
                      "MRF Movement for Rights and Freedoms", "ATTACK party",
                      "COALITION FOR BULGARIA", "UNION OF PATRIOTIC FORCES DEFENSE",
                      "NMSS", "BULGARIAN LEFT COALITION", "Liberal Alternative and Peace Party (PLAM)",
                      "PP GREENS", "SOCIAL DEMOCRATS", "P.P. THE OTHER BULGARIA",
                      "UNION OF BULGARIAN PATRIOTS (UPS)", "National Movement for the Salvation of the Fatherland",
                      "Bulgarian National Union - ND", "The Blue Coalition", "For the Motherland - DGI-NL")
# of course it ended up being 18 parties in the first place anyways -> but now at least sure abou the order 

names(buleg_09)[3:20] = bulgarian_parties
buleg_09 = renamer(buleg_09,1)
buleg_09 = main_function(buleg_09, "Order, Law and Justice", "For the Motherland - DGI-NL", 3, "COAT OF ARMS")
buleg_09 = extra_cols(buleg_09, "Bulgaria", "2009-07-05", "Legislative")


# Legislative 2005

bu_05 = read.csv("bulgaria_leg_2005.csv", encoding = 'UTF-8')
bu_05$country = trimws(bu_05$country)
bu_countries = unique(bu_05$country)
for (i in bu_countries)
  print(i)
# after google translate //
bu_english = c(
   "Australia",
   "Austria",
   "Albania",
   "Algeria",
   "Angola",
   "Argentina",
   "Afghanistan",
   "Belarus",
   "Belgium",
   "Bosnia and Herzegovina",
   "United Kingdom",
   "Venezuela",
   "Georgia",
   "Greece",
   "Denmark",
   "Egypt",
   "Zimbabwe",
   "Israel",
   "Iraq",
   "Iran",
   "Ireland",
   "Iceland",
   "Spain",
   "Italy",
   "Yemen",
   "Jordan",
   "Kazakhstan",
   "Canada",
   "Qatar",
   "Cyprus",
   "China",
   "Korea",
   "Kosovo",
   "Kuwait",
   "Libya",
   "Lebanon",
   "Macedonia",
   "Malta",
   "Morocco",
   "Mexico",
   "Moldova",
   "Nigeria",
   "Netherlands",
   "New Zealand",
   "Norway",
   "UAE",
   "Poland",
   "Portugal",
   "Romania",
   "Russia",
   "USA",
   "Singapore",
   "Syria",
   "Slovakia",
   "Slovenia",
   "Sudan",
   "Serbia and Montenegro",
   "Tunisia",
   "Turkey",
   "Uzbekistan",
   "Ukraine",
   "Hungary",
   "Finland",
   "Germany",
   "France",
   "Croatia",
   "Czech Republic",
   "Chile",
   "Switzerland",
   "Sweden",
   "South Africa",
   "Japan"
)

bu_05_dic = as_data_frame(cbind(bu_countries, bu_english))
# seems to be correct //
bu_values = unique(bu_05$X1)
for (i in bu_values)
  print(i)
bu_en = c(
  "Number of voters on the main voter list:",
  "Number of voters in the supplementary voter list:",
  "Number of voters according to the signatures on the voter lists:",
  "Number of ballots found in the ballot boxes:",
  "Number of invalid votes:",
  "Number of actual votes:",
  "11. DISTRIBUTION OF VOTES ON CANDIDATE LISTS",
  "Number and title of the party or coalition or the names of the independent candidates",
  "\" FEDERATION OF FREE BUSINESS - UNION BULGARIA \"",
  "MOVEMENT\" FORWARD BULGARIA \"",
  "\" COALITION FOR BULGARIA - BSP, PBS, PD - SOCIAL DEMOCRATS, DSH, P. \"ROMA \", KPB, BZNS - AL. STAMBOLIYSKI, ZPB \"",
  "GRANITE",
  "\" CHAMBER OF EXPERTS \"",
  "SIMEON II NATIONAL MOVEMENT (NMSS)",
  "COALITION \" DIGNIFIED BULGARIA \"",
  "\" DEMOCRATS FOR STRONG BULGARIA \"(DSB)",
  "NK \" LONG LIVE BULGARIA! \"",
  "PD \" EUROROMA \"",
  "ROSE COALITION (BSD, NDP and OBT PARTIES)",
  "UNION OF FREE DEMOCRATS, AUA - PEOPLE'S UNION, VMRO - BULGARIAN NATIONAL MOVEMENT - COALITION \" BULGARIAN PEOPLE'S UNION \"",
  "THE NEW TIME",
  "ATTACK COALITION",
  "FAGO",
  "\" UNITED PARTY OF PENSIONERS IN BULGARIA \"(OPPB)",
  "MRF\" MOVEMENT FOR RIGHTS AND FREEDOMS \"",
  "BULGARIAN CHRISTIAN COALITION (BHC)",
  "UDF - UDF, DEMOCRATIC PARTY, MOVEMENT \" GERGYOVDEN \", BZNS NS - BZNS, DROM",
  "NZP \" NIKOLA PETKOV \"",
  "FREE DEMOCRATS PARTY - PSD",
  "\" HOMELAND \""
)

bu_05_dic_2 = as_data_frame(cbind(bu_values, bu_en))
# translating the relevant features
bu_05$country = countrycode(bu_05$country, 'bu_countries', 'bu_english', custom_dict = bu_05_dic)
bu_05$X1 =  countrycode(bu_05$X1, 'bu_values', 'bu_en', custom_dict = bu_05_dic_2)

# this worked actually good 

bu_05 = bu_05[-c(1,3,4)]
drop_vector = c("11. DISTRIBUTION OF VOTES ON CANDIDATE LISTS", "Number and title of the party or coalition or the names of the independent candidates")
bu_05 = bu_05 %>% filter(!(X1 %in% drop_vector))
bu_05$X2 = as.numeric(bu_05$X2)
# this will already sum up by country
bu_05 = bu_05 %>% pivot_wider(names_from = X1, values_from = X2, values_fn = sum)

## the first 4 numerical columns are identical (besides the  0)
bu_05 = bu_05[-c(3:5)]
names(bu_05)[2:4] = c('total_votes', 'invalid_votes', 'valid_votes')
# also I'm annoyed by quotes so I will drop them
names(bu_05) = gsub("\"", "", names(bu_05))
names(bu_05) = trimws(names(bu_05))
str(bu_05)

bu_05 = main_function(bu_05, "FEDERATION OF FREE BUSINESS - UNION BULGARIA", "HOMELAND", 5, "COALITION FOR BULGARIA - BSP, PBS, PD - SOCIAL DEMOCRATS, DSH, P. ROMA , KPB, BZNS - AL. STAMBOLIYSKI, ZPB")
bu_05 = extra_cols(bu_05, 'Bulgaria', "2005-06-25", "Legislative")
bu_05$country = countryname(bu_05$country)

## Presidential 2011
bu_11 = read.csv('bulgaria_pres_2011.csv', encoding = 'UTF-8')
bu_11_dic = read.csv('bu_pres_2011_dic.csv', encoding = 'UTF-8')
names(bu_11_dic)[2] = 'party_number'
bu_11$party = countrycode(bu_11$X.U.2116, 'party_number', 'Candidate', custom_dict = bu_11_dic)
bu_11 = bu_11[-c(1,3,4,6)]
names(bu_11)[2] = 'votes'
bu_11 = bu_11 %>% pivot_wider(names_from = party, values_from = votes)
bu_11$country = countryname(bu_11$country)
bu_11 = add_column(bu_11, valid_votes = rowSums(bu_11[2:19]), .after = 'country')
bu_11 = main_function(bu_11, 'Kuneva-Hristov Initiative Committee', 'Yosifov-Dimitrov Initiative Committee', 
                      3, 'PP GERB')
bu_11 = extra_cols(bu_11, 'Bulgaria', '2011-10-23', 'Presidential')

#### Peru ----------------------------------------------------------------------

# Legislative 2006:
setwd("C:/Users/lenovo/Documents/BSE/RA/Data/Data/EVP/Peru/leg_06/named_files")
my_files <- list.files(pattern = "\\.csv$")
df_list <- lapply(my_files, read.csv, encoding = "UTF-8")
names(df_list) = gsub("\\.csv$", "", my_files)
for (i in seq_along(df_list)){
  df_list[[i]]$country = names(df_list)[i]
  df_list[[i]] = df_list[[i]][-c(3,4)]
  names(df_list[[i]])[1:2] = c('party', 'votes')
}
# problem is that some party names differ a bit in their spelling across countries
# but no worries as I just use the spelling of first table for every table as 
# structure is the same

party_vector = df_list[[1]][,1]
for (i in seq_along(df_list))
  df_list[[i]][1] = party_vector


perl_06 = do.call(rbind, df_list)
rownames(perl_06) = NULL
perl_06$votes = gsub("\\.", "", perl_06$votes)
perl_06$votes = gsub("\\,", "", perl_06$votes)
perl_06$votes = as.numeric(perl_06$votes)
perl_06$party = trimws(perl_06$party)
perl_06 = perl_06 %>% pivot_wider(names_from = party, values_from = votes)
perl_06 = perl_06[-30]
names(perl_06)[26:30] = c('valid_votes', 'blanco_votes', 'null_votes', 'total_votes',
                          'registered_voters')

names(perl_06) = iconv(names(perl_06), from = "UTF-8", to = 'ASCII//TRANSLIT')
str(perl_06)
perl_06 = main_function(perl_06, 'PARTIDO SOCIALISTA', 'Y SE LLAMA PERU', 7, 'UNION POR EL PERU')
perl_06 = extra_cols(perl_06, 'Peru', "2006-04-09", 'Legislative')
perl_06$weird = countryname(perl_06$country)

peru = unlist(str_split("Guayana, Netherland Antilles, Philipines", ", "))
english = c("Guyana", "Netherlands Antilles","Philippines")
for (i in seq_along(peru)){
  perl_06$weird[perl_06$country == peru[i]] = english[i]
}

perl_06$country = countryname(perl_06$weird)
perl_06 = perl_06[-60]

## Presidential 2006 first round //
setwd("C:/Users/lenovo/Documents/BSE/RA/Data/Data/EVP/Peru/pres_06/named_files")
my_files <- list.files(pattern = "\\.csv$")
df_list <- lapply(my_files, read.csv, encoding = "UTF-8")
names(df_list) = gsub("\\.csv$", "", my_files)
for (i in seq_along(df_list)){
  df_list[[i]]$country = names(df_list)[i]
  df_list[[i]] = df_list[[i]][-c(3,4)]
  names(df_list[[i]])[1:2] = c('party', 'votes')
}

party_vector = df_list[[1]][,1]
for (i in seq_along(df_list))
  df_list[[i]][1] = party_vector
per_06 = do.call(rbind, df_list)
rownames(perl_06) = NULL
per_06$votes = gsub("\\.", "", per_06$votes)
per_06$votes = gsub("\\,", "", per_06$votes)
per_06$votes = as.numeric(per_06$votes)
per_06$party = trimws(per_06$party)
per_06 = per_06 %>% pivot_wider(names_from = party, values_from = votes)
names(per_06) = iconv(names(per_06), from = "UTF-8", to = 'ASCII//TRANSLIT')
per_06 = per_06[-26]
names(per_06)[22:26] = c('valid_votes', 'blanco_votes', 'null_votes', 'total_votes',
                          'registered_voters')
per_06 = main_function(per_06, 'PARTIDO SOCIALISTA', 'Y SE LLAMA PERU', 7, 'UNION POR EL PERU')
per_06 = extra_cols(per_06, 'Peru', "2006-04-09", 'Presidential')
per_06$weird = countryname(per_06$country)
peru = unlist(str_split("Guayana, Philipines", ", "))
english = c("Guyana","Philippines")
for (i in seq_along(peru)){
  per_06$weird[per_06$country == peru[i]] = english[i]
}
per_06$country = countryname(per_06$weird)
per_06 = perl_06[-52]

## Presidential 2016

setwd("C:/Users/lenovo/Documents/BSE/RA/Data/Data/EVP/Peru/pres_16")
my_files = list.files(pattern = ".csv")
df_list <- lapply(my_files, read.csv, encoding = "UTF-8")
names(df_list) = gsub("\\.csv$", "", my_files)
for (i in seq_along(df_list)){
  df_list[[i]]$country = names(df_list)[i]
  df_list[[i]] = df_list[[i]][-c(3,4)]
  names(df_list[[i]])[1:2] = c('party', 'votes')
}

per_16 = do.call(rbind, df_list)
#### Colombia ==================================================================

#Legislative 2002
# this is so bad! 
setwd("C:/Users/lenovo/Documents/BSE/RA/Data/Data/EVP/Colombia/Colombia 2002 Camara")

my_files <- list.files(pattern = "\\.xlsx$")

df_list <- lapply(my_files, read_xlsx, range = cell_rows(8:35), trim = T)
names(df_list) <- gsub("\\.xlsx$", "", my_files)
'%ni%' <- Negate('%in%')
list_ind = c()
for (i in seq_along(df_list)){
  if ("Departamento" %ni% names(df_list[[i]])){
    list_ind = append(list_ind, i)
    
  }
}
for (i in  list_ind){
  df_list[[i]] = row_to_names(df_list[[i]], 1)
}

## dies bit weird order
df_list[[57]] = row_to_names(df_list[[57]], 1)


for(i in seq_along(df_list))
  df_list[[i]]$country = names(df_list)[i]


colleg_02 =  do.call(rbind, df_list)
colleg_02 = colleg_02[-c(1:3)]
colleg_02 = colleg_02 %>% filter(!(is.na(Candidato)))
colleg_02 = colleg_02 %>% pivot_wider(names_from = Candidato, values_from = Votos)
colleg_02[2:28] = lapply(colleg_02[2:28], function(y) as.numeric(gsub("\\.", "", y)))
colleg_02 = add_column(colleg_02, valid_votes = rowSums(colleg_02[2:26]), .after = 'country')
colleg_02 = colleg_02[-c(28,29)]
names(colleg_02) = iconv(names(colleg_02), from = "UTF-8", to = 'ASCII//TRANSLIT')
str(colleg_02)
colleg_02$weird= countryname(colleg_02$country)
colombian = "Antilas Holondesas, Dom. Rep., Guayana, Malysia"
colombian = unlist(str_split(colombian, ", "))
english = c("Netherlands Antilles", "Dominican Republic", "Guyana",  "Malaysia")
for (i in seq_along(colombian)){
  colleg_02$weird[colleg_02$country == colombian[i]] = english[i]
}
colleg_02$country = countryname(colleg_02$weird)
colleg_02 = colleg_02[-28]

# who tf won this election?
colleg_02 = main_function(colleg_02, 'RAFAEL DE JESUS CASTELLAR', 'ALVARO DE JESUS ZULETA CORTES', 3, ) 


## Legislative 2006
# file structure is super weird -> needs some extra attention
setwd("C:/Users/lenovo/Documents/BSE/RA/Data/Data/EVP/Colombia/leg_06/Source Files")
my_files = list.files(pattern = "\\.xlsx$")
df_list <- lapply(my_files, read_xlsx, range = cell_rows(16:23))
df_list = df_list[-9]
my_files = my_files[-9]
names(df_list) <- gsub("\\.xlsx$", "", my_files)
for (i in seq_along(df_list))
  df_list[[i]]$country = names(df_list)[i]
#also germany is fucked
df_list = df_list[-19]
col_06 = do.call(rbind, df_list)
# thats a start // 
my_files = list.files(pattern = "\\.xlsx$")
df_list <- lapply(my_files, read_xlsx, skip = 24)
df_list = df_list[-c(9,20)]
my_files = my_files[-c(9,20)]
names(df_list) <- gsub("\\.xlsx$", "", my_files)
for (i in seq_along(df_list))
  df_list[[i]]$country = names(df_list)[i]
col_06_votes = do.call(rbind, df_list)


rownames(col_06) = NULL
rownames(col_06_votes) = NULL


col_06 = col_06[-c(4,8)]
col_06= col_06 %>% filter(!(is.na(...1)))
col_06 = col_06 %>% pivot_wider(names_from = c(...1, ...3, ...6), values_from = c(...2, ...5, ...7))
## a bit confusing in terms of the names but we only want these columns:

col_06 = col_06[c(1:4, 10:12)]
names(col_06)[2:7] = c('registered_voters', 'total_votes', 'valid_votes', 'blanco_votes',
                       'null_votes', 'unmarked_votes')
col_06$blanco_votes  = gsub("—", "0", col_06$blanco_votes)
col_06[is.na(col_06)] = '0'
col_06[2:7] = lapply(col_06[2:7], function(y) as.numeric(gsub(",", "", y)))

# turning back to votes:

col_06_votes = col_06_votes[-c(1,3,5)]
col_06_votes = col_06_votes %>% filter(!(is.na(VOTOS)))
drop_vector = c("VOTOS EN BLANCO .......", "VOTOS NULOS ..........",
                "VOTOS NO MARCADOS ....", "TOTAL VOTOS ...........",
                "TOTAL VOTOS SUFRAGADOS ...")
col_06_votes = col_06_votes %>% filter(!(`PARTIDO O MOVIMIENTO POLITICO` %in%  drop_vector))
col_06_votes = col_06_votes %>% pivot_wider(names_from = `PARTIDO O MOVIMIENTO POLITICO`, values_from = VOTOS)
col_06_votes[is.na(col_06_votes)] = '0'
col_06_votes[2:33] = lapply(col_06_votes[2:33], function(y) as.numeric(gsub(",", "", y)))
col_06 = left_join(col_06, col_06_votes)
## Canada and Germany missing - will have to put in values by Hand

ger_party = c(24, 52, 7, 2, 4, 2, 44, 6, 1, 2, 0, 0,0, 1, 1, 0, 1, 2, 0, 1,0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,0)
ger_rest = c('Germany', NA, NA, NA, 32, NA, NA)
col_06 = rbind(col_06, c(ger_rest, ger_party))
col_06$valid_votes[col_06$country == 'Germany'] = sum(ger_party)
can_party = c(227, 153, 171, 33, 115, 8, 124, 19, 3, 3, 0, 2, 5, 3, 14, 6, 4, 7, 4, 2, 18, 2, 5, 11, 4, 3, 2, 2, 4, 2, 1, 0)
sum(can_party)
can_rest = c('Canada', NA, NA, 957, 114, NA, NA)
col_06 = rbind(col_06, c(can_rest, can_party))
col_06[2:39] = lapply(col_06[2:39], function(y) as.numeric(y))
col_06 = main_function(col_06, "PARTIDO SOCIAL DE UNIDAD NACIONAL", "MOVIMIENTO CIVICO INDEPENDIENTE", 8, "PARTIDO LIBERAL COLOMBIANO")
col_06 = extra_cols(col_06, 'Colombia', "2006-03-12", 'Legislative')
col_06$weird = countryname(col_06$country)
col_06$weird[col_06$country == "Dom. Rep."] = "Dominican Republic"
col_06$country = col_06$weird
col_06 = col_06[-77]



## Legislative 2010
setwd("C:/Users/lenovo/Documents/BSE/RA/Data/Data/EVP/Colombia/ocr_files")

my_files <- list.files(pattern = "\\.xls$")

df_list <- lapply(my_files, read_xls)
names(df_list) <- gsub("\\.xls$", "", my_files)
for(i in seq_along(df_list))
  df_list[[i]]$country = names(df_list)[i]

col_10 =  do.call(rbind, df_list)
row.names(col_10) <- NULL
col_10 = col_10[c(2,3,8)]
col_10 = col_10 %>% pivot_wider(names_from = NOMBRE, values_from = VOTACION, values_fill = 0)
col_10$weird = countryname(col_10$country)
colombian =  c("curazao", "UK (London+Dublin)")
english = c("Curaçao", 'UK')
for (i in seq_along(colombian)){
  col_10$weird[col_10$country == colombian[i]] = english[i]
}
col_10$country = countryname(col_10$weird)
col_10 = col_10[-17]
col_10 = main_function(col_10, "PARTIDO LIBERAL COLOMBIANO", "MOVIMIENTO INDEPENDIENTE DE RENOVACION ABSOLUTA MIRA", 6, "PARTIDO SOCIAL DE UNIDAD NACIONAL PARTIDO DE LA U")
names(col_10)[2:5] = c("blanco_votes", 'valid_votes', 'null_votes', 'unmarked_votes')
col_10 = extra_cols(col_10, 'Colombia', "2010-03-14", "Legislative")



## Czech Republic //
cz_02 = read_xlsx("Czechia/ps2002_zahranici (1)/PST4p_zahranici.xlsx")
cz_parties = read_xlsx("Czechia/ps2002_zahranici (1)/PSRKL.xlsx")
cz_codes = read_xlsx("Czechia/ps2002_zahranici (1)/ps2002_rzvo.xlsx")
cz_codes = cz_codes[c(2,12)]
cz_02 = left_join(cz_02, cz_codes)

# okay now we have what we want: drop all the hlasy#
# also general informatin on votes is not4 available in the excel but on the website //

cz_02 = cz_02[-c(11:46)]
#drop all the other columns // 

cz_02 = cz_02[-c(1:5, 10:14)]
#forgot one
cz_02 = cz_02[-2]

cz_02 = cz_02 %>% pivot_wider(names_from = KSTRANA, values_from = POC_HLASU, values_fill = 0)

cz_02 = aggregate(c(cz_02[3:25]), by = cz_02[2], sum)
cz_02 = renamer(cz_02,1)
cz_02 = add_column(cz_02, valid_votes = rowSums(cz_02[2:24]), .after = "country")
names(cz_02) = gsub("X", "", names(cz_02))
view(cz_parties)
cz_parties = cz_parties[c(1,6)]
# just gonna use party abbreviation //
cz_parties$ZKRATKAK8 = iconv(cz_parties$ZKRATKAK8, from = "UTF-8", to = 'ASCII//TRANSLIT')
names(cz_parties) = c('KSTRANA', 'party')
names(cz_02)[3:25] = countrycode(as.numeric(names(cz_02)[3:25]), 'KSTRANA', 'party', custom_dict = cz_parties) 
cz_02 = main_function(cz_02, "CSSD", "VPB", 3, "CSSD")
cz_02 = extra_cols(cz_02, "Czechia", "2002-06-14", "Legislative")
cz_02$country = countryname(cz_02$country)
#### merging -------------------------------------------------------------------
## adding columns for symmetry
ro_leg_00 = add_column(ro_leg_00, registered_voters = NA, .after = 'election_type')
ro_leg_00 = add_column(ro_leg_00, blanco_votes = NA, .after = 'null_votes')
ro_leg_00 = add_column(ro_leg_00, invalid_votes = NA, .after = 'blanco_votes')

batch_4 = bind_rows(ro_leg_00, bo_19, ind_14, ro_leg_04, ro_pres00, ro_pres04,
                    ro_leg08, ro_16, rol16, buleg_13, buleg_09, col_10)



write.csv(batch_4, 'batch_4.csv', row.names = F)




































