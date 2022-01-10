# EVP Batch 2
# Loading libraries ------------------------------------------------------------
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
# Setting working directory 
setwd("C:/Users/lenovo/Documents/BSE/RA/Data/Data/EVP")
### Honduras ===================================================================
# for the first two elections csv were created in python by scraping from pdf 
# 2001
Hon_01 <- read.csv('Hon_2001.csv', dec = ",")
Hon_01 <- Hon_01[7,]
Hon_01 <- Hon_01[-12]
names(Hon_01)[c(1, 7:11)] <- c('country', 'valid_votes', 'null_votes', 'blanco_votes', 'total_votes', 'registered_voters')
Hon_01$country <- 'US'
Hon_01[2:11] <- lapply(Hon_01[2:11], function(y) as.numeric(sub('.', '', y, fixed = TRUE)))
Hon_01 <- slim_fitter(Hon_01, 'PL', 'PN')
Hon_01_nw <- Hon_01 %>% filter(party == 'PN')
Hon_01_nw <- Hon_01_nw[c(1,7,8)]
Hon_01$ph <- looper(Hon_01)
Hon_01 <- my_pivoter(Hon_01)
Hon_01 <- column_sorter(Hon_01, 7,16)
Hon_01 <- national_winner(Hon_01, Hon_01_nw)
Hon_01 <- add_column(Hon_01, country_of_origin = 'Honduras', .after = 'country')
Hon_01 <- add_column(Hon_01, election_date = as.Date("2001-11-25"), .after = 'country_of_origin')
Hon_01 <- add_column(Hon_01, election_type = 'Presidential', .after =  "election_date")

# 2005
Hon_05 <- read.csv("Hon_2005.csv", dec = ",")
Hon_05 <- Hon_05[-c(1:2)]
Hon_05 <- Hon_05[20,]
names(Hon_05) <- gsub("[.]*$|[.]*(?=[.])", "",names(Hon_05), perl = TRUE)
names(Hon_05) <- gsub("\\.", " ", names(Hon_05))
names(Hon_05)[c(1,2,8:11)] <- c('country', 'registered_voters', 'valid_votes', 'null_votes', 'blanco_votes', 'total_votes')
Hon_05 <- Hon_05[-c(12)]
Hon_05[2:11] <- lapply(Hon_05[2:11], function(y) as.numeric(sub('.', '', y, fixed = TRUE)))
Hon_05 <- slim_fitter(Hon_05, "Partido Innovacion y Unidad SD", "Partido Democrata Cristiano")
Hon_05_nw <- Hon_05 %>% filter(party == "Partido Liberal de Honduras")
Hon_05_nw <- Hon_05_nw[c(1,7,8)]
Hon_05$ph <- looper(Hon_05)
Hon_05 <- my_pivoter(Hon_05)
Hon_05 <- column_sorter(Hon_05, 7, 16)
Hon_05 <- national_winner(Hon_05, Hon_05_nw)
Hon_05$country <- 'US'
Hon_05 <- add_column(Hon_05, country_of_origin = 'Honduras', .after = 'country')
Hon_05 <- add_column(Hon_05, election_date = as.Date("2005-11-27"), .after = 'country_of_origin')
Hon_05 <- add_column(Hon_05, election_type = 'Presidential', .after =  "election_date")

## 2009
Hon_09 <- read.csv("Honduras/RepDetalleNivelPresidencial.xls - RepDetalleNivelPresidencial.csv", sep = ',')
Hon_09 <- row_to_names(Hon_09,1)
Hon_09 <- Hon_09[324,]
Hon_09 <- Hon_09[-c(2,5)]
names(Hon_09)[c(1,7)] <- c('country', 'valid_votes')
Hon_09$country <- 'US'
str(Hon_09)
Hon_09[2:7] <- lapply(Hon_09[2:7], function(y) as.numeric(sub(",", "", y, fixed = TRUE)))
Hon_09 <- slim_fitter(Hon_09, 'PARTIDO NACIONAL DE HONDURAS', 'PARTIDO UNIFICACION DEMOCRATICA')
Hon_09_nw <- Hon_09 %>% filter(party == 'PARTIDO NACIONAL DE HONDURAS')
Hon_09_nw <- Hon_09_nw[-c(2)]
Hon_09$ph <- looper(Hon_09)
Hon_09 <- my_pivoter(Hon_09)
Hon_09 <- column_sorter(Hon_09, 3,12)
Hon_09 <- national_winner(Hon_09, Hon_09_nw)
Hon_09 <- add_column(Hon_09, country_of_origin = 'Honduras', .after = 'country')
Hon_09 <- add_column(Hon_09, election_date = as.Date("2009-11-29"), .after = 'country_of_origin')
Hon_09 <- add_column(Hon_09, election_type = 'Presidential', .after =  "election_date")


# 2013:
Hon_13 <- read_xlsx("C:/Users/lenovo/Documents/BSE/RA/Data/Data/EVP/Honduras/Honduras Pres. 2013 Elections.xlsx")
Hon_13 <- row_to_names(Hon_13, 8)
index = is.na(colnames(Hon_13))
Hon_13 <- Hon_13[!(index)]
Hon_13 <- Hon_13 %>% filter(Departamento == 'ESTADOS UNIDOS')
str(Hon_13)
Hon_13[2:13] <- lapply(Hon_13[2:13], function(y) as.numeric(as.character(y)))
number = c(2:9)
number = c(5:12)
rowSums(Hon_13[,number], na.rm = T) # same as total / will be valid_votes? 

Hon_13 <- Hon_13[-c(13)]
names(Hon_13)[1] <- 'country'
Hon_13 <- slim_fitter(Hon_13, 'PARTIDO DEMOCRATA CRISTIANO', 'PARTIDO NACIONAL DE HONDURAS')
Hon_13_nw <- Hon_13 %>% filter(party == "PARTIDO NACIONAL DE HONDURAS")
Hon_13_nw <- Hon_13_nw[c(1,5,6)]
Hon_13$ph <- looper(Hon_13)
Hon_13 <- my_pivoter(Hon_13)
Hon_13 <- column_sorter(Hon_13, 5, 20)
Hon_13 <- national_winner(Hon_13, Hon_13_nw)
names(Hon_13)[2:4] <- c('blanco_votes', 'null_votes', 'valid_votes')
Hon_13$country <- 'US'
Hon_13 <- add_column(Hon_13, country_of_origin = 'Honduras', .after = 'country')
Hon_13 <- add_column(Hon_13, election_date = as.Date("2013-11-24"), .after = 'country_of_origin')
Hon_13 <- add_column(Hon_13, election_type = 'Presidential', .after =  "election_date")


## 2017
Hon_17 <- read_xlsx("Honduras/Honduras Pres. 2017. USA Polling Stations Results.xlsx")
Hon_17 <- row_to_names(Hon_17, 1)
Hon_17 <- Hon_17[c(8),]
Hon_17 <- Hon_17[-c(2)]
names(Hon_17)[c(1,11)] <- c('country', 'valid_votes')
Hon_17$country <- 'US'
Hon_17[2:10] <- lapply(Hon_17[2:10], function(y) as.numeric(as.character(y)))
Hon_17 <- slim_fitter(Hon_17, 'PARTIDO LIBERAL DE HONDURAS', 'PARTIDO VA MOVIMIENTO SOLIDARIO')
Hon_17$ph <- looper(Hon_17)
Hon_17_nw <- Hon_17 %>% filter(party == 'PARTIDO NACIONAL DE HONDURAS')
Hon_17_nw <- Hon_17_nw[c(1,3,4)]
Hon_17 <- my_pivoter(Hon_17)
Hon_17 <- column_sorter(Hon_17, 3, 20)
Hon_17 <- national_winner(Hon_17, Hon_17_nw)
Hon_17 <- add_column(Hon_17, country_of_origin = 'Honduras', .after = 'country')
Hon_17 <- add_column(Hon_17, election_date = as.Date("2017-11-26"), .after = 'country_of_origin')
Hon_17 <- add_column(Hon_17, election_type = 'Presidential', .after =  "election_date")


### France -------------------------------------------------------------------- 
fr_02 <- read.csv("France/France Pres. 2002 (1st Round).csv", sep = ";")
fr_02 <- row_to_names(fr_02, 2)
fr_02 <- fr_02[-c(135:137),]
fr_02 <- fr_02[-c(1,6)]

fr_02[2:20] <-  lapply(fr_02[2:20], function(y) as.numeric(sub(" ", "", y, fixed = TRUE)))
names(fr_02)[1:4] <- c('country', 'registered_voters', 'total_votes', 'valid_votes')
fr_02 <- slim_fitter(fr_02, 'MEGRET', 'BESANCENOT')
fr_02_nw <- fr_02 %>% filter(party == 'CHIRAC')
fr_02_nw <- fr_02_nw[c(1,5,6)]
fr_02$ph <- looper(fr_02)
fr_02 <- my_pivoter(fr_02)
fr_02 <- column_sorter(fr_02, 5, 36)
fr_02 <- national_winner(fr_02, fr_02_nw)
fr_02$weird <- countrycode(fr_02$country, "CoR_Original", "CoR_English", custom_dict = france_dic)
french <- 'BURKINA, CAP-VERT, CENTRAFRICAINE (REP.), CONGO (REP. DEMOCRAT.), DOMINICAINE (REP.), ETATS-UNIS, JERUSALEM - TER. PALESTINIENS, ROYAUME-UNI, SALVADOR, TRINITE-ET-TOBAGO, VIET NAM'
french <- unlist(strsplit(french, ", "))
#fr_02_election_date <- '2002-04-21'
english <- c( "Burkina Faso", "Cape Verde", "Central African Republic" ,"Congo - Kinshasa", "Dominican Republic", "US" , "Palestine", "UK", "El Salvador",  "Trinidad & Tobago", "Vietnam")
for (i in 1:11){
  fr_02$weird[fr_02$country == french[i]] <- english[i]
}

fr_02$country <- countryname(fr_02$weird)
fr_02 <- fr_02[-c(39)]
fr_02 <- add_column(fr_02, country_of_origin = 'France', .after = 'country')
fr_02 <- add_column(fr_02, election_date = as.Date("2002-04-21"), .after = 'country_of_origin')
fr_02 <- add_column(fr_02, election_type = 'Presidential', .after =  "election_date")

## Brazil 2002 Pres -----------------------------------------------------------
setwd("C:/Users/lenovo/Documents/BSE/RA/Data/Data/EVP/New/Brazil/pres_2002")
list_brazil <- list.files(pattern = "\\.csv$")
a = 1
for (i in list_brazil){
  assign(paste0('bra_', a), read.csv(i, skip = 7, header = F))
  a = a + 1
}
name_list <- c("CIRO FERREIRA GOMES", "JOSÉ MARIA DE ALMEIDA", "JOSÉ SERRA", 
               "LUIZ INACIO LULA DA SILVA", "RUI COSTA PIMENTA", "ANTHONY WILLIAM GAROTINHO MATHEUS DE OLIVEIRA")

bra_1 <- row_to_names(bra_1, 1)
bra_2 <- row_to_names(bra_2, 1)
bra_3 <- row_to_names(bra_3, 1)
bra_4 <-row_to_names(bra_4, 1)
bra_5 <- row_to_names(bra_5, 1)
bra_6 <-row_to_names(bra_6, 1)

names(bra_1)[4] <- name_list[1]
names(bra_2)[4] <- name_list[2]
names(bra_3)[4] <- name_list[3]
names(bra_4)[4] <- name_list[4]
names(bra_5)[4] <- name_list[5]
names(bra_6)[4] <- name_list[6]

bra_1 <- bra_1[c(1,4)]
bra_2 <- bra_2[c(1,4)]
bra_3 <- bra_3[-c(5,6)]
bra_4 <- bra_4[c(1,4)]
bra_5 <- bra_5[c(1,4)]
bra_6 <- bra_6[c(1,4)]

## 
bra_02 <- left_join(bra_3, bra_1)
bra_02 <- left_join(bra_02, bra_2)
bra_02 <- left_join(bra_02, bra_4)
bra_02 <- left_join(bra_02, bra_5)
bra_02 <- left_join(bra_02, bra_6)

bra_02[2:9] <- lapply(bra_02[2:9], function(y) as.numeric(sub(",", "", y, fixed = TRUE)))
bra_02[is.na(bra_02)] <- 0
names(bra_02)[1:3] <- c('country', 'registered_voters', 'valid_votes')
bra_02 <- slim_fitter(bra_02, "JOSÉ SERRA", 'ANTHONY WILLIAM GAROTINHO MATHEUS DE OLIVEIRA')
bra_02_nw <- bra_02 %>% filter(party == "LUIZ INACIO LULA DA SILVA")
bra_02_nw <- bra_02_nw[c(1,4,5)]
bra_02$ph <- looper(bra_02)
bra_02 <- my_pivoter(bra_02)
bra_02 <- column_sorter(bra_02, 4, 15)
bra_02 <- national_winner(bra_02, bra_02_nw)
countryname(bra_02$country)

custom_dict$portugese2 <- iconv(custom_dict$portugese, from = 'UTF-8', to = 'ASCII//TRANSLIT')
bra_02$weird <-countrycode(tolower(bra_02$country), 'portugese2', 'english', custom_dict = custom_dict)
brazil <- "checoslovaquia, cingapura, coreia, emirados arabes, guine bissau, hong kong, inglaterra, kenia"
brazil <- unlist(strsplit(brazil, ", "))
english <- c("Czechia", "Singapore", "South Korea", "United Arab Emirates", "Guinea-Bissau", 
             "Hong Kong", "UK", "Kenya")
for (i in 1:8){
  bra_02$weird[tolower(bra_02$country) == brazil[i]] <- english[i]
}

bra_02$country <- countryname(bra_02$weird)
bra_02 <- bra_02[-c(18)]
bra_02 <- add_column(bra_02, country_of_origin = 'Brazil', .after = 'country')
bra_02 <- add_column(bra_02, election_date = as.Date("2002-10-06"), .after = 'country_of_origin')
bra_02 <- add_column(bra_02, election_type = 'Presidential', .after =  "election_date")

### Colombia 02 ----------------------------------------------------------------
col_02 <- read_xls("Colombia/0000 Export Pres. 2002 Colombia.xls")
col_02 <- col_02 %>% filter(cod_dpto == 88)
col_02 <- col_02[-c(1:4)]
col_02_e <- col_02 %>% filter(cod_candidato %in% c(996, 997, 998, 999))
col_02_e <- col_02_e[c(1,3,6)]
col_02_e <- col_02_e %>%
  pivot_wider(names_from = Nom_candidato, values_from = votos)
col_02 <-  col_02 %>% filter(!(cod_candidato %in% c(996, 997, 998, 999)))
# now problematic as we have two candidates from the same party/ 
# as we are interested in parties -> I will aggregate
col_02 <- aggregate( col_02['votos'], by = c(col_02['nom_mpio'], col_02['movimiento']), sum)
names(col_02) <- c('country', 'party', 'votes')
col_02 <- party_sorter(col_02)
col_02$ph <- looper(col_02)
# in this case the party of the winner alvaro uribe 
#is under other parties - renaming that (as it is just one candidate 
#to Primero Colombia 
col_02$party[col_02$party == 'OTROS PARTIDOS O MOV.S'] <- 'PRIMERO COLOMBIA'
col_02_nw <- col_02 %>% filter(party == 'PRIMERO COLOMBIA')
col_02_nw <- col_02_nw[c(1:3)]
col_02 <- my_pivoter(col_02)
col_02 <- add_column(col_02, valid_votes = rowSums(col_02[,c(2:11)], na.rm = T), .after = "country")
col_02 <- column_sorter(col_02, 3, 22)
col_02 <- national_winner(col_02, col_02_nw)
names(col_02_e) <- c('country', 'blanco_votes', 'null_votes', 'invalid_votes', 'total_votes')
col_02 <- left_join(col_02, col_02_e)
custom_dict$spanish2 <- iconv(custom_dict$spanish, from = 'UTF-8', to = 'ASCII//TRANSLIT')
col_02$weird <- countrycode(tolower(col_02$country), 'spanish2', 'english', custom_dict = custom_dict)
colomb <- "antillas holandesas, checoslovaquia, china republica popular, espa/a, holanda, hungria republica popular, inglaterra, rumania republica social"
colomb <- unlist(strsplit(colomb, ", "))
english <- c('Netherlands Antilles', 'Czechia', 'China', 'Spain', 'Netherlands', 'Hungary', 'UK', 'Romania')
for (i in 1:8){
  col_02$weird[tolower(col_02$country) == colomb[i]] <- english[i]
}
col_02$country <- countryname(col_02$weird)
col_02 <- col_02[-c(29)]
col_02 <- extra_cols(col_02, 'Colombia', '2002-05-26', 'Presidential')

## Col 2014
setwd("Colombia/2014_leg/Colombia Leg 2014 (Camara)")
my_files <- list.files(pattern = "\\.xls$")
df_list <- lapply(my_files, read_xls, skip = 9)
names(df_list) <- gsub("\\.xls$", "", my_files)
for(i in seq_along(df_list))
  df_list[[i]]$country = names(df_list)[i]
col_14 <- do.call(rbind, df_list)
rownames(col_14) <- NULL
col_14 <- col_14 %>% filter(!(is.na(VOTOS)))
col_14_e <- col_14 %>% filter(CODIGO %in% c(996:998))
col_14 <- col_14 %>% filter(!(CODIGO %in% c(996:998)))
col_14 <- col_14[-c(1,2,5)]
# maybe that will be relevant: 
col_14 <- col_14[-c(2)]
col_14 <- col_14 %>% pivot_wider(names_from = PARTIDOS, values_from = VOTOS)
col_14[2:12] <- lapply(col_14[2:12], function(y) as.numeric(sub('.', '', y, fixed = TRUE)))
col_14[is.na(col_14)] <- 0
col_14 <- add_column(col_14, valid_votes = rowSums(col_14[2:12]), .after = 'country')
col_14_e <- col_14_e[-c(2,4:6)]
col_14_e <- col_14_e %>% pivot_wider(names_from = CODIGO, values_from = VOTOS)
names(col_14_e)[2:4] <- c('blanco_votes', 'null_votes', 'invalid_votes')
# again last one maybe wrong
# it says votes not marked //
col_14 <- left_join(col_14, col_14_e)
col_14[14:16] <- lapply(col_14[14:16], function(y) as.numeric(sub('.', '', y, fixed = TRUE)))
col_14 <- main_function(col_14, 'PARTIDO LIBERAL COLOMBIANO', 'MOVIMIENTO DE INCLUSIÃ?N Y OPORTUNIDADES', 6, 'PARTIDO DE LA U')
col_14 <- add_column(col_14, total_votes = rowSums(col_14[2:5]), .after = 'country')
col_14 <- extra_cols(col_14, 'Colombia', '2014-03-09', 'Legislative')
col_14$weird <- countryname(col_14$country)
colomb <- 'Dom Rep., Nicarargua'
colomb <- unlist(strsplit(colomb, ', '))
english <- c("Dominican Republic", "Nicaragua")
for (i in 1:2){
  col_14$weird[col_14$country == colomb[i]] <- english[i]
}
col_14$country <- countryname(col_14$weird)
col_14 <- col_14[-34]

### Italy ----------------------------------------------------------------------
setwd("C:/Users/lenovo/Documents/BSE/RA/Data/Data/EVP/Italy/Italy Leg. 2006 Sources/part_votes")
my_files <- list.files(pattern = "\\.csv$")
df_list <- lapply(my_files, read.csv, sep = ";")
for (i in 1:2){
  df_list[[i]] <- df_list[[i]][-5]
}
ita_06 <- do.call(rbind, df_list)
ita_06 <- ita_06[-4]
names(ita_06) <- c('country', 'party', 'votes')
ita_06 <- ita_06 %>% pivot_wider(names_from = party, values_from = votes)
ita_06[is.na(ita_06)] <- 0
setwd("C:/Users/lenovo/Documents/BSE/RA/Data/Data/EVP/Italy/Italy Leg. 2006 Sources")
## problem south american votes are missing // 

# 2013 
setwd("C:/Users/lenovo/Documents/BSE/RA/Data/Data/EVP/Italy/Italy Leg. 2013 Sources")
my_files <- list.files(pattern = "\\.csv$")
df_list <- lapply(my_files, read.csv, sep = ";")
df_list = df_list[-4]
ita_13 <- read.csv("LISTE-Camera-24_02_2013-ESTERO-EUROPA-Nazione.csv")
for (i in 1:3){
  df_list[[i]] <- df_list[[i]][-5]
}
ita_13_2 <- do.call(rbind, df_list)
ita_13 <- rbind(ita_13, ita_13_2)
ita_13 <- ita_13[-4]
ita_13 <- ita_13 %>% pivot_wider(names_from = X, values_from = Liste.Gruppi)
ita_13[is.na(ita_13)] <- 0
ita_13 <- renamer(ita_13, 1)
ita_13 <- add_column(ita_13, valid_votes = rowSums(ita_13[2:14]), .after = 'country')
ita_13 <- main_function(ita_13, "IL POPOLO DELLA LIBERTA'", "INSIEME PER GLI ITALIANI", 3, "MOVIMENTO 5 STELLE BEPPEGRILLO.IT")
# next step is maybe problematic but throwing out countries without any valid votes
ita_13 <- extra_cols(ita_13, 'Italy', '2013-02-24', 'Legislative')
ita_13 <- ita_13 %>% filter(valid_votes > 0)
custom_dict$italian <- tolower(countrycode::codelist$cldr.name.it)
custom_dict$italian <- iconv(custom_dict$italian, from = 'UTF-8', to = 'ASCII//TRANSLIT')
ita_13$weird <- countrycode(tolower(ita_13$country), 'italian', 'english', custom_dict = custom_dict)
italy = "bosnia-erzegovina, circoscrizione autonoma, congo, costarica, 
federazione russa, kazakhstan, moldova, peru', repubblica ceca, repubblica democratica del congo, repubblica di corea, 
repubblica di macedonia, repubblica di serbia, repubblica popolare cinese, stati uniti d'america, sud africa"
italy = unlist(strsplit(italy, ', '))
italy = unlist(strsplit(italy, ','))
italy = trimws(italy)
english = c('Bosnia', 'Autonomous Region', "Congo - Brazzaville", "Costa Rica", 'Russia', "Kazakhstan",
            'Moldova', 'Peru', 'Czechia', "Congo - Kinshasa", 'South Korea', "North Macedonia", 'Serbia',
            'China', 'US', 'South Africa')
for (i in 1:16){
  ita_13$weird[tolower(ita_13$country) == italy[i]] <- english[i]
}

ita_13$country <- countryname(ita_13$weird)
ita_13$country[ita_13$weird == 'Autonomous Region'] <- "Autonomous Region"
ita_13 <- ita_13[-34]


### Czech Republic-------------------------------------------------------------
cz_17 <- read.csv("C:/Users/lenovo/Downloads/result (3).csv")
cz_17 <- cz_17[-c(1:4, 6, 8, 21:44)]
cz_17 <- cz_17 %>% filter(!(is.na(ZAHRANICI__KONTINENT__STAT__UCAST__OKRSKY_CELKEM)))
cz_17$country = countrycode(cz_17$ZAHRANICI__KONTINENT__STAT__ZKRATKA, 'iso2c', 'country.name')
cz_17$country[cz_17$ZAHRANICI__KONTINENT__STAT__ZKRATKA == 'KO'] <- 'Kosovo'
cz_17$country = countryname(cz_17$country)
#KO = Kosovo
cz_17 <- cz_17[-c(1:2)]
cz_17 <- cz_17[-c(1:3)]
names(cz_17)[1:9] <- c('registered_voters', 'total_votes', 'turnout', 
                       'also_total_votes', 'valid_votes', 'valid_votes_perc', 'party', 'votes', 'votes_share')
cz_17 <- cz_17[-c(3,4,6,9)]
cz_17_codes <- read.csv("Czechia/leg_2017/result (4).csv", encoding = 'UTF-8')
cz_17_codes <- cz_17_codes %>% filter(!duplicated(KRAJ__STRANA__KSTRANA))
cz_17_codes <- cz_17_codes[c(17,18)]
names(cz_17_codes) <- c('party_code', 'party_name')
write.csv(cz_17_codes, 'party_codes_cz_17.csv')
write.csv(cz_17, 'cz_leg_17.csv')
# continuing //
# using the countrycode function as it is quite convienient to transform values according to 
# a dictionary -> in this case party_code to party_name
cz_17$party <- countrycode(cz_17$party, 'party_code', 'party_name', custom_dict = cz_17_codes)
# converting characters so that they don't contain special items 
cz_17$party <- iconv(cz_17$party, from = 'UTF-8', to = 'ASCII//TRANSLIT')
# dropping countries with no votes -> but useful for participation
cz_17 <- cz_17 %>% filter(!is.na(party))
cz_17 <- cz_17 %>% pivot_wider(names_from = party, values_from = votes)
cz_17[is.na(cz_17)] <- 0
cz_17 <- cz_17 %>%
  select(country, everything())
cz_17 <- main_function(cz_17, 'Obcanska demokraticka strana', 'Dobra volba 2016', 5, 'ANO 2011')
cz_17 <- extra_cols(cz_17, 'Czechia', '2017-10-20', 'Legislative')

# 13 legislative
cz_13 <- read.csv("Czechia/leg_2013/result (4).csv")
cz_13 <- cz_13[-c(1:6, 8, 33:44)]
cz_13 <- cz_13 %>% filter(!(is.na(ZAHRANICI__KONTINENT__STAT__UCAST__OKRSKY_CELKEM)))
cz_13$country = countrycode(cz_13$ZAHRANICI__KONTINENT__STAT__ZKRATKA, 'iso2c', 'country.name')
cz_13$country[cz_13$ZAHRANICI__KONTINENT__STAT__ZKRATKA == 'KO'] <- 'Kosovo'
cz_13$country = countryname(cz_13$country)
#KO = Kosovo
cz_13 <- cz_13[-c(1:4)]
cz_13 <- cz_13[-c(10:21)]
names(cz_13)[1:9] <- c('registered_voters', 'total_votes', 'turnout', 
                       'also_total_votes', 'valid_votes', 'valid_votes_perc', 'party', 'votes', 'votes_share')
cz_13 <- cz_13 %>%
  select(country, everything())
# Turning to codes
cz_13_codes <- read.csv("Czechia/leg_2013/general_results.csv", encoding = 'UTF-8')
cz_13_codes <- cz_13_codes %>% filter(!duplicated(KRAJ__STRANA__KSTRANA))
cz_13_codes <- cz_13_codes[c(17,18)]
names(cz_13_codes) <- c('party_code', 'party_name')
cz_13_codes$party_name <- iconv(cz_13_codes$party_name, from = 'UTF-8', to = 'ASCII//TRANSLIT')
write.csv(cz_13_codes, 'party_codes_cz_13.csv')
write.csv(cz_13, 'cz_leg_13.csv')
cz_13$party <- countrycode(cz_13$party, 'party_code', 'party_name', custom_dict = cz_13_codes)
cz_13 <- cz_13[-c(4,5,7,10)]
cz_13 = cz_13 %>% pivot_wider(names_from = party, values_from = votes)
cz_13[is.na(cz_13)] <- 0
cz_13 <- main_function(cz_13, 'Ceska str.socialne demokrat.', 'LEV 21-Narodni socialiste', 5, 'Ceska str.socialne demokrat.')
cz_13 <- extra_cols(cz_13, 'Czechia', '2013-10-25','Legislative')

### Mozambique-----------------------------------------------------------------
# Legislative 04
mozl_04 <- read_xls("C:/Users/lenovo/Documents/BSE/RA/Data/Data/EVP/Mozambique/2004/legislativas_africa_austral.xls")
mozl2_04 <- read_xls("C:/Users/lenovo/Documents/BSE/RA/Data/Data/EVP/Mozambique/2004/legislativas_europa.xls")
df_list <- list(mozl_04, mozl2_04)
df_list <- lapply(df_list, function(x) row_to_names(x, 2))
df_list[[2]]$PDD <- NA
mozl_04 = do.call(rbind, df_list)
mozl_04 = mozl_04 %>% filter(!(País %in% c('Total', 'TOTAIS')))
mozl_04[2:10] <- lapply(mozl_04[2:10], function(y) as.numeric(y))
mozl_04 <- renamer(mozl_04, 1)

mozl_04 <- main_function(mozl_04, 'RENAMO-UE', 'PPD', 7, 'FRELIMO')

names(mozl_04)[2:6] <- c('registered_voters', 'valid_votes', 'blanco_votes', 'null_votes',
                         'total_votes')

mozl_04 <- extra_cols(mozl_04, "Mozambique", "2004-12-1", 'Legislative')
mozl_04$country <- iconv(mozl_04$country, from = 'UTF-8', to = 'ASCII//TRANSLIT')
mozl_04$weird <- countrycode(tolower(mozl_04$country), 'portugese2', 'english', custom_dict = custom_dict)
port <- "malawi, swazilandia, zimbabwe"
port <- unlist(strsplit(port, ", "))
english <- c("Malawi", "Eswatini", "Zimbabwe")
for (i in 1:3){
  mozl_04$weird[tolower(mozl_04$country) == port[i]] <- english[i]
}
mozl_04$country <- countryname(mozl_04$weird)

#Presidential 04
mozp_04 <- read_xls("Mozambique/2004/presidenciais_africa_austral.xls")
mozp2_04 <- read_xls("Mozambique/2004/presidenciais_europa.xls")
df_list <- list(mozp_04, mozp2_04)
df_list <- lapply(df_list, function(x) row_to_names(x, 2))
df_list <- lapply(df_list, function(x) renamer(x, 1))
mozp_04 = do.call(rbind, df_list)
mozp_04 = mozp_04 %>% filter(!(country %in% c('Total', 'TOTAIS')))
mozp_04[2:11] <- lapply(mozp_04[2:11], function(y) as.numeric(y))
mozp_04 <- main_function(mozp_04, 'RAUL DOMINGOS', 'CARLOS DOS REIS', 7, 'ARMANDO GUEBUZA')
names(mozp_04)[2:6] <- c('registered_voters', 'valid_votes', 'blanco_votes', 'null_votes',
                         'total_votes')
mozp_04 <- extra_cols(mozp_04, "Mozambique", "2004-12-1", 'Presidential')
mozp_04$country <- mozl_04$country

#Legislative 2009 
mozl_09 <- read_xlsx("Mozambique/2009/Mozambique Leg2009. RAW.xlsx", sheet = 3)
mozl_09 <- mozl_09[, which(colMeans(!is.na(mozl_09)) > 0.5)]
mozl_09 <- mozl_09[-48, -1]
mozl_09 <- aggregate(c(mozl_09[7:14]), by = mozl_09[1], sum)
mozl_09 <- renamer(mozl_09,1)
names(mozl_09) <- gsub("[.]*$|[.]*(?=[.])", "",names(mozl_09), perl = TRUE)
names(mozl_09) <- gsub("\\.", " ", names(mozl_09))
mozl_09 <- add_column(mozl_09, valid_votes = rowSums(mozl_09[3:7], na.rm = T), .after = "country")
mozl_09 <- main_function(mozl_09, 'FRELIMO', 'UM', 7, 'FRELIMO')
names(mozl_09)[3:6] <- c('registered_voters', 'blanco_votes', 'null_votes', 'total_votes')
mozl_09 <- extra_cols(mozl_09, "Mozambique", "2009-10-28", 'Legislative')
mozl_09$country <- mozl_04$country

# Presidential 2009
mozp_09 <- read_xlsx("Mozambique/2009/Mozambique Pres2009. RAW.xlsx", sheet = 2)
mozp_09 <- mozp_09[-c(94:96), -1]
mozp_09[5:7] <- lapply(mozp_09[5:7], function(y) as.numeric(y)) 
mozp_09 <- aggregate(c(mozp_09[4:11]), by = mozp_09[1], sum)
names(mozp_09)[3:5] <- c('FRELIMO', 'RENAMO', 'MDM')
mozp_09 <- renamer(mozp_09, 1)
mozp_09 <- main_function(mozp_09, 'FRELIMO', 'MDM', 7, 'FRELIMO')
names(mozp_09)[2:6] <- c('registered_voters', 'blanco_votes', 'null_votes', 'total_votes', 'valid_votes')
mozp_09 <- extra_cols(mozp_09, "Mozambique", "2009-10-28", 'Presidential')
mozp_09$country <- mozl_09$country

#Legislative 2014 
mozl_14 <- read.csv("Mozambique/2014/africa/Mesa a Mesa - AR.csv")
# weird warning -> removed -> one string in csv was missing // 
mozl2_14 <- read.csv("Mozambique/2014/europe/Mesa a Mesa - AR.csv", skip = 1)
df_list = list(mozl_14, mozl2_14)
mozl_14 = do.call(rbind, df_list)
mozl_14 = aggregate(c(mozl_14[7:18]), by = mozl_14[3], sum)
mozl_14 = renamer(mozl_14, 1)
mozl_14 = mozl_14[-13]
mozl_14 <- main_function(mozl_14, 'MDM', 'UE', 7, 'FRELIMO')
names(mozl_14)[2:6] <- c('registered_voters', 'valid_votes', 'null_votes' ,'blanco_votes',
                         'total_votes')
mozl_14 <- extra_cols(mozl_14, "Mozambique", "2014-10-15", 'Legislative')
mozl_14$country <- c("South Africa", "Germany", "Kenya", "Malawi", "Portugal",  
                     "Eswatini", "Tanzania", "Zambia","Zimbabwe")    

#Presidential 2014
mozp_14 <- read.csv("Mozambique/2014/africa/Mesa a Mesa - PR.csv")
mozp2_14 <- read.csv("Mozambique/2014/europe/Mesa a Mesa - PR.csv", skip = 1)
df_list = list(mozp_14, mozp2_14)
mozp_14 = do.call(rbind, df_list)
mozp_14 = aggregate(c(mozp_14[7:15]), by = mozp_14[3], sum)
mozp_14 = renamer(mozp_14, 1)
mozp_14 = mozp_14[-10]
names(mozp_14)[3:5] <- c('RENAMO', 'FRELIMO','MDM')
mozp_14 <- main_function(mozp_14, 'RENAMO', 'MDM', 7, 'FRELIMO')
names(mozp_14)[2:6] <- c('registered_voters', 'valid_votes', 'null_votes' ,'blanco_votes',
                         'total_votes')
mozp_14 = extra_cols(mozp_14, "Mozambique", "2014-10-15", 'Presidential')
mozp_14$country <- mozl_14$country

### Batch 2 Wedding ============================================================
# cz_17 is the longest
cz_17 = add_column(cz_17, null_votes = NA, .after = 'valid_votes')
cz_17 = add_column(cz_17, blanco_votes = NA, .after= 'null_votes')
final_df_2 = bind_rows(cz_17, cz_13, Hon_01, Hon_05, Hon_09, Hon_13, Hon_17, fr_02, bra_02, col_02, 
                       col_14, ita_13, mozl_04, mozp_04, mozl_09, mozp_09, mozl_14, mozp_14)
final_df_2 <- final_df_2[-c(63)]
names(final_df_2)[1] <- 'country_of_residence' 
final_df_2 = add_column(final_df_2, cor_iso3 = countrycode(final_df_2$country_of_residence, 'country.name', 'iso3c'), .after = 'country_of_residence')
# Problems here for: Autonomous Region, Kosovo, Netherlands Antilles
write.csv(final_df_2,"batch_2.csv", row.names = FALSE)
 