#_______________________________________________
#EVP Batch 1
#_____________________
### Installing relevant packages ==============================================
install.packages('tidyverse')
install.packages('ggthemes')
install.packages('tidyr')
install.packages('tibble', 'dplyr', 'readxl')
install.packages('janitor')
install.packages('countrycode')
install.packages('matrixStats')
install.packages('purr')
install.packages('gtools')
install.packages('stringi')
install.packages('plyr')
install.packages('rvest')
install.packages('pdftools')
install.packages("googledrive")
install.packages('XML')
install.packages('xml2')
install.packages("htmltab")
install.packages('RSelenium')
# loading 
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
library(htmltab)
library(RSelenium)
#library(purr)
### Setting working directory =================================================
setwd("C:/Users/lenovo/Documents/BSE/RA/Data/Data/EVP")

####France---------------------------------------------------------------------
## Presidential 2007 
france_names <- c('registered_voters', 'total_votes', 'valid_votes')
frPres_07 <- read.csv("France/Pres. 2007/France Pres. 2007 (1st Round).csv", sep = ";")
frPres_07 <- row_to_names(frPres_07, row_number = 4)
frPres_07 <- renamer(frPres_07, 1)
names(frPres_07)[4] <- "percentage"
frPres_07 <- frPres_07[-c(156:166),]
frPres_07[2:17] <- lapply(frPres_07[2:17], function(y) as.numeric(gsub(" ", "", y)))
# loose percentage 
frPres_07 <- slim_fitter(frPres_07, 'LCR', 'UMP')
# _nw will be used for national winner
frPres07_nw <- frPres_07 %>% filter(party == 'UMP')
frPres07_nw <- frPres07_nw[c(1,6,7)]
frPres_07$ph <- looper(frPres_07)
names(frPres_07)[c(2,3,5)] <- france_names
frPres_07 <- frPres_07[-4]
frPres_07 <- my_pivoter(frPres_07)
frPres_07 <- column_sorter(frPres_07, 5, 28)
frPres_07 <- national_winner(frPres_07, frPres07_nw)
# for names -> see countrynames file

## France Pres 2012 
frPres_12 <- read.csv("France/Pres. 2012/France Pres. 2012 (1st Round).csv", sep = ";")
frPres_12 <- row_to_names(frPres_12, row_number = 4)
frPres_12 <- renamer(frPres_12, 1)
frPres_12 <- frPres_12[-c(4,16)]
frPres_12 <- frPres_12[-c(163:164),]
# getting the numbers as numeric -> they are written with space for bigger than 1000
frPres_12[2:14] <- lapply(frPres_12[2:14], function(y) as.numeric(gsub(" ", "", y)))
frPres_12 <- slim_fitter(frPres_12, 'EELV', 'PS')
frPres_12$ph <- looper(frPres_12)
frPres_12_nw <- frPres_12 %>% filter(party == 'PS')
frPres_12_nw <- frPres_12_nw[c(1,5,6)]
names(frPres_12)[2:4] <- france_names
frPres_12 <- my_pivoter(frPres_12)
frPres_12 <- column_sorter(frPres_12, 5, 24)
frPres_12 <- national_winner(frPres_12, frPres_12_nw)
## countryname  file // 

## France Presendential 2017 
frPres_17 <- read_xlsx("France/Pres. 2017/France Pres. 2017 raw data.xlsx")
frPres_17 <- row_to_names(frPres_17, row_number = 1)
frPres_17 <- renamer(frPres_17, 1)
frPres_17 <- frPres_17[-c(1, 150),]
frPres_17 <- frPres_17[-c(14:25)]
# getting names in the right format 
names(frPres_17)[3:13] <- gsub("Somme de Mme ", "", names(frPres_17)[3:13])
names(frPres_17)[3:13] <- gsub("Somme de M. ", "", names(frPres_17)[3:13])
names(frPres_17)[3:13] <- gsub(" - TOTAL", "", names(frPres_17)[3:13])
names(frPres_17)[2] <- "valid_votes" ## Is this true or is it all votes?
frPres_17[2:13] <- lapply(frPres_17[2:13], function(y) as.numeric(as.character(y)))
frPres_17 <- slim_fitter(frPres_17, "Nicolas DUPONT-AIGNAN", 'François FILLON')
frPres_17$ph <- looper(frPres_17)
frPres_17_nw <- frPres_17 %>% filter(party == "Emmanuel MACRON")
frPres_17_nw <- frPres_17_nw[c(1,3,4)]
frPres_17 <- my_pivoter(frPres_17) 
frPres_17 <- column_sorter(frPres_17, 3, 24)
frPres_17 <- national_winner(frPres_17, frPres_17_nw)

## France Leg 2012
for (i in 1:11){
  assign(paste0("frLeg_12_", i), read_xls("France/Leg. 2012/France Leg. 2012 Raw Data.xls", sheet = i))
}
# manipulating every single df because they are all unique in their size
#(not very beautiful)
dic_1 <- frLeg_12_1[4, c(6:23)]
frLeg_12_1 <- frLeg_12_1[-c(3:8),]
dic_2 <- frLeg_12_2[27, c(6:17)]
frLeg_12_2 <- frLeg_12_2[-c(26:28),]
dic_3 <- frLeg_12_3[12, c(6:21)]
frLeg_12_3 <- frLeg_12_3[-c(11:14),]
dic_4 <- frLeg_12_4[5, c(6:21)]
frLeg_12_4 <- frLeg_12_4[-c(4:7),]
dic_5 <- frLeg_12_5[6, c(6:18)]
frLeg_12_5 <- frLeg_12_5[-c(5:8),]
dic_6 <- frLeg_12_6[3, c(6:26)]
frLeg_12_6 <- frLeg_12_6[-c(2:5),]
dic_7 <- frLeg_12_7[18, c(6:20)]
frLeg_12_7 <- frLeg_12_7[-c(17:21),]
dic_8 <- frLeg_12_8[9, c(6:15)]
frLeg_12_8 <- frLeg_12_8[-c(8:12),]
dic_9 <- frLeg_12_9[15, c(6:19)]
frLeg_12_9 <- frLeg_12_9[-c(14:18),]
dic_10 <- frLeg_12_10[45, c(6:24)]
frLeg_12_10 <- frLeg_12_10[-c(44:47),]
dic_11 <- frLeg_12_11[40, c(6:26)]
frLeg_12_11 <- frLeg_12_11[-c(39:43),]
# rename country column 
frLeg_12_1 <- renamer(frLeg_12_1, 1)
frLeg_12_2 <- renamer(frLeg_12_2, 1)
frLeg_12_3 <- renamer(frLeg_12_3, 1)
frLeg_12_4 <- renamer(frLeg_12_4, 1)
frLeg_12_5 <- renamer(frLeg_12_5, 1)
frLeg_12_6 <- renamer(frLeg_12_6, 1)
frLeg_12_7 <- renamer(frLeg_12_7, 1)
frLeg_12_8 <- renamer(frLeg_12_8, 1)
frLeg_12_9 <- renamer(frLeg_12_9, 1)
frLeg_12_10 <- renamer(frLeg_12_10, 1)
frLeg_12_11 <- renamer(frLeg_12_11, 1)
# doing the 'normal steps' for every dataframe
# I could have been more accurate 
frLeg_12_1[-1] <- lapply(frLeg_12_1[-1], function(y) as.numeric(as.character(y)))
frLeg_12_1 <- slim_fitter(frLeg_12_1, "Karel VEREYCKEN", "Christophe NAVEL")
#
frLeg_12_2[-1] <- lapply(frLeg_12_2[-1], function(y) as.numeric(as.character(y)))
frLeg_12_2 <- slim_fitter(frLeg_12_2, "Françoise LINDEMANN", "Thérèse MARIANNE-PEPIN")
frLeg_12_3[-1] <- lapply(frLeg_12_3[-1], function(y) as.numeric(as.character(y)))
frLeg_12_3 <- slim_fitter(frLeg_12_3,  "Guy LE GUEZENNEC", "Edith TIXIER")
frLeg_12_4[-1] <- lapply(frLeg_12_4[-1], function(y) as.numeric(as.character(y)))
frLeg_12_4 <- slim_fitter(frLeg_12_4, "Tanguy LE BRETON", "Dominique PAILLE")
frLeg_12_5[-1] <- lapply(frLeg_12_5[-1], function(y) as.numeric(as.character(y)))
frLeg_12_5 <- slim_fitter(frLeg_12_5, "Juliette ESTIVILL", "Jean-Bastien URFELS")
frLeg_12_6[-1] <- lapply(frLeg_12_6[-1], function(y) as.numeric(as.character(y)))
frLeg_12_6 <- slim_fitter(frLeg_12_6, "Romain DEVOUASSOUX", "Christiane FLOQUET")
frLeg_12_7[-1] <- lapply(frLeg_12_7[-1], function(y) as.numeric(as.character(y)))
frLeg_12_7 <- slim_fitter(frLeg_12_7, "Isabelle ROBIN", "Eric BOURGUIGNON")
frLeg_12_8[-1] <- lapply(frLeg_12_8[-1], function(y) as.numeric(as.character(y)))
frLeg_12_8 <- slim_fitter(frLeg_12_8, "Gil TAIEB", "Julien LEMAITRE")
frLeg_12_9[-1] <- lapply(frLeg_12_9[-1], function(y) as.numeric(as.character(y)))
frLeg_12_9 <- slim_fitter(frLeg_12_9, "Bertrand VITU", "Zine-Eddine M'JATI")
frLeg_12_10[-1] <- lapply(frLeg_12_10[-1], function(y) as.numeric(as.character(y)))
frLeg_12_10 <- slim_fitter(frLeg_12_10, "Guy MAKKI", "Marcel MISSLIN")
frLeg_12_11 <- frLeg_12_11[-c(26)]
frLeg_12_11[-1] <- lapply(frLeg_12_11[-1], function(y) as.numeric(as.character(y)))
# something weird with that column -> same in excel! 
frLeg_12_11 <- slim_fitter(frLeg_12_11, "Aurélien LESLUYE", "Jean-Loup FAYOLLE")
names(frLeg_12_1)[4] <- 'participation'
names(frLeg_12_2)[4] <- 'participation'
names(frLeg_12_3)[4] <- 'participation'
names(frLeg_12_4)[4] <- 'participation'
names(frLeg_12_5)[4] <- 'participation'
names(frLeg_12_6)[4] <- 'participation'
names(frLeg_12_7)[4] <- 'participation'
names(frLeg_12_8)[4] <- 'participation'
names(frLeg_12_9)[4] <- 'participation'
names(frLeg_12_10)[4] <- 'participation'
names(frLeg_12_11)[4] <- 'participation'
# paste the data  
frLeg_12 <- do.call("rbind", list(frLeg_12_1, frLeg_12_2, frLeg_12_3, frLeg_12_4, frLeg_12_5, frLeg_12_6,
                               frLeg_12_7, frLeg_12_8, frLeg_12_9, frLeg_12_10, frLeg_12_11))
frLeg_12 <- party_sorter(frLeg_12)
frLeg_12$ph <- looper(frLeg_12)
# now I would neeed dictionary list and use it
dic_list = c(dic_1, dic_2, dic_3, dic_4, 
             dic_5, dic_6, dic_7, dic_8,
             dic_9, dic_10, dic_11)
lapply(dic_list, function(x) {
  pivot_longer(x, everything())} )
dic_1 %>% pivot_longer(everything())
dif_dic = NULL
for (i in c(dic_1, dic_2, dic_3, dic_4, 
            dic_5, dic_6, dic_7, dic_8,
            dic_9, dic_10, dic_11)){
  assign(i, pivot_longer(i, everything()))
  df_dic = bind_rows(df_dic, i)
}
pivot_longer(dic_list, everything())
dic_list
# not working

## France Legislative 2017 
#missing // just realized that
##Romania======================================================================
## Presidential 2009
RoPres_09 <- read_xls("Romania/Pres. 2009 Romania/Source Files/1st Round/Romania Pres. 2009 1st Round (All Polling Stations).xls")
RoPres_09_IL <- RoPres_09 %>% filter(JUDET != "Strainatate")
RoPres_09 <- RoPres_09 %>% filter(JUDET == "Strainatate")
RoPres_09 <- RoPres_09[-c(1:3)]
RoPres_09 <- renamer(RoPres_09, 1)
RoPres_09 <- aggregate(c(RoPres_09[c(5,7, 9:24)]), by = RoPres_09["country"], sum)
names_ro <- c('ANTONESCU','GEOANA', 'KELEMEN', 'BASESCU', 'OPRESCU', 'BECALI', 'IANE',
              'MANOLE', 'VADIM-TUDOR', 'CERNEA', 'POTIRCA', 'ROTARU')
names(RoPres_09)[8:19] <- names_ro
RoPres_09 <- slim_fitter(RoPres_09, 'ANTONESCU', 'ROTARU')
RoPres_09$ph <- looper(RoPres_09)
RoPres_09_nw <- RoPres_09 %>% filter(party == "BASESCU")
## renaming
RoPres_09 <- my_pivoter(RoPres_09)
RoPres_09 <- column_sorter(RoPres_09, 8, 31)
RoPres_09_nw <- RoPres_09_nw[c(1,8,9)]
RoPres_09 <- national_winner(RoPres_09, RoPres_09_nw)
RoPres_09 <- RoPres_09[-c(2,7)]
names(RoPres_09)[2:5] <- c('total_votes', 'valid_votes', 'null_votes', 'registered_voters')

## 2014 presendential 
RoPres_14 <- read_xls("Romania/Pres. 2014 Romania/Source Files/1st Round/Romania Pres. 2014 1st Round (All Polling Stations).xls")
RoPres_14 <- RoPres_14[-c(32,33)]
RoPres_14_IL <- RoPres_14 %>% filter(`Cod Birou Electoral` != 48)
RoPres_14 <- RoPres_14 %>% filter(`Cod Birou Electoral` == 48)
RoPres_14 <- RoPres_14[-c(1:6)]
RoPres_14 <- renamer(RoPres_14, 1)
RoPres_14 <- aggregate(c(RoPres_14[c(4:25)]), by = RoPres_14["country"], sum)
RoPres_14 <- slim_fitter(RoPres_14, 'HUNOR.KELEMEN', 'CORNELIU.VADIM.TUDOR')
RoPres_14$ph <- looper(RoPres_14)
RoPres_14_nw <- RoPres_14%>% filter(party == "VICTOR.VIOREL.PONTA")
RoPres_14_nw <- RoPres_14_nw[c(1,10,11)]
RoPres_14 <- my_pivoter(RoPres_14)
RoPres_14 <- RoPres_14[-c(2,3,5,9)]
names(RoPres_14)[2:5] <- c('total_votes', 'valid_votes', 'null_votes', 'registered_voters')
RoPres_14 <- column_sorter(RoPres_14, 6, 33)
RoPres_14 <- national_winner(RoPres_14, RoPres_14_nw)

## Leg 2012
RoLeg_12 <- read_xls("Romania/Leg. 2012 Romania/Source Files. Romania Leg. 2012/Rezultate2012-StatisticaStrainatatePeTari.xls")
RoLeg_12 <- RoLeg_12 %>% filter(`Tip Colegiu` == 'CAMERA DEPUTATILOR')
RoLeg_12 <- RoLeg_12[-c(1,2,4,5)]
RoLeg_12 <- renamer(RoLeg_12,1)
names(RoLeg_12)[16] <- 'party'
names(RoLeg_12)[17] <- 'votes'
names(RoLeg_12)[c(8,10,12,13,14)] <- c('total_votes', 'registered_voters', 'valid_votes', 'null_votes', 'white_votes')
RoLeg_12 <- RoLeg_12[-c(2:7, 9, 11, 15, 18)]
RoLeg_12$ph <- looper(RoLeg_12)
RoLeg_12_nw <- RoLeg_12 %>% filter(grepl("UNIUNEA SOCIAL LIBERAL", party))
RoLeg_12_nw <- RoLeg_12_nw[c(1,7,8)]
RoLeg_12 <- my_pivoter(RoLeg_12)
RoLeg_12 <- column_sorter(RoLeg_12, 7, 56)
RoLeg_12 <- national_winner(RoLeg_12, RoLeg_12_nw)

### Bolivia: ########################
## 2009 General (?)
BoGe_09 <- read_xlsx("New/Bolivia/2009/Source Files/ELECCIONES GENERALES 2009 EXTERIOR.xlsx")
BoGe_09 <- row_to_names(BoGe_09, row_number = 2)
BoGe_09 <- renamer(BoGe_09, 2)
# excluding percentage columns
indx <- grepl('%', colnames(BoGe_09))
BoGe_09 <- BoGe_09[!(indx)]
BoGe_09 <- BoGe_09[-c(5),]
BoGe_09[3:10] <- lapply(BoGe_09[3:10], function(y) as.numeric(as.character(y)))
BoGe_09 <- slim_fitter(BoGe_09, 'MAS-IPSP', 'AS')
BoGe_09$ph <- looper(BoGe_09)
names(BoGe_09)[3:6] <- c('valid_votes', 'blanco_votes', 'null_votes', 'total_votes')
names
BoGe_09$country <- countrycode(BoGe_09$Codigo, 'iso2c', 'country.name')
BoGe_09$country <- countryname(BoGe_09$country)
BoGe_09_nw <- BoGe_09 %>% filter(party == 'MAS-IPSP')
BoGe_09_nw <- BoGe_09_nw[c(2,7,8)]
BoGe_09 <- my_pivoter(BoGe_09)
BoGe_09 <- column_sorter(BoGe_09, 7,22)
BoGe_09 <- national_winner(BoGe_09, BoGe_09_nw)
BoGe_09 <- BoGe_09[-c(1)]
BoGe_09[2:5] <- lapply(BoGe_09[2:5], function(y) as.numeric(as.character(y)))

## 2014 
BoGe_14 <- read_xlsx("New/Bolivia/2014/Source Files/ELECCIONES GENERALES 2014 EXTERIOR.xlsx")
BoGe_14 <- row_to_names(BoGe_14, row_number = 2)
BoGe_14 <- renamer(BoGe_14, 2)
indx <- grepl('%', colnames(BoGe_14))
BoGe_14 <- BoGe_14[!(indx)]
BoGe_14 <- BoGe_14[-c(34),]
BoGe_14[3:12] <- lapply(BoGe_14[3:12], function(y) as.numeric(as.character(y)))
BoGe_14 <- slim_fitter(BoGe_14, 'MAS-IPSP', 'MSM')
BoGe_14$ph <- looper(BoGe_14)
BoGe_14_nw <- BoGe_14 %>% filter(party == 'MAS-IPSP')
BoGe_14_nw <- BoGe_14_nw[c(2,8,9)]
names(BoGe_14)[3:7] <- c('valid_votes', 'blanco_votes', 'null_votes', 'total_votes', 'registered_voters')
BoGe_14 <- my_pivoter(BoGe_14)
BoGe_14 <- column_sorter(BoGe_14, 8, 17)
BoGe_14 <- national_winner(BoGe_14, BoGe_14_nw)
BoGe_14$country <- countrycode(BoGe_14$Codigo, 'iso2c', 'country.name')
BoGe_14 <- BoGe_14[-c(1)]
# 
#### Bulgaria-------------------------------------------------------------------
#Bulgaria 2016 
#BuPres_16_section<- read.delim("New/Bulgaria/Pres. 2016/06.11.2016 Source Files Bulgaria Pres. 2016/sections_06.11.2016.txt", sep = ";")
BuPres_16 <- read.delim("New/Bulgaria/Pres. 2016/06.11.2016 Source Files Bulgaria Pres. 2016/votes_06.11.2016.txt", sep = ";", header = F)
#BuPres_16_candidates <- read.delim("New/Bulgaria/Pres. 2016/06.11.2016 Source Files Bulgaria Pres. 2016/cik_candidates_06.11.2016.txt", sep = ";")
## now we have fourfold structure // 
BuPres_16 <- BuPres_16 %>% filter(V1 >= 320100001)
#extracted the file with country_names myself
BuPres_names <- read.delim("New/Bulgaria/Pres. 2016/country_names.txt", sep = ";", header = F)
BuPres_names$country <- str_split_fixed(BuPres_names$V5, ",", 2)[,1]
BuPres_names$country <- countryname(BuPres_names$country)
BuPres_names <- BuPres_names[c(1,9)]
Bu_dic <- do.call("rbind", list(Bu_dic, BuPres_names))
Bu_dic <- Bu_dic %>% filter(!(duplicated(V1)))
#setting countryname according to the codes 
BuPres_16$country <- countrycode(BuPres_16$V1, "V1", "country", custom_dic = Bu_dic)
BuPres_16 <- BuPres_16[-c(1,2)]
BuPres_16 <- aggregate(c(BuPres_16[-c(106)]), by = BuPres_16["country"], sum)
# now problem /nr party / votes / bs / bs / invalid 
number = c(4)
a = 4
while (a < 104){
  number = append(number, a + 1)
  a =  a + 5
  number = append(number, a)
}
number = append(number, 105)
BuPres_16 <- BuPres_16[-c(number)]
number = c(2)
a = 2
while (a < 62){
  a =  a + 3
  number = append(number, a)
}
BuPres_16 <- BuPres_16[-c(number)]
number = c()
for (i in c(2:43)){
  if (i %% 2 != 0){
    number = append(number, i)
  }
}
BuPres_16$invalid_votes <- rowSums(BuPres_16[,c(number)])
BuPres_16 <- BuPres_16[-c(number)]
#names actually from google translate -> could be problematic
party_names <- c('Plamen Trifonov Hristov', 'MOVEMENT FOR RADICAL CHANGE BULGARIAN SPRING', 'Nikolay Toshkov Enchev',
                 'CHRISTIAN-SOCIAL UNION', 'Valentin Assenov Nikolov', 'REFORM BLOCK', 'Vittorio Biserov Milanov',
                 'BULGARIAN SOCIAL DEMOCRACY - EUROLEVICA', 'Ivan Ivanov Stamboliev', 'Dvizhenie 21 - NMSS',
                 'Stefan Lambov Danailov', 'Bulgarian Democratic Community', 'KALFIN-PRESIDENT', 'Valeri Vassilev Venkov',
                 'GERB', 'Ivaylo Plamenov Velinov', 'UNITED PATRIOTS', 'Balkan Democratic League Political Party', 
                 'Ognyan Slavchev Tsvetkov', 'Bulgarian National Union', 'Isai Velichkov Peshev')
names(BuPres_16)[2:22] <- party_names
BuPres_16 <- slim_fitter(BuPres_16, 'Plamen Trifonov Hristov', 'Isai Velichkov Peshev')
BuPres_16$ph <- looper(BuPres_16)
BuPres_16_nw <- BuPres_16 %>% filter(party == 'Stefan Lambov Danailov')
BuPres_16_nw <- BuPres_16_nw[c(1,3,4)]
BuPres_16 <- my_pivoter(BuPres_16)
BuPres_16 <- column_sorter(BuPres_16, 3, 44)
BuPres_16 <- national_winner(BuPres_16, BuPres_16_nw)
number = c()
for (i in 3:44){
  if(i %% 2 != 0){
    number = append(number, i)
  }
}
BuPres_16 <- add_column(BuPres_16, valid_votes = rowSums(BuPres_16[,c(number)]), .after = "country")
BuPres_16 <- add_column(BuPres_16, total_votes = (BuPres_16$valid_votes + BuPres_16$invalid_votes), .after = "country")

## Legislative 2014
BuLeg_14 <- read.delim("New/Bulgaria/Parl 2014/Source Files. Bulgaria Parl 2014/votes_pe2014.txt", sep = ";", header = F)
BuLeg_14_sections <- read.delim("C:/Users/lenovo/Documents/BSE/RA/Data/Data/EVP/New/Bulgaria/Parl 2014/counries.txt", sep = ";", header = F)
BuLeg_14_sections$country <- str_split_fixed(BuLeg_14_sections$V2, ",", 2)[,1]
BuLeg_14_sections$country <- countryname(BuLeg_14_sections$country)
Bu_dic <- BuLeg_14_sections[c(1,7)]
# only selecting abroad countries 
BuLeg_14 <- BuLeg_14 %>% filter(V1 >= 320100001)
#turning codes into countrynames
BuLeg_14$country <- countrycode(BuLeg_14$V1, 'V1', 'country', custom_dict = Bu_dic)
BuLeg_14$country[BuLeg_14$V1 == 321700107] <- 'Spain'
BuLeg_14 <- BuLeg_14[-c(1,52:54)]
names(BuLeg_14)
number = c()
for (i in c(2:51)){
  if (i %% 2 != 0){
    number = append(number, i)
  }
}

# 25 candidates with valid and not valid votes 
# so there is a column for every candidate and his/her invalid votes 
# going to aggregate these like above
BuLeg_14 <- aggregate(c(BuLeg_14[-c(51)]), by = BuLeg_14["country"], sum)
BuLeg_14$invalid_votes <- rowSums(BuLeg_14[,c(number)])
BuLeg_14 <- BuLeg_14[-c(number)]
# will remove pp, kp, hyphen as well as comma (before names)
# names plugged in from google translator -> could be problematic
party_names <- c("Movement 21", "BULGARIAN SOCIALDEMOCRACY", 
                 "BULGARIAN NATIONAL UNION", "NOVOTO VREME",
                 "UNITED BULGARIA", "SOCIAL DEMOCRATIC PARTY", "REFORM BLOCK", "Nova Bulgaria",
                 "GERB", "SOCIETY FOR NEW BULGARIA", "ATTACK", "NMSS", 
                 "BULGARIA UNCENSORED", "Coalition ABV", "THE LEFT AND THE GREEN PARTY",
                 "NEW ALTERNATIVE", "KP DESNITE", "MRF", "Glas Naroden", "GREEN PARTY", "NEW FORCE",#
                 "BSP left Bulgaria", "THE GREENS", "PATRIOTIC FRONT", "Republic BG")
names(BuLeg_14)[2:26] <- party_names
BuLeg_14 <- slim_fitter(BuLeg_14, "Movement 21", "Republic BG")
BuLeg_14$ph <- looper(BuLeg_14)
BuLeg_14_nw <- BuLeg_14 %>% filter(party == 'GERB')
BuLeg_14_nw <- BuLeg_14_nw[c(1,3,4)]
BuLeg_14 <- my_pivoter(BuLeg_14)
BuLeg_14 <- column_sorter(BuLeg_14, 3, 52)
BuLeg_14 <- national_winner(BuLeg_14, BuLeg_14_nw)
number = c()
for (i in c(3:52)){
  if (i %% 2 != 0){
    number = append(number, i)
  }
}

BuLeg_14 <- add_column(BuLeg_14, valid_votes = rowSums(BuLeg_14[,c(number)]), .after = "country")
BuLeg_14 <- add_column(BuLeg_14, total_votes = (BuLeg_14$valid_votes + BuLeg_14$invalid_votes), .after = "country")

##Legislative 2017
BuLeg_17 <- read.delim("C:/Users/lenovo/Documents/BSE/RA/Data/Data/EVP/New/Bulgaria/Parl 2017/Source Files. Bulgaria Parl 2017/votes_26.03.2017.txt", sep = ";", header = F)
BuLeg_17 <- BuLeg_17 %>% filter(V1 >= 320100001)
BuLeg_17_sections <-  read.delim("C:/Users/lenovo/Documents/BSE/RA/Data/Data/EVP/New/Bulgaria/Parl 2017/country_names.txt", sep = ";", header = F)
BuLeg_17_sections$country <- str_split_fixed(BuLeg_17_sections$V5, ",", 2)[,1]
BuLeg_17_sections$country <- countryname(BuLeg_17_sections$country)
BuLeg_17_sections <- BuLeg_17_sections[c(1,8)]
Bu_dic <- do.call("rbind", list(Bu_dic, BuLeg_17_sections))
Bu_dic <- Bu_dic %>% filter(!(duplicated(V1)))
BuLeg_17$country <- countrycode(BuLeg_17$V1, "V1", "country", custom_dic = Bu_dic)
BuLeg_17 <- BuLeg_17[-c(1,2)]
BuLeg_17 <- aggregate(c(BuLeg_17[-c(67)]), by = BuLeg_17["country"], sum)

## now besides country_column -> number_party / number_valid / number_invalid
number = c(2)
a = 2
while (a < 62){
  a =  a + 3
  number = append(number, a)
}
BuLeg_17 <- BuLeg_17[-c(number, 65:67)]
number = c()
for (i in c(2:43)){
  if (i %% 2 != 0){
    number = append(number, i)
  }
}
BuLeg_17$invalid_votes <- rowSums(BuLeg_17[,c(number)])
BuLeg_17 <- BuLeg_17[-c(number)]
party_names <- c('DROM',
                 'Movement for Radical Change Bulgarian Spring',
                 'Political Party Movement Forward Bulgaria',
                 'DSB',  'COALITION OF DISSATISFIED', 'WILL','BDC', ' Movement 21', 'MRF',
                 'BSP', 'GERB', 'GREEN party', 'Association DOST', 'Vazrazhdane', 'MOVEMENT CHARGED BULGARIA',
                 'MOVEMENT TO BULGARIA', 'UNITED PATRIOTS', 'WHO','NATIONAL REPUBLICAN PARTY',
                 'BULGARIAN NATIONAL ASSOCIATION', 'REFORMATION BLOCK')

names(BuLeg_17)[2:22] <- party_names
BuLeg_17 <- slim_fitter(BuLeg_17, 'DROM', 'REFORMATION BLOCK')
BuLeg_17$ph <- looper(BuLeg_17)
BuLeg_17_nw <- BuLeg_17 %>% filter(party == "GERB")
BuLeg_17_nw <- BuLeg_17_nw[c(1,3,4)]
BuLeg_17 <- my_pivoter(BuLeg_17)
BuLeg_17 <- column_sorter(BuLeg_17, 3, 44)
BuLeg_17 <- national_winner(BuLeg_17, BuLeg_17_nw)
number = c()
for (i in c(3:44)){
  if (i %% 2 != 0){
    number = append(number, i)
  }
}

BuLeg_17 <- add_column(BuLeg_17, valid_votes = rowSums(BuLeg_17[,c(number)]), .after = "country")
BuLeg_17 <- add_column(BuLeg_17, total_votes = (BuLeg_17$valid_votes + BuLeg_17$invalid_votes), .after = "country")
#### Costa Rica ---------------------------------------------------------------
## Presidential 2018
setwd('New/Costa Rica/Source Files. Costa Rica Pres. 2018/')
# we have files for every country
my_files <- list.files(pattern = "\\.xlsx$")
data_cr <- lapply(my_files, read_xlsx, range = cell_rows(5:25))
names(data_cr) <- gsub("\\.xlsx$", "", my_files)
# renaming every first column of all the files
for(i in seq_along(data_cr))
  data_cr[[i]]$country = names(data_cr)[i]
# binding them together
# excuse the naming
df_cr = do.call(rbind, data_cr)
df_cr <- df_cr[-c(3)]
df_cr_1 <- df_cr %>%
  pivot_wider(names_from = `Partido político`, values_from = Votos)
df_cr_1 <- df_cr_1[-c(15)]
df_cr_1 <-slim_fitter(df_cr_1, "ACCIÓN CIUDADANA", "RENOVACIÓN COSTARRICENSE")
df_cr_1$ph <- looper(df_cr_1)
df_cr_1_nw <- df_cr_1 %>% filter(party == "RESTAURACIÓN NACIONAL")
df_cr_1_nw <- df_cr_1_nw[c(1,8,9)]
names(df_cr_1)
df_cr_1 <- df_cr_1[-c(2,3)]
# in this case null votes incluldes blanco (?) 
names(df_cr_1)[2:5] <- c('total_votes', 'valid_votes', 'null_votes', 'participation')
df_cr_1$registered_voters <-  round(df_cr_1$total_votes / df_cr_1$participation)
df_cr_1 <- df_cr_1[c(1,2,3,4,9,6,7,8)]
CrPres_18 <- my_pivoter(df_cr_1)
CrPres_18 <- column_sorter(CrPres_18, 6, 31)
CrPres_18 <- national_winner(CrPres_18, df_cr_1_nw)
# getting the countrynames
CrPres_18$weird <- countryname(CrPres_18$country)
CrPres_18$weird[CrPres_18$country == 'Dom. Rep.'] <- "Dominican Republic"
CrPres_18$weird[CrPres_18$country == 'Quatar'] <- "Qatar"
CrPres_18$country <- CrPres_18$weird
CrPres_18 <- CrPres_18[-c(34)]

#### Brazil --------------------------------------------------------------------
## Presidential 2018
setwd("C:/Users/lenovo/Documents/BSE/RA/Data/Data/EVP")
BraPres_18 <- read.csv("New/Brazil/Pres. 2018/votacao_partido_munzona_2018_BR.csv", sep = ";")
BraPres_18 <- BraPres_18 %>% filter(SG_UF == 'ZZ')
# problematic we have citynames that have to be translated into countries
BraPres_18$NM_MUNICIPIO <- tolower(BraPres_18$NM_MUNICIPIO)
BraPres_18$iso2 <- countrycode(BraPres_18$NM_MUNICIPIO, 'city_origin', 'iso2a', custom_dict = country_names)
# this dictionary was actually made by hand mostly and the geonames database 
# I will not provide code for it as it is partly made manually so just the file will be attached 
#of the dictionary 
#only first round 
BraPres_18 <- BraPres_18 %>% filter(CD_ELEICAO == '295')
BraPres_18 <- BraPres_18[-c(1:18)]
names(BraPres_18)[9] <- 'votes'
BraPres_18 <- aggregate(c(BraPres_18['votes']), 
                        by = c(BraPres_18['NM_PARTIDO'], BraPres_18['NR_PARTIDO'], BraPres_18["iso2"]), sum)
BraPres_18$country <- countrycode(BraPres_18$iso2, 'iso2c', 'country.name')
BraPres_18 <- BraPres_18[c(5,3,1,2,4)]
names(BraPres_18)[3] <- 'party'
BraPres_18 <- party_sorter(BraPres_18)
BraPres_18$ph <- looper(BraPres_18)
BraPres_18 <- BraPres_18[-c(4)]
BraPres_18_nw <- BraPres_18 %>% filter(party == "Partido Social Liberal")
BraPres_18_nw <- BraPres_18_nw[c(1,3,4)]
BraPres_18 <- my_pivoter(BraPres_18)
BraPres_18 <- column_sorter(BraPres_18, 3, 28)
BraPres_18 <- national_winner(BraPres_18, BraPres_18_nw)
BraPres_18 <- BraPres_18[-c(2)]
number = c()
for(i in 2:27){
  if (i %% 2 == 0){
    number = append(number, i)
  }
}
BraPres_18 <- add_column(BraPres_18, valid_votes = rowSums(BraPres_18[,c(number)], na.rm = T), .after = "country")
## Presidential 2014
BraPres_14 <- read.csv("New/Brazil/Pres. 2014/[RAW] votacao_partido_munzona_2014_BR.csv", sep = ";")
BraPres_14 <- BraPres_14 %>% filter(SG_UF == 'ZZ')
BraPres_14$NM_MUNICIPIO <- tolower(BraPres_14$NM_MUNICIPIO)
BraPres_14$iso2 <- countrycode(BraPres_14$NM_MUNICIPIO, 'city_origin', 'iso2a', custom_dict = country_names)
BraPres_14 <- BraPres_14 %>% filter(CD_ELEICAO == '143')
BraPres_14 <- BraPres_14[-c(1:18)]
names(BraPres_14)[9] <- 'votes'
BraPres_14 <- aggregate(c(BraPres_14['votes']), 
                        by = c(BraPres_14['NM_PARTIDO'], BraPres_14['NR_PARTIDO'], BraPres_14["iso2"]), sum)
BraPres_14$country <- countrycode(BraPres_14$iso2, 'iso2c', 'country.name')
BraPres_14 <- BraPres_14[c(5,3,1,2,4)]
names(BraPres_14)[3] <- 'party'
BraPres_14 <- party_sorter(BraPres_14)
BraPres_14$ph <- looper(BraPres_14)
BraPres_14 <- BraPres_14[-c(4)]
BraPres_14_nw <- BraPres_14 %>% filter(party == "Partido dos Trabalhadores")
BraPres_14_nw <- BraPres_14_nw[c(1,3,4)]
BraPres_14 <- my_pivoter(BraPres_14)
BraPres_14 <- column_sorter(BraPres_14, 3, 24)
BraPres_14 <- national_winner(BraPres_14, BraPres_14_nw)
BraPres_14 <- BraPres_14[-c(2)]
number = c()
for(i in 2:23){
  if (i %% 2 == 0){
    number = append(number, i)
  }
}
BraPres_14 <- add_column(BraPres_14, valid_votes = rowSums(BraPres_14[,c(number)], na.rm = T), .after = "country")

## Presidential 2010
BraPres_10 <- read.delim("New/Brazil/Pres. 2010/Source Files. Brazil 2010 Pres. Elections/Partido Por Municipio (Exterior).txt", sep = ";", header = F)
BraPres_10_2 <- read.delim("New/Brazil/Pres. 2010/Source Files. Brazil 2010 Pres. Elections/Partido por MunicipioZona.txt", sep = ";", header = F)
x <- gsub("\\-.*","",BraPres_10$V10)
x <- unique(x)
x <- tolower(x)
u <- countrycode(x, 'city_origin', 'iso2a', custom_dict = country_names)
new_names <- cbind(x, u)
new_names <- as.data.frame(new_names)
names(new_names) <- c('city_origin', 'iso2a')
new_names$iso2a[new_names$city_origin %in% c('nagoya', 'mitsukaido', 'oizumi',
                                             'suzuka', 'takaoka',
                                             'toyohashi', 'ueda')] <- 'JP'
new_names$iso2a[new_names$city_origin == "sta c la sierra"] <- 'BO'
z <- c("abidjan", "argel", "brazzaville", "cingapura",  "damasco", "dili", "dongguan", "kingston", "libreville", "mendonza",
"méxico", "mitsukaido", "nagoya", "nairobi", "oizumi", "panamá", "pedro caballero", "roterdã", "saltos do guaira", 
"sta c la sierra", "suzuka", "takaoka", "toyohashi", "ueda")

BraPres_10$V10 <- gsub("\\-.*","",BraPres_10$V10)
BraPres_10$V10 <- tolower(BraPres_10$V10)
BraPres_10$iso2 <- countrycode(BraPres_10$V10, 'city_origin', 'iso2a', custom_dict = new_names)
#  forgot saltos do guaira // 
BraPres_10$iso2[BraPres_10$V10 == 'saltos do guaira'] <- 'PY'
BraPres_10$country <- countrycode(BraPres_10$iso2, 'iso2c', 'country.name')
BraPres_10 <- BraPres_10[-c(1:13)]
BraPres_10 <- BraPres_10[-c(2:5, 7,8)]
BraPres_10 <- BraPres_10[c(3,1,2)]
names(BraPres_10)[c(2,3)] <- c('party', 'votes')
BraPres_10 <- aggregate(BraPres_10['votes'], by = c(BraPres_10['country'], BraPres_10['party']), sum)
BraPres_10 <- party_sorter(BraPres_10)
BraPres_10$ph <- looper(BraPres_10)
BraPres_10_nw <- BraPres_10 %>% filter(party == 'PT')
BraPres_10_nw <- BraPres_10_nw[c(1,2,3)]
BraPres_10 <- my_pivoter(BraPres_10)
BraPres_10 <- column_sorter(BraPres_10, 2, 19)
BraPres_10 <- national_winner(BraPres_10, BraPres_10_nw)
number = c()
for(i in 2:19){
  if (i %% 2 == 0){
    number = append(number, i)
  }
}
BraPres_10 <- add_column(BraPres_10, valid_votes = rowSums(BraPres_10[,c(number)], na.rm = T), .after = "country")

## Presidential 2006 
BraPres_06 <- read.delim("New/Brazil/Pres. 2006/votacao_partido_munzona_2006/votacao_partido_munzona_2006_ZZ [EXTERIOR. RAW].txt", sep = ";", header =F)
BraPres_06$V9 <- tolower(BraPres_06$V9)
BraPres_06$country <- countrycode(BraPres_06$V9, 'portugese', 'english', custom_dict = custom_dict)
BraPres_06$country[BraPres_06$V9 == "checoslováquia"] <- 'czechia'
BraPres_06$country[BraPres_06$V9 == "cingapura"] <- 'singapore'
BraPres_06$country[BraPres_06$V9 == "coréia"] <- 'south korea'
BraPres_06$country[BraPres_06$V9 == "emirados árabes"] <- 'united arab emirates'
BraPres_06$country[BraPres_06$V9 == "guiné bissau"] <- 'guinea-bissau'
BraPres_06$country[BraPres_06$V9 == "hong kong"] <- 'hong kong sar china' 
BraPres_06$country[BraPres_06$V9 == "inglaterra"] <- 'united kingdom'
BraPres_06$country[BraPres_06$V9 == "kênia"] <- 'kenya'
countrycode(BraPres_06$country, 'country.name', 'iso3c')
# only first round
BraPres_06 <- BraPres_06 %>% filter(V4 == 1) 
BraPres_06 <- BraPres_06[-c(1:17)]
# now without party number
BraPres_06 <- BraPres_06[c(5,1,2,3,4)]
names(BraPres_06)[c(2,3)] <- c('party', 'votes')
BraPres_06 <- party_sorter(BraPres_06)
BraPres_06$ph <- looper(BraPres_06)
# drop them as they don't contain information 
BraPres_06 <- BraPres_06[-c(4,5)]
BraPres_06_nw <- BraPres_06 %>% filter(party == "PARTIDO DOS TRABALHADORES")
BraPres_06_nw <- BraPres_06_nw[c(1,2,3)]
BraPres_06 <- my_pivoter(BraPres_06)
BraPres_06 <- column_sorter(BraPres_06, 2, 15)
BraPres_06 <- national_winner(BraPres_06, BraPres_06_nw)
BraPres_06$country <- countryname(BraPres_06$country)
number = c()
for(i in 2:15){
  if (i %% 2 == 0){
    number = append(number, i)
  }
}
BraPres_06 <- add_column(BraPres_06, valid_votes = rowSums(BraPres_06[,c(number)], na.rm = T), .after = "country")

#### croatia ===================================================================
## Leg 2003
croatia_names <- read_xlsx("Croatia/Croatia Parl. 2003/Source Files/Abroad/XI izborna jedinica.xlsx", sheet = 3)
croatia_names <- croatia_names[c(1,2)]
CroParl_03 <- read_xlsx("Croatia/Croatia Parl. 2003/Source Files/Abroad/XI izborna jedinica.xlsx", sheet = 2)
#croatia_names <- CroParl_03[c(1,2)] if nicolas sheet loaded 
CroParl_03 <- CroParl_03 %>% filter(is.na(`Broj biračkog mjesta`))
CroParl_03 <- CroParl_03[-c(1,2), -c(2)]
names(CroParl_03)[1:5] <- c("country", 'registered_voters', 'voters', 'valid_votes', 'invalid_votes')
# next step get party abb besides the independent 
names(CroParl_03)[22:25] <- c("prof. dr.sc. BRANKO BAKULA, dr.med" ,'LJUBO ĆESIĆ', 'JERKO IVANKOVIĆ', 'PETAR MILIĆ') 
# for independent listing //
names(CroParl_03) <- gsub("\r?\n|\r.*","", names(CroParl_03))
names(CroParl_03) <- gsub(".*-", "",names(CroParl_03))
names(CroParl_03) <- gsub("^\\s+|\\s+$", "", names(CroParl_03))
CroParl_03$country <- gsub("Total", "", CroParl_03$country)
CroParl_03 <- CroParl_03[-c(51,52),]
CroParl_03 <- slim_fitter(CroParl_03, "ALIJANSA", "SNS")
CroParl_03$ph <- looper(CroParl_03)
CroParl_03_nw <- CroParl_03 %>% filter(party == "HDZ" )
CroParl_03_nw <- CroParl_03_nw[c(1, 6,7)]
CroParl_03 <- my_pivoter(CroParl_03)
CroParl_03 <- column_sorter(CroParl_03, 6, 51)
CroParl_03 <- national_winner(CroParl_03, CroParl_03_nw)
croatia_names$english <- countryname(croatia_names$`Parliamentary Elections, 23.11.2003`)
croatia_names$Croatia <- gsub("Total", "", croatia_names$Croatia)
CroParl_03$country <- countrycode(CroParl_03$country, 'Croatia', "english", custom_dict = croatia_names)
names(CroParl_03)[3] <- 'total_votes'

## Legislative 2007: 
CroParl_07 <- read_xlsx("Croatia/Croatia Parl. 2007/Source Files/Abroad/11_XI_izborna_jedinica.xlsx", sheet = 3)
CroParl_07 <- CroParl_07[-c(1,2,56), -c(2,4)]
names(CroParl_07)[1:5] <- c("country", 'registered_voters', 'voters', 'valid_votes', 'invalid_votes')
names(CroParl_07)[16:17] <- c("ANDJELKO GALIĆ", "JERKO IVANKOVIĆ LIJANOVIĆ")
names(CroParl_07) <- gsub("\r?\n|\r.*","", names(CroParl_07))
names(CroParl_07) <- gsub(".*-", "",names(CroParl_07))
names(CroParl_07) <- gsub("^\\s+|\\s+$", "", names(CroParl_07))
CroParl_07 <- slim_fitter(CroParl_07, "ABECEDA", "SHB")
CroParl_07$ph <- looper(CroParl_07)
CroParl_07_nw <- CroParl_07 %>% filter(party == "HDZ")
CroParl_07_nw <- CroParl_07_nw[c(1,6,7)]
CroParl_07 <- my_pivoter(CroParl_07)
CroParl_07 <- column_sorter(CroParl_07, 6, 31)
CroParl_07 <- national_winner(CroParl_07, CroParl_07_nw)
names(CroParl_07)[3] <- 'total_votes'
croatia_names <-  read_xlsx("Croatia/Croatia Parl. 2007/Source Files/Abroad/11_XI_izborna_jedinica.xlsx", sheet = 4)
croatia_names <- croatia_names[c(2,5)]
croatia_names$english <- countryname(croatia_names$...2)
CroParl_07$country <- countrycode(CroParl_07$country, '...5', "english", custom_dict = croatia_names)
# for names cheat with Nicolas work :)

##  Legislative 2011 
CroParl_11 <- read_xlsx("Croatia/Croatia Parl. 2011/Source Files/Abroad/11_XI_izborna_jedinica.xlsx", sheet = 3)
CroParl_11 <- CroParl_11[-c(1,54), -c(1,3,4)]
names(CroParl_11)[1:5] <- c("country", 'registered_voters', 'voters', 'valid_votes', 'invalid_votes')
names(CroParl_11) <- gsub("\r?\n|\r.*","", names(CroParl_11))
names(CroParl_11) <- gsub(".*-", "",names(CroParl_11))
names(CroParl_11) <- gsub("^\\s+|\\s+$", "", names(CroParl_11))
## one problem all sort of different HSP variants 
names(CroParl_11)[8] <- "A-HSP"
CroParl_11 <- slim_fitter(CroParl_11, "ABECEDA", "SHZ")
CroParl_11$ph <- looper(CroParl_11)
#CroParl_11_nw <- CroParl_11 %>% filter(party == )
CroParl_11 <- my_pivoter(CroParl_11)
CroParl_11 <- column_sorter(CroParl_11, 6,35)
## no national winner or at least problems 
CroParl_11$weird <- countrycode(CroParl_11$country, '...5', "english", custom_dict = croatia_names)
CroParl_11$weird[CroParl_11$country == "KOSOVO"] <- 'Kosovo'
CroParl_11$weird[CroParl_11$country == "RUSIJA"] <- 'Russia'
CroParl_11$weird[CroParl_11$country == "SLOVAČKA"] <- "Slovakia"
CroParl_11$weird[CroParl_11$country == "SIRIJA"] <- "Syria"
CroParl_11$country <- CroParl_11$weird
CroParl_11 <- CroParl_11[-c(36)]
CroParl_11$country[is.na(CroParl_11$country)] <- 'Slovakia'
names(CroParl_11)[3] <- 'total_votes'

## Leg 2015
CroParl_15 <- read_xlsx("Croatia/Croatia Parl. 2015/Source Files/Abroad/02_11.xlsx", sheet = 1)
grep("-", names(CroParl_15))
CroParl_15 <- CroParl_15[c(6, 12:16,24,32,47,54,62,69,78,88,96,104)]
names(CroParl_15) <- gsub(".*-", "",names(CroParl_15))
names(CroParl_15)[1:5] <- c('country', 'registered_voters', 'total_votes', 'valid_votes', 'invalid_votes')
names(CroParl_15) <- gsub("^\\s+|\\s+$", "", names(CroParl_15))
CroParl_15 <- aggregate(c(CroParl_15[2:16]), by = CroParl_15['country'], sum)
CroParl_15 <- slim_fitter(CroParl_15, 'HSP', 'SRP')
CroParl_15_nw <- CroParl_15 %>% filter(party == 'ZDS')
CroParl_15_nw <- CroParl_15_nw[c(1,6,7)]
CroParl_15$ph <- looper(CroParl_15)
CroParl_15 <-  my_pivoter(CroParl_15)
CroParl_15 <- column_sorter(CroParl_15, 6,27) 
CroParl_15 <- national_winner(CroParl_15, CroParl_15_nw)
# country names // 
CroParl_15$weird <- countrycode(CroParl_15$country, '...5', "english", custom_dict = croatia_names)
CroParl_15$weird[CroParl_15$country == 'KATAR'] <- 'Qatar'
CroParl_15$weird[CroParl_15$country == "KOSOVO"] <- 'Kosovo'
CroParl_15$weird[CroParl_15$country == "RUSIJA"] <- 'Russia'
CroParl_15$country <- CroParl_15$weird
CroParl_15 <- CroParl_15[-c(30)]

## Legislative 2016 
CroParl_16 <- read_xlsx("Croatia/Croatia Parl. 2016/Source Files/Abroad/011_00.xlsx")
grep("-", names(CroParl_16))
CroParl_16 <- CroParl_16[c(6, 12:16,25,40,54,55,63,94,104,111,118,126,138,145,156)]
names(CroParl_16) <- gsub(".*-", "",names(CroParl_16))
names(CroParl_16)[1:5] <- c('country', 'registered_voters', 'total_votes', 'valid_votes', 'invalid_votes')
names(CroParl_16) <- gsub("^\\s+|\\s+$", "", names(CroParl_16))
CroParl_16 <- aggregate(c(CroParl_16[2:19]), by = CroParl_16['country'], sum)
CroParl_16 <- slim_fitter(CroParl_16, 'HSP', 'ORaH')
CroParl_16$ph <- looper(CroParl_16)
CroParl_16_nw <- CroParl_16 %>% filter(party == 'HDZ')
CroParl_16_nw <- CroParl_16_nw[c(1,6,7)]
CroParl_16 <-  my_pivoter(CroParl_16)
CroParl_16 <- column_sorter(CroParl_16, 6,33) 
CroParl_16 <- national_winner(CroParl_16, CroParl_16_nw)
CroParl_16$weird <- countrycode(CroParl_16$country, '...5', "english", custom_dict = croatia_names)
CroParl_16$weird[CroParl_16$country == 'KATAR'] <- 'Qatar'
CroParl_16$weird[CroParl_16$country == "KOSOVO"] <- 'Kosovo'
CroParl_16$weird[CroParl_16$country == "RUSIJA"] <- 'Russia'
CroParl_16$country <- CroParl_16$weird
CroParl_16 <- CroParl_16[-c(36)]

## Presidential 2005
CroPres_05 <- read_xlsx("Croatia/Pres.2005/RAW.Rezultati_po_BM_-_1krug.xlsx")
CroPres_05 <- CroPres_05 %>% filter(Županija == "INOZEMSTVO")
CroPres_05 <- CroPres_05[-c(1,2,4)]
names(CroPres_05)[1:5] <- c('country', 'registered_voters', 'total_votes', 'valid_votes', 'invalid_votes') 
CroPres_05[is.na(CroPres_05)] <- 0
CroPres_05 <- aggregate(c(CroPres_05[c(2:18)]), by = CroPres_05["country"], sum)
CroPres_05 <- slim_fitter(CroPres_05, "Ðurda.Adlešic", "Miroslav.Rajh")
CroPres_05$ph <- looper(CroPres_05)
CroPres_05_nw <- CroPres_05 %>% filter(party == 'Stjepan.Mesic..dipl.iur.')
CroPres_05_nw <- CroPres_05_nw[c(1,6,7)]
CroPres_05 <- my_pivoter(CroPres_05)
CroPres_05 <- column_sorter(CroPres_05, 6, 31)
CroPres_05 <- national_winner(CroPres_05, CroPres_05_nw)
CroPres_05$weird <- countrycode(tolower(CroPres_05$country), 'croatia', 'english', custom_dict = custom_dict)
CroPres_05$weird[tolower(CroPres_05$country) == 'ceška republika'] <- 'Czechia'
CroPres_05$weird[tolower(CroPres_05$country) == 'makedonija'] <- "North Macedonia"
CroPres_05$weird[tolower(CroPres_05$country) == 'ruska federacija'] <- 'Russia'
CroPres_05$weird[tolower(CroPres_05$country) == 'sad'] <- "US"
CroPres_05$weird[tolower(CroPres_05$country) == 'slovacka republika'] <- "Slovakia"
CroPres_05$weird[tolower(CroPres_05$country) == 'srbija i crna gora'] <- "Serbia and Montenegro"
CroPres_05$weird[tolower(CroPres_05$country) == 'velika britanija'] <-  "UK"
CroPres_05$weird[tolower(CroPres_05$country) == 'venecuela'] <-  "Venezuela"
CroPres_05$weird <- countryname(CroPres_05$weird)
CroPres_05$country <- CroPres_05$weird
CroPres_05 <- CroPres_05[-c(34)]

## Presidential 2009 
CroPres_09 <- read_xlsx("C:/Users/lenovo/Documents/BSE/RA/Data/Data/EVP/Croatia/Pres.2009/rezultati_po_bm_1krug.xlsx")
CroPres_09 <- CroPres_09 %>% filter(`Rbr. županije` == 22)
CroPres_09 <- CroPres_09[-c(1,2,4)]
names(CroPres_09)[1:5] <- c('country', 'registered_voters', 'total_votes', 'valid_votes', 'invalid_votes') 
CroPres_09 <- select(CroPres_09,-starts_with("%"))
CroPres_09 <- aggregate(c(CroPres_09[c(2:17)]), by = CroPres_09["country"], sum)
CroPres_09 <- slim_fitter(CroPres_09, "MILAN.BANDIC", "SLAVKO.VUKŠIC..ing.")
CroPres_09$ph <- looper(CroPres_09)
CroPres_09_nw <- CroPres_09 %>% filter(party == "prof.dr.sc..IVO.JOSIPOVIC")
CroPres_09_nw <- CroPres_09_nw[c(1,6,7)]
CroPres_09 <- my_pivoter(CroPres_09)
CroPres_09 <- column_sorter(CroPres_09, 6, 29)
CroPres_09 <- national_winner(CroPres_09, CroPres_09_nw)
## names missing-> see below

## Presidential 2014
CroPres_14 <- read_xlsx("Croatia/Pres.2014/2014_Predsjednik_1_krug_rezultati_po_birackim_mjestima RAW Data. Total.xlsx")
CroPres_14 <- CroPres_14 %>% filter(`Oznaka Gr/Op/Dr` == 'država')
CroPres_14 <- CroPres_14[-c(1,2,3, 5:8)]
CroPres_14 <- CroPres_14[-c(2)]
## because column 2 and 3 are the same
names(CroPres_14)[1:5] <- c('country', 'registered_voters', 'total_votes', 'valid_votes', 'invalid_votes')
CroPres_14 <- select(CroPres_14,-starts_with("(%)"))
CroPres_14 <- aggregate(c(CroPres_14[c(2:9)]), by = CroPres_14["country"], sum)
CroPres_14 <- slim_fitter(CroPres_14, "KOLINDA.GRABAR...KITAROVIC", "IVAN.SINCIC....univ.bacc.ing.el.")
CroPres_14$ph <- looper(CroPres_14)
CroPres_14_nw <- CroPres_14 %>% filter(party == "IVO.JOSIPOVIC")
CroPres_14_nw <- CroPres_14_nw[c(1,6,7)]
CroPres_14 <- my_pivoter(CroPres_14)
CroPres_14 <- column_sorter(CroPres_14, 6, 13)
CroPres_14 <- national_winner(CroPres_14, CroPres_14_nw)
# names missing // -> below

## Presidential 2019
CroPres_19 <- read_xlsx("Croatia/Pres.2019/Croatia Pres. 2019 Raw Data.xlsx")
CroPres_19 <- CroPres_19 %>% filter(`Oznaka Gr/Op/Dr` == 'država')
CroPres_19 <- CroPres_19[-c(1,2,3, 5:9)]
names(CroPres_19)[1:5] <- c('country', 'registered_voters', 'total_votes', 'valid_votes', 'invalid_votes')
CroPres_19[-c(1,2)] <- lapply(CroPres_19[-c(1,2)], function(y) as.numeric(as.character(y)))
CroPres_19 <- aggregate(c(CroPres_19[c(2:16)]), by = CroPres_19["country"], sum)
CroPres_19 <- slim_fitter(CroPres_19, "NEDJELJKO.BABIC", "MIROSLAV.ŠKORO")
CroPres_19$ph <- looper(CroPres_19)
CroPres_19_nw <- CroPres_19 %>% filter(party ==  "ZORAN.MILANOVIC")
CroPres_19_nw <- CroPres_19_nw[c(1,6,7)]
CroPres_19 <- my_pivoter(CroPres_19)
CroPres_19 <- column_sorter(CroPres_19, 6, 27)
CroPres_19 <- national_winner(CroPres_19, CroPres_19_nw)

#in this case total voters and total votes is the same //

### country_names for 09-19 
CroPres_09$weird <- countrycode(CroPres_09$country, '...5', 'english', custom_dict = croatia_names)
CroPres_09$weird[CroPres_09$country == 'JUŽNOAFR. REPUBLIKA'] <- "South Africa"
CroPres_09$weird[CroPres_09$country == 'KOSOVO'] <- 'Kosovo'
CroPres_09$weird[CroPres_09$country == 'SIRIJA'] <- "Syria"
CroPres_09$weird <- countryname(CroPres_09$weird)
CroPres_09$country <- CroPres_09$weird
CroPres_09 <- CroPres_09[-c(32)]
##
CroPres_14$weird <- countrycode(CroPres_14$country, '...5', 'english', custom_dict = croatia_names)
CroPres_14$weird[CroPres_14$country == 'KATAR'] <- "Qatar"
CroPres_14$weird[CroPres_14$country == 'KOSOVO'] <- 'Kosovo'
CroPres_14$weird[CroPres_14$country == 'RUSIJA'] <- "Russia"
CroPres_14$weird <- countryname(CroPres_14$weird)
CroPres_14$country <- CroPres_14$weird
CroPres_14 <- CroPres_14[-c(16)]
##
CroPres_19$weird <- countrycode(CroPres_19$country, '...5', 'english', custom_dict = croatia_names)
CroPres_19$weird[CroPres_19$country == 'KATAR'] <- "Qatar"
CroPres_19$weird[CroPres_19$country == 'KOSOVO'] <- 'Kosovo'
CroPres_19$weird[CroPres_19$country == 'RUSIJA'] <- "Russia"
CroPres_19$weird[CroPres_19$country == 'REPUBLIKA SJEVERNA MAKEDONIJA'] <-"North Macedonia"
CroPres_19$weird <- countryname(CroPres_19$weird)
CroPres_19$country <- CroPres_19$weird
CroPres_19 <- CroPres_19[-c(30)]

#### DomRep --------------------------------------------------------------------
## Presidential 2004
DorPres_04 <- read_xls("Dom. Rep/2004. Pres/Totales por colegio 2004.xls")
DorPres_04 <- DorPres_04 %>% filter(Cod_Provincia > 40)
DorPres_04 <- DorPres_04[-c(1, 3:9)]
names(DorPres_04)[1:5] <-c('country', 'total_votes', 'members', 'valid_votes', 'null_votes')
DorPres_04 <- aggregate(c(DorPres_04[c(2:28)]), by = DorPres_04["country"], sum)
DorPres_04 <- slim_fitter(DorPres_04, 'PRD_', 'PHD_')
DorPres_04$ph <- looper(DorPres_04)
DorPres_04 <- DorPres_04[-c(3,6)]
DorPres_04_nw <- DorPres_04 %>% filter(party == "PLD_")
DorPres_04_nw <- DorPres_04_nw[c(1,5,6)]
DorPres_04 <- my_pivoter(DorPres_04)
DorPres_04 <- column_sorter(DorPres_04, 5, 48)
DorPres_04 <- national_winner(DorPres_04, DorPres_04_nw)
# names
DorPres_04$weird <- countrycode(tolower(DorPres_04$country), 'spanish', 'english', custom_dict = custom_dict)
DorPres_04$weird[DorPres_04$country == 'CANADA'] <- 'Canada'
DorPres_04$country <- countryname(DorPres_04$weird)
DorPres_04 <- DorPres_04[-c(51)]

## Presidential 2008 
DorPres_08 <- read_xls("Dom. Rep/2008. Pres/Resultados de Colegios 2008.xls")
DorPres_08 <- DorPres_08[-c(1:12754), -c(2:6)]
names(DorPres_08)[1:5] <-c('country', 'registered_voters', 'total_votes', 'valid_votes', 'null_votes')
DorPres_08 <- aggregate(c(DorPres_08[c(2:28)]), by = DorPres_08["country"], sum)
DorPres_08 <- slim_fitter(DorPres_08, 'PLD', 'MODA')
DorPres_08$ph <- looper(DorPres_08)
DorPres_08 <- DorPres_08[-c(6)]
DorPres_08_nw <- DorPres_08 %>% filter(party == 'PLD')
DorPres_08_nw <- DorPres_08_nw[c(1,6,7)]
DorPres_08 <- my_pivoter(DorPres_08)
DorPres_08 <- column_sorter(DorPres_08, 6, 49)
DorPres_08 <- national_winner(DorPres_08, DorPres_08_nw)
# name
DorPres_08$weird <- countrycode(tolower(DorPres_08$country), 'spanish', 'english', custom_dict = custom_dict)
DorPres_08$weird[DorPres_08$country == 'CANADA'] <- 'Canada'
DorPres_08$weird[DorPres_08$country == 'HOLANDA'] <- "Netherlands"
DorPres_08$weird[DorPres_08$country == 'PANAMA'] <- "Panama"
DorPres_08$weird[DorPres_08$country == 'SAN MARTIN'] <- "Saint Martin (French part)"
DorPres_08$country <- countryname(DorPres_08$weird)
DorPres_08 <- DorPres_08[-c(52)]

## Presidential 2012
DorPres_12 <- read_xls("Dom. Rep/2012. Pres. & Leg/Pres. 2012/Resultados por Colegios 2012 Nivel Presidencial.xls")
DorPres_12 <- DorPres_12 %>% filter(`Cod. Provincia` > 40)
DorPres_12 <- DorPres_12[-c(1, 3:10)]
DorPres_12 <- renamer(DorPres_12, 1)
DorPres_12 <- aggregate(c(DorPres_12[c(2:31)]), by = DorPres_12["country"], sum)
DorPres_12 <- slim_fitter(DorPres_12, "PRD", "ALPAIS")
names(DorPres_12)[2:7] <- c('registered_voters', 'members', 'valid_votes', 'null_votes',
                            'observed_votes', 'total_votes')
DorPres_12 <- DorPres_12[c(1,2,7,4,5,3,6,8,9)]
DorPres_12$ph <- looper(DorPres_12)
DorPres_12 <- DorPres_12[-c(6,7)]
DorPres_12_nw <- DorPres_12 %>% filter(party == 'PLD')
DorPres_12_nw <- DorPres_12_nw[c(1,6,7)]
DorPres_12 <- my_pivoter(DorPres_12)
DorPres_12 <- column_sorter(DorPres_12,6, 53)
DorPres_12 <- national_winner(DorPres_12, DorPres_12_nw)
DorPres_12$weird <- countrycode(tolower(DorPres_12$country), 'spanish', 'english', custom_dict = custom_dict)
DorPres_12$weird[DorPres_12$country == 'CANADA'] <- 'Canada'
DorPres_12$weird[DorPres_12$country == 'HOLANDA'] <- 'Netherlands'
DorPres_12$weird[DorPres_12$country == 'PANAMA'] <- 'Panama'
DorPres_12$country <- countryname(DorPres_12$weird)
DorPres_12$country[is.na(DorPres_12$country)] <- 'Antillas Menores'
DorPres_12 <- DorPres_12[-c(56)]

## Legislative 2012 
DorLeg_12 <- read_xls("Dom. Rep/2012. Pres. & Leg/Leg. 2012/Resultados por Colegios 2012 Dip. Exterior.xls")
DorLeg_12 <- DorLeg_12[-c(1, 3:10)]
DorLeg_12 <- renamer(DorLeg_12, 1)
DorLeg_12 <- aggregate(c(DorLeg_12[c(2:31)]), by = DorLeg_12["country"], sum)
DorLeg_12 <- slim_fitter(DorLeg_12, "PRD", "ALPAIS")
names(DorLeg_12)[2:7] <- c('registered_voters', 'members', 'valid_votes', 'null_votes',
                            'observed_votes', 'total_votes')
DorLeg_12 <- DorLeg_12[c(1,2,7,4,5,3,6,8,9)]
DorLeg_12$ph <- looper(DorLeg_12)
DorLeg_12 <- DorLeg_12[-c(6,7)]
DorLeg_12_nw <- DorLeg_12 %>% filter(party == 'PRD')
DorLeg_12_nw <- DorLeg_12_nw[c(1,6,7)]
DorLeg_12 <- my_pivoter(DorLeg_12)
DorLeg_12 <- column_sorter(DorLeg_12, 6, 53)
DorLeg_12 <- national_winner(DorLeg_12, DorLeg_12_nw)
DorLeg_12$weird <- countrycode(tolower(DorLeg_12$country), 'spanish', 'english', custom_dict = custom_dict)
DorLeg_12$weird[DorLeg_12$country == 'HOLANDA'] <- 'Netherlands'
DorLeg_12$weird[DorLeg_12$country == 'PANAMA'] <- 'Panama'
DorLeg_12$country <- countryname(DorLeg_12$weird)
DorLeg_12$country[is.na(DorLeg_12$country)] <- 'Antillas Menores'
DorLeg_12 <- DorLeg_12[-c(56)]

## Presidential 2016
DorPres_16 <- read_xlsx("Dom. Rep/2016. Pres/Resultados Electorales 2016 NIVEL A (Presidential).xlsx")
DorPres_16 <- DorPres_16 %>% filter(cod_prov > 40)
# what is penitary vote ? 
DorPres_16 <- DorPres_16[-c(1, 3:11)]
names(DorPres_16)[1:5] <- c('country', 'registered_voters', 'total_votes', 'valid_votes', 'null_votes')
DorPres_16 <- aggregate(c(DorPres_16[c(2:40)]), by = DorPres_16["country"], sum)
DorPres_16 <- slim_fitter(DorPres_16, "PRD", "MAS")
DorPres_16$ph <- looper(DorPres_16)
DorPres_16 <- DorPres_16[-c(6)]
DorPres_16_nw <- DorPres_16 %>% filter(party == 'PLD')
DorPres_16_nw <- DorPres_16_nw[c(1,6,7)]
DorPres_16 <- my_pivoter(DorPres_16)
DorPres_16 <- column_sorter(DorPres_16, 6, 73)
DorPres_16 <- national_winner(DorPres_16, DorPres_16_nw)
DorPres_16$weird <- countrycode(tolower(DorPres_16$country), 'spanish', 'english', custom_dict = custom_dict)
DorPres_16$weird[DorPres_16$country == 'HOLANDA'] <- 'Netherlands'
DorPres_16$weird[DorPres_16$country == 'PANAMA'] <- 'Panama'
DorPres_16 <- DorPres_16 %>% filter(!country == 'VOTO PENITENCIARIO')
DorPres_16$country <- countryname(DorPres_16$weird)
DorPres_16$country[is.na(DorPres_16$country)] <- 'Antillas Menores'


#### Colombia ------------------------------------------------------------------
## Legislative 2018
CoLeg_18 <- read_xls("Colombia/Colombia Leg. 2018/Raw Sources/Colombia Leg. 2018 (Camara de Representes) Raw.xls")
CoLeg_18 <- CoLeg_18[- grep('LUNES|MARTES|MIERCOLES|JUEVES|VIERNES|SABADO', CoLeg_18$Npuesto),]
Col18_check <- CoLeg_18 %>% filter(is.na(partido))
Col18_check <- aggregate(Col18_check['votos'], by = c(Col18_check['NMpio'], Col18_check['candidato']), sum)
Col18_check <- Col18_check %>%
  pivot_wider(names_from = candidato, values_from = votos)
CoLeg_18 <-  CoLeg_18 %>% filter(!is.na(partido))
CoLeg_18 <- aggregate(CoLeg_18['votos'], by = c(CoLeg_18['NMpio'], CoLeg_18['partido']), sum)
names(CoLeg_18) <- c('country', 'party', 'votes')
CoLeg_18 <- party_sorter(CoLeg_18)
CoLeg_18$ph <- looper(CoLeg_18)
CoLeg_18_nw <- CoLeg_18 %>% filter(party == "PARTIDO LIBERAL COLOMBIANO")
CoLeg_18 <- my_pivoter(CoLeg_18)
CoLeg_18 <- column_sorter(CoLeg_18, 2, 87)
CoLeg_18 <- national_winner(CoLeg_18, CoLeg_18_nw)
CoLeg_18 <- CoLeg_18[-c(90)]
# valid votes 
number = c()
for (i in 2:87){
  if (i %% 2 == 0){
    number = append(number, i)
  }
}
CoLeg_18 <- add_column(CoLeg_18, valid_votes = rowSums(CoLeg_18[,c(number)], na.rm = T), .after = "country")
names(Col18_check) <- c('country', 'blanco_votes', 'invalid_votes', 'null_votes')
CoLeg_18 <- left_join(CoLeg_18, Col18_check)
CoLeg_18 <- select(CoLeg_18, country, valid_votes, blanco_votes, invalid_votes, null_votes, everything())
CoLeg_18 <- add_column(CoLeg_18, total_votes = rowSums(CoLeg_18[,c(2:5)], na.rm = T), .after = 'country')
CoLeg_18$weird <- countrycode(tolower(CoLeg_18$country), 'spanish', 'english', custom_dict = custom_dict)
CoLeg_18$weird[CoLeg_18$country == toupper('arzerbaiyan')] <- "Azerbaijan"
CoLeg_18$weird[CoLeg_18$country == toupper('belgica')] <- 'Belgium'
CoLeg_18$weird[CoLeg_18$country == toupper('canada')] <- 'Canada'
CoLeg_18$weird[CoLeg_18$country == toupper('china republica popular')] <- 'China'
CoLeg_18$weird[CoLeg_18$country == toupper('emiratos arabes unidos')] <-  "United Arab Emirates"
CoLeg_18$weird[CoLeg_18$country == toupper('inglaterra')] <- 'UK'
CoLeg_18$weird[CoLeg_18$country == toupper('japon')] <- 'Japan'
CoLeg_18$weird[CoLeg_18$country == toupper('libano')] <- 'Lebanon'
CoLeg_18$weird[CoLeg_18$country == toupper('mexico')] <- 'Mexico'
CoLeg_18$weird[CoLeg_18$country == toupper('nueva zelandia')] <- 'New Zealand'
CoLeg_18$weird[CoLeg_18$country == toupper('paises bajos')] <- "Netherlands"
CoLeg_18$weird[CoLeg_18$country == toupper('panama')] <- 'Panama'
CoLeg_18$weird[CoLeg_18$country == toupper('peru')] <- 'Peru'
CoLeg_18$weird[CoLeg_18$country == toupper('republica de filipinas')] <- "Philippines"
CoLeg_18$weird[CoLeg_18$country == toupper(' republica de singapur')] <- 'Singapore'
CoLeg_18$weird[CoLeg_18$country == toupper('republica dominicana')] <- "Dominican Republic"
CoLeg_18$weird[CoLeg_18$country == toupper('sudafrica')] <- "South Africa"
CoLeg_18$weird[CoLeg_18$country == toupper('turquia')] <- 'Turkey'
CoLeg_18$country <- countryname(CoLeg_18$weird)
CoLeg_18 <- CoLeg_18[-c(95)]

## Pres 2018 first round
CoPres_18 <- read.csv("Colombia/Colombia Pres. 2018/Raw Sources/2018_PRESIDENTE_Y_VICEPRESIDENTE_DE_LA_REPUBLICA_PRIMERA_VUELTA CONSULADOS.csv", sep = ";")
CoPres_18 <- CoPres_18[- grep('LUNES|MARTES|MIERCOLES|JUEVES|VIERNES|SABADO', CoPres_18$puesto),]
CoPres_18_check <- CoPres_18 %>% filter(partido == "")
CoPres_18_check <-  aggregate(CoPres_18_check['votos'], by = c(CoPres_18_check['mpio'], CoPres_18_check['candidato']), sum)
CoPres_18_check <- CoPres_18_check %>%
  pivot_wider(names_from = candidato, values_from = votos)
CoPres_18 <- CoPres_18 %>% filter(!partido == "")
CoPres_18 <- aggregate(CoPres_18['votos'], by = c(CoPres_18['mpio'], CoPres_18['partido']), sum)
names(CoPres_18) <- c('country', 'party', 'votes')#
CoPres_18 <- party_sorter(CoPres_18)
CoPres_18$ph <- looper(CoPres_18)
CoPres_18_nw <- CoPres_18 %>% filter(party == "PARTIDO CENTRO DEMOCRÃ\u0081TICO")
CoPres_18_nw <- CoPres_18_nw[-c(4)]
CoPres_18 <- my_pivoter(CoPres_18)
CoPres_18 <- column_sorter(CoPres_18, 2, 17)
CoPres_18 <- national_winner(CoPres_18, CoPres_18_nw)
number = c()
for (i in 2:17){
  if (i %% 2 == 0){
    number = append(number, i)
  }
}
CoPres_18 <- add_column(CoPres_18, valid_votes = rowSums(CoPres_18[,c(number)], na.rm = T), .after = 'country')
names(CoPres_18_check) <- c('country', 'blanco_votes', 'invalid_votes', 'null_votes')
CoPres_18 <- left_join(CoPres_18, CoPres_18_check)
CoPres_18 <- select(CoPres_18, country, valid_votes, blanco_votes, invalid_votes, null_votes, everything())
CoPres_18 <- add_column(CoPres_18, total_votes = rowSums(CoPres_18[,c(2:5)], na.rm = T), .after = 'country')
CoPres_18$weird <- countrycode(tolower(CoPres_18$country), 'spanish', 'english', custom_dict = custom_dict)
CoPres_18$weird[CoPres_18$country == toupper('arzerbaiyan')] <- "Azerbaijan"
CoPres_18$weird[CoPres_18$country == toupper('belgica')] <- 'Belgium'
CoPres_18$weird[CoPres_18$country == toupper('canada')] <- 'Canada'
CoPres_18$weird[CoPres_18$country == toupper('china republica popular')] <- 'China'
CoPres_18$weird[CoPres_18$country == toupper('emiratos arabes unidos')] <-  "United Arab Emirates"
CoPres_18$weird[CoPres_18$country == toupper('inglaterra')] <- 'UK'
CoPres_18$weird[CoPres_18$country == toupper('japon')] <- 'Japan'
CoPres_18$weird[CoPres_18$country == toupper('libano')] <- 'Lebanon'
CoPres_18$weird[CoPres_18$country == toupper('mexico')] <- 'Mexico'
CoPres_18$weird[CoPres_18$country == toupper('nueva zelandia')] <- 'New Zealand'
CoPres_18$weird[CoPres_18$country == toupper('paises bajos')] <- "Netherlands"
CoPres_18$weird[CoPres_18$country == toupper('panama')] <- 'Panama'
CoPres_18$weird[CoPres_18$country == toupper('peru')] <- 'Peru'
CoPres_18$weird[CoPres_18$country == toupper('republica de filipinas')] <- "Philippines"
CoPres_18$weird[CoPres_18$country == toupper(' republica de singapur')] <- 'Singapore'
CoPres_18$weird[CoPres_18$country == toupper('republica dominicana')] <- "Dominican Republic"
CoPres_18$weird[CoPres_18$country == toupper('sudafrica')] <- "South Africa"
CoPres_18$weird[CoPres_18$country == toupper('turquia')] <- 'Turkey'
CoPres_18$weird[CoPres_18$country == 'ESPAÃ‘A'] <- 'Spain'
CoPres_18$weird[CoPres_18$country == toupper('republica socialista devietnam')] <- 'Vietnam'
CoPres_18$country <- countryname(CoPres_18$weird)
CoPres_18 <- CoPres_18[-c(25)]



### Pasting batch 1 together =================================================== 
# going through every file -> checking if valid votes are there 
# going through longest structure and doing election ID
# but election id is not unique

frPres_07 <- elecid_gen(frPres_07, 'FRA', '07', 'P') 
frPres_12 <- elecid_gen(frPres_12, 'FRA', '12', 'P')
frPres_17 <- elecid_gen(frPres_17, 'FRA', '17', 'P')
RoPres_09 <- elecid_gen(RoPres_09, 'ROU', '09', 'P')
RoPres_14 <- elecid_gen(RoPres_14, 'ROU', '14', 'P')
names(RoLeg_12)[6] <- 'blanco_votes'
RoLeg_12 <- elecid_gen(RoLeg_12, 'ROU', '12', 'L')
BoGe_09 <- elecid_gen(BoGe_09, "BOL", '09', 'G')
BoGe_14 <- elecid_gen(BoGe_14, 'BOL', '14', 'G')
BuPres_16 <- elecid_gen(BuPres_16, "BGR", '16', 'P')
BuLeg_14 <- elecid_gen(BuLeg_14, "BGR", '14', 'L')
BuLeg_17 <- elecid_gen(BuLeg_17, "BGR", '17', 'L')
CrPres_18 <- elecid_gen(CrPres_18, 'CRI', '18', 'P')
BraPres_18 <- elecid_gen(BraPres_18, 'BRA', '18', 'P')
BraPres_14 <- elecid_gen(BraPres_14, 'BRA', '14', 'P')
BraPres_10 <- elecid_gen(BraPres_10, 'BRA', '10', 'P')
BraPres_06 <- elecid_gen(BraPres_06, 'BRA', '06', 'P')
CroParl_03 <- elecid_gen(CroParl_03, "HRV", '03', 'L')
CroParl_07 <- elecid_gen(CroParl_07, "HRV", '07', 'L')
CroParl_11 <- elecid_gen(CroParl_11, "HRV", '11', 'L')
CroParl_15 <- elecid_gen(CroParl_15, "HRV", '15', 'L')
CroParl_16 <- elecid_gen(CroParl_16, "HRV", '16', 'L')
CroPres_05 <- elecid_gen(CroPres_05, "HRV", '05', 'P')
CroPres_09 <- elecid_gen(CroPres_09, "HRV", '09', 'P')
CroPres_14 <- elecid_gen(CroPres_14, "HRV", '14', 'P')
CroPres_19 <- elecid_gen(CroPres_19, "HRV", '19', 'P')
DorPres_04 <- elecid_gen(DorPres_04, "DOM", '04', 'P')
DorPres_08 <- elecid_gen(DorPres_08, "DOM", '08', 'P')
DorPres_12 <- elecid_gen(DorPres_12, "DOM", '12', 'P')
DorPres_16 <- elecid_gen(DorPres_16, "DOM", '16', 'P')
DorLeg_12 <- elecid_gen(DorLeg_12, "DOM", '12', 'L')
CoLeg_18 <- elecid_gen(CoLeg_18, "COL", '18', 'L')
CoPres_18 <- elecid_gen(CoPres_18, "COL", '18', 'P')

CoLeg_18 <- add_column(CoLeg_18, registered_voters = NA, .after = 'country')
names(DorPres_16)[3] <- 'registered_voters'
DorPres_16 <- DorPres_16[-c(77)]
final_df <- bind_rows(CoLeg_18, CoPres_18, frPres_07, frPres_12, frPres_17,
                      RoPres_09, RoPres_14, RoLeg_12, BoGe_09, BoGe_14,
                      BuPres_16, BuLeg_14, BuLeg_17, CrPres_18, BraPres_06, BraPres_10,
                      BraPres_14, BraPres_18, CroParl_03, CroParl_07, CroParl_11, CroParl_15, CroParl_16,
                      CroPres_05, CroPres_09, CroPres_14, CroPres_19, DorPres_04, DorPres_08, DorPres_12, DorPres_16,
                      DorLeg_12)
final_df <- add_column(final_df, iso3c = countrycode(final_df$country, 'country.name', 'iso3c'), .after = 'country')
names(final_df)[2] <- 'host_country'
write.csv(final_df,"evp_v1.csv", row.names = FALSE)
