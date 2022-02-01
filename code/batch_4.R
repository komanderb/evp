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
bo_19 = bo_19 %>% pivot_wider(names_from = X0, values_from = X1)
# something is weird here -> double names thus pivot did not work that well


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
ind_14 = add_column(ind_14, valid_votes = rowSums(ind_14[2:3]), .after = 'country')
ind_14 = main_function(ind_14, "Prabowo Subianto", "Joko Widodo", 3, "Joko Widodo")
ind_14 = extra_cols(ind_14, 'Indonesia', '2014-07-09', 'Presidential')
countrycode(tolower(ind_14$country), 'indonesian', 'english', custom_dict = custom_dict)
# yeah we have duplicated countries with different names ->  so annoying // 
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