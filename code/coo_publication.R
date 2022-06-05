# extra work 

library(tidyverse)
library(readxl)

setwd("C:/Users/lenovo/Documents/BSE/RA/Data/Data/EVP")
# start with romania 

final_df_list <- c()
country_list <- c()
type_list <- c()
date_list <- c()

RoPres_09 <- read_xls("Romania/Pres. 2009 Romania/Source Files/1st Round/Romania Pres. 2009 1st Round (All Polling Stations).xls")
RoPres_09_coo <- RoPres_09 %>% filter(JUDET != "Strainatate")
RoPres_09_cor <- RoPres_09 %>% filter(JUDET == "Strainatate")
names_ro <- c('ANTONESCU','GEOANA', 'KELEMEN', 'BASESCU', 'OPRESCU', 'BECALI', 'IANE',
              'MANOLE', 'VADIM-TUDOR', 'CERNEA', 'POTIRCA', 'ROTARU')

df_list <- list(RoPres_09_cor, RoPres_09_coo)
for (i in seq_along(df_list)){
  df_list[[i]] <- df_list[[i]][-c(1:3)]
  df_list[[i]] <- as.data.frame(lapply(df_list[[i]][c(5,7, 9:24)],function(y) sum(y)))
  names(df_list[[i]])[7:18] <- names_ro
  df_list[[i]] <- df_list[[i]][-c(1,6)]
  names(df_list[[i]])[1:4] <- c('ACTUAL', 'VALID', 'NULL', 'REGISTERED')
}

df_list[[3]] <- df_list[[1]] + df_list[[2]]

ro_pres09 <- do.call(rbind, df_list)
ro_pres09 <- add_column(ro_pres09, SCOPE = c("ABROAD", "DOMESTIC", "TOTAL"), .before = "ACTUAL")

final_df_list <- append(final_df_list, list(ro_pres09))
country_list <- append(country_list, "Romania")
type_list <- append(type_list, "Presidential")
date_list <- append(date_list, "20091122")
# Pres 2014
RoPres_14 <- read_xls("Romania/Pres. 2014 Romania/Source Files/1st Round/Romania Pres. 2014 1st Round (All Polling Stations).xls")
RoPres_14 <- RoPres_14[-c(32,33)]
RoPres_14_coo <- RoPres_14 %>% filter(`Cod Birou Electoral` != 48)
RoPres_14_cor <- RoPres_14 %>% filter(`Cod Birou Electoral` == 48)

df_list <- list(RoPres_14_coo, RoPres_14_cor)

for (i in seq_along(df_list)){
  df_list[[i]] <- df_list[[i]][-c(1:6)]
  df_list[[i]] <- as.data.frame(lapply(df_list[[i]][c(4:25)],function(y) sum(y)))
  df_list[[i]] <- df_list[[i]][-c(1,2,4,8)]
  names(df_list[[i]])[1:4] <- c('ACTUAL', 'VALID', 'NULL', 'REGISTERED')
}

df_list[[3]] <- df_list[[1]] + df_list[[2]]

ro_pres14 <- do.call(rbind, df_list)
ro_pres14 <- add_column(ro_pres14, SCOPE = c("ABROAD", "DOMESTIC", "TOTAL"), .before = "ACTUAL")

final_df_list <- append(final_df_list, list(ro_pres14))
country_list <- append(country_list, "Romania")
type_list <- append(type_list, "Presidential")
date_list <- append(date_list, "20141102")


# Poland -----------------------------------------------------------------------
# 2019 leg, same code as in repo but minor changes 
pol_19 <- read.csv("Poland/Poland Leg. 2019 Raw Data File.csv", sep = ";", encoding = "UTF-8")
pol_19_cor <- pol_19 %>% filter(Gmina == "zagranica")
pol_19 <- pol_19 %>% filter(Gmina != "zagranica")
df_list <- list(pol_19, pol_19_cor)
for (i in seq_along(df_list)){
  df_list[[i]] <- df_list[[i]][-c(1:10, 41,42)]
  df_list[[i]][1:30] <- lapply(df_list[[i]][1:30], function(y) as.numeric(y))
  # some NA introduced for missing values 
  names(df_list[[i]])[c(2, 17, 18, 22)] <- c('REGISTERED', "TOTAL", "INVALID", "VALID")
  df_list[[i]] <- df_list[[i]][c(2, 17, 18, 22:30)]
  # removing na party columns
  df_list[[i]] <- df_list[[i]][-c(6,9,11)]
  # aggregate for country
  df_list[[i]] <- as.data.frame(lapply(df_list[[i]][c(1:9)],function(y) sum(y, na.rm = T)))
  
  names(df_list[[i]])[5:9] <- c("KOALICJA OBYWATELSKA", "KONFEDERACJA WOLNOSC I NIEPODLEGLOSC", 
                          "POLSKIE STRONNICTWO LUDOWE", "PRAWO I SPRAWIEDLIWOSC", "SOJUSZ LEWICY DEMOKRATYCZNEJ")
}

df_list[[3]] <- df_list[[1]] + df_list[[2]]


pol_leg19 <- do.call(rbind, df_list)
pol_leg19 <- add_column(pol_leg19, SCOPE = c("DOMESTIC","ABROAD",  "TOTAL"), .before = "REGISTERED")
final_df_list <- append(final_df_list, list(pol_leg19))
country_list <- append(country_list, "Poland")
type_list <- append(type_list, "Legislative")
date_list <- append(date_list, "20191013")


# scraped  
setwd("C:/Users/lenovo/Documents/BSE/RA/Data/Data/EVP/pub_coo")

# POl PRES 00
pol_00 <- read.csv("POL_PRES_00.csv", encoding = "UTF-8")
pol_00$X1 <- as.numeric(gsub(" ", "", pol_00$X1))
pol_00 <- pol_00[-1]
pol_00 <- pol_00 %>% pivot_wider(names_from = X0, values_from = X1)
names(pol_00) <- iconv(names(pol_00), from = "UTF-8", to = 'ASCII//TRANSLIT')
names(pol_00)
pol_00 <- add_column(pol_00, SCOPE = "DOMESTIC", .before = "GRABOWSKI Dariusz")
pol_00 <- add_column(pol_00, REGISTERED = 29122304, .after = "SCOPE")
# here a bit unsure whateve 
pol_00 <- add_column(pol_00, TOTAL = 17789231,.after = "REGISTERED")
pol_00 <- add_column(pol_00, VALID = 17598919,.after = "TOTAL")
pol_00 <- add_column(pol_00, INVALID = 190312,.after = "VALID")

final_df_list <- append(final_df_list, list(pol_00))
country_list <- append(country_list, "Poland")
type_list <- append(type_list, "Presidential")
date_list <- append(date_list, "20001008")


# 
pol_pres_10 <- read.csv("POL_PRES_10.csv", encoding = "UTF-8")
pol_pres_10$X4 <- as.numeric(pol_pres_10$X4)
pol_pres_10 <- pol_pres_10[-1]
pol_pres_10$X3 <- iconv(pol_pres_10$X3, from = "UTF-8", to = 'ASCII//TRANSLIT')
pol_pres_10 <- pol_pres_10 %>% pivot_wider(names_from = X3, values_from = X4)
names(pol_pres_10)
pol_pres_10 <- add_column(pol_pres_10, SCOPE = "DOMESTIC", .before = "JUREK Marek")
pol_pres_10 <- add_column(pol_pres_10, REGISTERED = 30813005, .after = "SCOPE")
pol_pres_10 <- add_column(pol_pres_10, TOTAL = 16923832, .after = "REGISTERED")
pol_pres_10 <- add_column(pol_pres_10, VALID = 16806170	, .after = "TOTAL")

final_df_list <- append(final_df_list, list(pol_pres_10))
country_list <- append(country_list, "Poland")
type_list <- append(type_list, "Presidential")
date_list <- append(date_list, "20100620")

# pol pres 15 
pol_pres_15 <- read.csv("POL_PRES_15.csv", encoding = "UTF-8")
pol_pres_15 <- pol_pres_15[-1]
pol_pres_15$Imie.i.nazwisko <- iconv(pol_pres_15$Imie.i.nazwisko, from = "UTF-8", to = 'ASCII//TRANSLIT')
pol_pres_15 <- pol_pres_15 %>% pivot_wider(names_from = Imie.i.nazwisko, values_from = Liczba.oddanych.glosów)
pol_pres_15 <- add_column(pol_pres_15, SCOPE = "DOMESTIC", .before = "Braun Grzegorz Michal")
pol_pres_15 <- add_column(pol_pres_15, REGISTERED = 30688570, .after = "SCOPE")
pol_pres_15 <- add_column(pol_pres_15, TOTAL =  15023886,.after = "REGISTERED")
pol_pres_15 <- add_column(pol_pres_15, VALID =  14898934, .after = "TOTAL")
pol_pres_15 <- add_column(pol_pres_15, INVALID = 124952,.after = "VALID")

final_df_list <- append(final_df_list, list(pol_pres_15))
country_list <- append(country_list, "Poland")
type_list <- append(type_list, "Presidential")
date_list <- append(date_list, "20150510")

# pol leg 11

pol_leg_11 <- read.csv("POL_LEG_11.csv", encoding = "UTF-8")
pol_leg_11e <- read.csv("POL_LEG_11extra.csv", encoding = "UTF-8")

pol_leg_11 <- pol_leg_11[-1]
pol_leg_11$X1 <- iconv(pol_leg_11$X1, from = "UTF-8", to = 'ASCII//TRANSLIT')
pol_leg_11 <- pol_leg_11 %>% pivot_wider(names_from = X1, values_from = X2)
pol_leg_11e <- pol_leg_11e[-c(1,3)]
pol_leg_11 <- cbind(pol_leg_11e, pol_leg_11)
pol_leg_11 <- add_column(pol_leg_11, SCOPE = "DOMESTIC", .before = "REGISTERED")

final_df_list <- append(final_df_list, list(pol_leg_11))
country_list <- append(country_list, "Poland")
type_list <- append(type_list, "Legislative")
date_list <- append(date_list, "20111009")

# pol leg 15 

pol_leg_15 <- read.csv("POL_LEG_15.csv", encoding = "UFT-8")
pol_leg_15 <- pol_leg_15[-1]
pol_leg_15$Zdobyte.gÅ.osy <- as.numeric(gsub(" ", "", pol_leg_15$Zdobyte.gÅ.osy))
pol_leg_15$Nazwa.komitetu <- iconv(pol_leg_15$Nazwa.komitetu, from = "UTF-8", to = 'ASCII//TRANSLIT')
pol_leg_15 <- pol_leg_15 %>% pivot_wider(names_from = Nazwa.komitetu, values_from = Zdobyte.gÅ.osy)
pol_leg_15 <- add_column(pol_leg_15, SCOPE = "DOMESTIC", .before = "Komitet Wyborczy Prawo i Sprawiedliwosc")
pol_leg_15 <- add_column(pol_leg_15, REGISTERED = 30629150, .after = "SCOPE")
pol_leg_15 <- add_column(pol_leg_15, VALID =  15200671, .after = "REGISTERED")
pol_leg_15 <- add_column(pol_leg_15, INVALID = 394664,.after = "VALID")

final_df_list <- append(final_df_list, list(pol_leg_15))
country_list <- append(country_list, "Poland")
type_list <- append(type_list, "Legislative")
date_list <- append(date_list, "20151025")

# Romania ---------------------------------------------------------------------
# Legislative
ro_maker <- function(csv_path, extra_path){
  df <- read.csv(csv_path, encoding = "UTF-8")
  df_e <- read.csv(extra_path, encoding = "UTF-8")
  
  df_e <- df_e[ - 3, -c(1:3)]
  names(df_e) <- c('name', 'value')
  df_e$name <- c("REGISTERED", "TOTAL", "VALID", "NULL")
  df_e$value <- as.numeric(df_e$value)
  df <- df[-c(1,3,5)]
  names(df) <- c("name", "value")
  df$name <- iconv(df$name, from = "UTF-8", to = 'ASCII//TRANSLIT')
  df <- bind_rows(df_e, df)
  df <- df %>% pivot_wider(names_from = name, values_from = value)
  df <- add_column(df, SCOPE = "DOMESTIC", .before = "REGISTERED")
  return(df)
}

leg_00 <- "RO_LEG_00.csv"
leg_00e <- "RO_LEG_00_extra.csv"
rol_leg_00 <- ro_maker(leg_00, leg_00e)

final_df_list <- append(final_df_list, list(rol_leg_00))
country_list <- append(country_list, "Romania")
type_list <- append(type_list, "Legislative")
date_list <- append(date_list, "20001126")

# leg 04 

leg_04 <- "RO_LEG_04.csv"
leg_04e <- "RO_LEG_04_extra.csv"
rol_leg_04 <- ro_maker(leg_04, leg_04e)

final_df_list <- append(final_df_list, list(rol_leg_04))
country_list <- append(country_list, "Romania")
type_list <- append(type_list, "Legislative")
date_list <- append(date_list, "20041128")

# leg 08 
leg_08 <- "RO_LEG_08.csv"
leg_08e <- "RO_LEG_08_extra.csv"
rol_leg_08 <- ro_maker(leg_08, leg_08e)

final_df_list <- append(final_df_list, list(rol_leg_08))
country_list <- append(country_list, "Romania")
type_list <- append(type_list, "Legislative")
date_list <- append(date_list, "20081130")

# leg 12
leg_12 <- "RO_LEG_12.csv"
leg_12e <- "RO_LEG_12_extra.csv"
rol_leg_12 <- ro_maker(leg_12, leg_12e)

final_df_list <- append(final_df_list, list(rol_leg_12))
country_list <- append(country_list, "Romania")
type_list <- append(type_list, "Legislative")
date_list <- append(date_list, "20121209")


# actually these two were not in evp data? what was I doing?? 
# pres 00
pres_00 <- "RO_PRES_00.csv"
pres_00e <- "RO_PRES_00_extra.csv"
ro_pres_00 <- ro_maker(pres_00, pres_00e)


# pres 04
pres_04 <- "RO_PRES_04.csv"
pres_04e <- "RO_PRES_04_extra.csv"
ro_pres_04 <- ro_maker(pres_04, pres_04e)




# Serbia ----------------------------------------------------------------------
# Pres 08
ser_pres_08 <- read_xlsx("SER_PRES_08.xlsx")
names(ser_pres_08) <- iconv(names(ser_pres_08), from = "UTF-8", to = 'ASCII//TRANSLIT')
final_df_list <- append(final_df_list, list(ser_pres_08))
country_list <- append(country_list, "Serbia")
type_list <- append(type_list, "Presidential")
date_list <- append(date_list, "20080120")

# Pres 12 
ser_pres_12 <- read.csv("SER_PRES_12 - Sheet1.csv", encoding = "UTF-8")
ser_pres_12$VOTES <- as.numeric(gsub(",", "", ser_pres_12$VOTES))
ser_pres_12$CANDIDATE <- iconv(ser_pres_12$CANDIDATE, from = "UTF-8", to = 'ASCII//TRANSLIT')
ser_pres_12 <- ser_pres_12 %>% pivot_wider(names_from = CANDIDATE, values_from = VOTES)
ser_pres_12 <- add_column(ser_pres_12, SCOPE ="DOMESTIC", .before = "REGISTERED")

final_df_list <- append(final_df_list, list(ser_pres_12))
country_list <- append(country_list, "Serbia")
type_list <- append(type_list, "Presidential")
date_list <- append(date_list, "20120506")

# Pres 17 
ser_pres_17 <- read_xlsx("SER_PRES_17.xlsx", col_names = F)
ser_pres_17$...1 <- iconv(ser_pres_17$...1, from = "UTF-8", to = 'ASCII//TRANSLIT')
ser_pres_17 <- ser_pres_17 %>% pivot_wider(names_from = ...1, values_from = ...2)
ser_pres_17 <- add_column(ser_pres_17, SCOPE ="DOMESTIC", .before = "REGISTERED")

final_df_list <- append(final_df_list, list(ser_pres_17))
country_list <- append(country_list, "Serbia")
type_list <- append(type_list, "Presidential")
date_list <- append(date_list, "20170402")

# Leg 12 
ser_leg_12 <- read_xlsx("SER_LEG_12.xlsx", col_names = F)
ser_leg_12$...1 <- iconv(ser_leg_12$...1, from = "UTF-8", to = 'ASCII//TRANSLIT')
ser_leg_12$...2 <- as.numeric(gsub("\\.", "", as.character(ser_leg_12$...2)))
ser_leg_12 <- ser_leg_12 %>% pivot_wider(names_from = ...1, values_from = ...2)
ser_leg_12 <- add_column(ser_leg_12, SCOPE ="DOMESTIC", .before = "REGISTERED")

final_df_list <- append(final_df_list, list(ser_leg_12))
country_list <- append(country_list, "Serbia")
type_list <- append(type_list, "Legislative")
date_list <- append(date_list, "20120506")

# Leg 14 
ser_leg_14 <- read_xlsx("SER_LEG_14.xlsx", col_names = F)
ser_leg_14$...1 <- iconv(ser_leg_14$...1, from = "UTF-8", to = 'ASCII//TRANSLIT')
ser_leg_14$...2 <- as.numeric(gsub("\\.", "", as.character(ser_leg_14$...2)))
ser_leg_14 <- ser_leg_14 %>% pivot_wider(names_from = ...1, values_from = ...2)
ser_leg_14 <- add_column(ser_leg_14, SCOPE ="DOMESTIC", .before = "REGISTERED")

final_df_list <- append(final_df_list, list(ser_leg_14))
country_list <- append(country_list, "Serbia")
type_list <- append(type_list, "Legislative")
date_list <- append(date_list, "20140316")

# Leg 16 not working= 
# Ukraine ---------------------------------------------------------------------
# Leg 02 
ukr_leg_02 <- read_xlsx("UKR_LEG_02total.xlsx", col_names = F)
ukr_leg_02$...2 <-as.numeric(gsub(" |\\,", "", ukr_leg_02$...2))
valid_ukraine <- sum(ukr_leg_02$...2[5:37])
ukr_leg_02 <- ukr_leg_02 %>% pivot_wider(names_from = ...1, values_from = ...2)
ukr_leg_02 <- add_column(ukr_leg_02, SCOPE = "TOTAL", .before = "REGISTERED")
ukr_leg_02 <- add_column(ukr_leg_02, VALID = valid_ukraine, .after = "TOTAL")

final_df_list <- append(final_df_list, list(ukr_leg_02))
country_list <- append(country_list, "Ukraine")
type_list <- append(type_list, "Legislative")
date_list <- append(date_list, "20020331")

# Leg 06
ukr_leg_06 <- read_xlsx("UKR_LEG_06.xlsx")
ukr_leg_06$`Votes FOR` <- as.numeric(gsub(" |\\,", "", ukr_leg_06$`Votes FOR`))
valid_ukraine <- sum(ukr_leg_06$`Votes FOR`)
ukr_leg_06 <- ukr_leg_06 %>% pivot_wider(names_from = `Party (bloc)`, values_from = `Votes FOR`)
ukr_leg_06 <- add_column(ukr_leg_06, SCOPE = "DOMESTIC", .before = "Party of Regions")
ukr_leg_06 <- add_column(ukr_leg_06, VALID = valid_ukraine, .after = "SCOPE")

final_df_list <- append(final_df_list, list(ukr_leg_06))
country_list <- append(country_list, "Ukraine")
type_list <- append(type_list, "Legislative")
date_list <- append(date_list, "20060326")


# Leg 07
ukr_leg_07 <- read_xlsx("UKR_LEG_07.xlsx")
ukr_leg_07$Schedule <- as.numeric(gsub(" |\\,", "", ukr_leg_07$Schedule))
valid_ukraine <- sum(ukr_leg_07$Schedule)
ukr_leg_07 <- ukr_leg_07 %>% pivot_wider(names_from = `Party (bloc)`, values_from = Schedule)
ukr_leg_07 <- add_column(ukr_leg_07, SCOPE = "DOMESTIC", .before = "Party of Regions")
ukr_leg_07 <- add_column(ukr_leg_07, VALID = valid_ukraine, .after = "SCOPE")

final_df_list <- append(final_df_list, list(ukr_leg_07))
country_list <- append(country_list, "Ukraine")
type_list <- append(type_list, "Legislative")
date_list <- append(date_list, "20070930")

# Leg 12
ukr_leg_12 <- read_xlsx("UKR_LEG_12.xlsx")
ukr_leg_12$`Votes "FOR"` <- as.numeric(gsub(" |\\,", "", ukr_leg_12$`Votes "FOR"`))
valid_ukraine <- sum(ukr_leg_12$`Votes "FOR"`)
ukr_leg_12 <- ukr_leg_12 %>% pivot_wider(names_from = Party, values_from = `Votes "FOR"`)
ukr_leg_12 <- add_column(ukr_leg_12, SCOPE = "DOMESTIC", .before = "PARTY OF REGIONS")
ukr_leg_12 <- add_column(ukr_leg_12, VALID = valid_ukraine, .after = "SCOPE")

final_df_list <- append(final_df_list, list(ukr_leg_12))
country_list <- append(country_list, "Ukraine")
type_list <- append(type_list, "Legislative")
date_list <- append(date_list, "20121028")

# Leg 14 
ukr_leg_14 <- read_xlsx("UKR_LEG_14.xlsx")
ukr_leg_14$`Votes "FOR"` <- as.numeric(gsub(" |\\,", "", ukr_leg_14$`Votes "FOR"`))
valid_ukraine <- sum(ukr_leg_14$`Votes "FOR"`)
ukr_leg_14 <- ukr_leg_14 %>% pivot_wider(names_from = Party, values_from = `Votes "FOR"`)
ukr_leg_14 <- add_column(ukr_leg_14, SCOPE = "DOMESTIC", .before =  "NARODNYI FRONT political party")
ukr_leg_14 <- add_column(ukr_leg_14, VALID = valid_ukraine, .after = "SCOPE")

final_df_list <- append(final_df_list, list(ukr_leg_14))
country_list <- append(country_list, "Ukraine")
type_list <- append(type_list, "Legislative")
date_list <- append(date_list, "20141026")

# Pres 10
ukr_pres_10 <-read_xlsx("UKR_PRES_10.xlsx", col_names = F)
ukr_pres_10$...1[1:3] <- c("TOTAL", "INVALID", "NULL")
ukr_pres_10$...2[2:3] <- c(405789 ,542819)
ukr_pres_10$...2 <- as.numeric(gsub(" |\\.|\\,", "", ukr_pres_10$...2))
valid_ukraine <- sum(ukr_pres_10$...2[4:21])
ukr_pres_10 <- ukr_pres_10 %>% pivot_wider(names_from = ...1, values_from = ...2)
ukr_pres_10 <- add_column(ukr_pres_10, SCOPE = "DOMESTIC", .before =  "TOTAL")
ukr_pres_10 <- add_column(ukr_pres_10, VALID = valid_ukraine, .after = "TOTAL")

final_df_list <- append(final_df_list, list(ukr_pres_10))
country_list <- append(country_list, "Ukraine")
type_list <- append(type_list, "Presidential")
date_list <- append(date_list, "20100117")


# Pres 14
ukr_pres_14 <-read_xlsx("UKR_PRES_14.xlsx", col_names = F)
ukr_pres_14$...1[1:2] <- c("TOTAL", "INVALID")
ukr_pres_14$...2[2] <- 244659	
ukr_pres_14 <- ukr_pres_14[-3,]
ukr_pres_14$...2 <- as.numeric(gsub(" |\\.|\\,", "", ukr_pres_14$...2))

valid_ukraine <- sum(ukr_pres_14$...2[3:23])
ukr_pres_14 <- ukr_pres_14 %>% pivot_wider(names_from = ...1, values_from = ...2)
ukr_pres_14 <- add_column(ukr_pres_14, SCOPE = "DOMESTIC", .before =  "TOTAL")
ukr_pres_14 <- add_column(ukr_pres_14, VALID = valid_ukraine, .after = "TOTAL")

final_df_list <- append(final_df_list, list(ukr_pres_14))
country_list <- append(country_list, "Ukraine")
type_list <- append(type_list, "Presidential")
date_list <- append(date_list, "20140525")

# Pres 19
ukr_pres_19 <-read_xlsx("UKR_PRES_19.xlsx", col_names = F)
ukr_pres_19$...2 <- as.numeric(gsub(" |\\,", "", as.character(ukr_pres_19$...2)))
valid_ukraine <- sum(ukr_pres_19$...2[4:42])
ukr_pres_19 <- ukr_pres_19 %>% pivot_wider(names_from = ...1, values_from = ...2)
ukr_pres_19 <- add_column(ukr_pres_19, SCOPE = "DOMESTIC", .before =  "REGISTERED")
ukr_pres_19 <- add_column(ukr_pres_19, VALID = valid_ukraine, .after = "TOTAL")



final_df_list <- append(final_df_list, list(ukr_pres_14))
country_list <- append(country_list, "Ukraine")
type_list <- append(type_list, "Presidential")
date_list <- append(date_list, "20190331")


# WRITE EVERYTHING ------------------------------------------------------------
library(openxlsx)
`%notin%` <- Negate(`%in%`)
type_list <- gsub("Legislative", "Leg", type_list)
type_list <- gsub("Presidential", "Pres", type_list)

setwd("G:/.shortcut-targets-by-id/1BiiqfA9L9DaS6ELE2OPJkvYdmM6Ws-x6/EVP. Data/publication_data")

write_coo <- function(list_df, list_country, list_type, list_date){
  check_cols <- c("REGISTERED", "ACTUAL", "NULL", "BLANK", "INVALID")
  for (i in seq_along(list_df)){
    names(list_df[[i]])[names(list_df[[i]]) == "TOTAL"] <- "ACTUAL"
    for (column in check_cols){
      if (column %notin% names(list_df[[i]])){
        
        list_df[[i]][, column] <- NA
        
      }
    }
    
    
    list_df[[i]] <- list_df[[i]] %>% select("SCOPE", "REGISTERED", "ACTUAL", 
                                            "VALID", "NULL", "BLANK", "INVALID",
                                            everything())
    
    name_string = paste(list_country[i], list_date[i], list_type[i], "COO.xlsx",
                        sep = "_")
    
    # write xlsx 
    write.xlsx(list_df[[i]], file = name_string)
  }
}


write_coo(final_df_list, country_list, type_list, date_list)











