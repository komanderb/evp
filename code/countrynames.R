### This file is solely for renaming countries
# Some beginnings first
###
custom_dict <- data.frame(portugese = countrycode::codelist$cldr.name.pt,
                          english = countrycode::codelist$cldr.name.en,
                          romanian = countrycode::codelist$cldr.name.ro,
                          french = countrycode::codelist$cldr.name.fr,
                          croatia = codelist$cldr.name.hr,
                          spanish = codelist$cldr.name.es,
                          stringsAsFactors = FALSE)
##
custom_dict$spanish <- tolower(custom_dict$spanish)
custom_dict$english <- tolower(custom_dict$english)
custom_dict$romanian <- tolower(custom_dict$romanian)
custom_dict$french <- tolower(custom_dict$french)
custom_dict$croatia <- tolower(custom_dict$croatia)


### France stuff: 
france_dic <- read_xlsx("France/France Leg. 2012 Data.xlsx")

france_dic <- france_dic[c(2,3)]
france_dic <- row_to_names(france_dic, row_number = 2)



n_1 <- c('BENGLADESH', 'BOLIVIE', 'BURUNDI (dont RWANDA)', 'CENTRAFRICAINE (Rép.)',  'CONGO (Rép. démocratiq.)', 'COTE D`IVOIRE', 'DOMINICAINE (Rép.)', "ETATS-UNIS D'AMERIQUE",
         "ISLANDE" , "JAMAÏQUE", "MACEDOINE", "MALAISIE", "MAROC", "NIGER" , "PAPOUASIE N-GUINEE", "SLOVAQUIE", "TCHEQUE (Rép.)",
         "BELGIQUE",  "DOMINICAINE (REPUBLIQUE)", "FIDJI" , "IRLANDE",  "TAIWAN", 'CHINE')
n_2 <- c("Bangladesh", "Bolivia", "Burundi", "Central African Republic",  "Congo - Kinshasa", "Côte d’Ivoire",  "Dominican Republic", "US", "Iceland", "Jamaica", 
         "North Macedonia",  "Malaysia", "Morocco", "Niger",  "Papua New Guinea", "Slovakia", "Czechia",  "Belgium", "Dominican Republic",
         "Fiji",  "Ireland", "Taiwan", 'China')
france_dic_2 <- cbind(n_1, n_2)
france_dic_2 <- as_data_frame(france_dic_2)
names(france_dic_2) <- c("CoR_Original", "CoR_English")
france_dic <- rbind(france_dic, france_dic_2)
france_dic <- france_dic %>% filter(!duplicated(CoR_Original))
france_dic$CoR_English[france_dic$CoR_English == 'Green Cap'] <- "Cape Verde"
france_dic$CoR_English[france_dic$CoR_English == 'Benign'] <- "Benin"


## 07
frPres_07$country  <- gsub("^\\s+|\\s+$", "", frPres_07$country)
frPres_07 <- frPres_07 %>% filter(!country == 'TOTAUX')
frPres_07$weird <- countrycode( frPres_07$country, "CoR_Original", "CoR_English", custom_dict = france_dic)
frPres_07$weird2 <- countryname(frPres_07$weird)
frPres_07$weird2[frPres_07$weird == 'Jerusalem'] <- 'Jerusalem'
frPres_07$country <- frPres_07$weird2
frPres_07 <- frPres_07[-c(31,32)]
# 12
frPres_12$country  <- gsub("^\\s+|\\s+$", "", frPres_12$country)
frPres_12 <- frPres_12 %>% filter(!country == 'TOTAUX')
frPres_12$weird <- countrycode(frPres_12$country,  "CoR_Original", "CoR_English", custom_dict = france_dic)
frPres_12$weird2 <- countryname(frPres_12$weird)
frPres_12$weird2[frPres_12$weird == 'Jerusalem'] <- 'Jerusalem'
frPres_12$country <- frPres_12$weird2
frPres_12 <- frPres_12[-c(27,28)]

## 17 
frPres_17$weird <- countryname(frPres_17$country)
frPres_17$weird[frPres_17$country == 'Benign'] <- 'Benin'
frPres_17$weird[frPres_17$country == 'Central African (Republic)'] <- 'Central African Republic'
frPres_17$weird[frPres_17$country == 'Jerusalem'] <- 'Jerusalem'
frPres_17$country <- frPres_17$weird 
frPres_17 <- frPres_17[-c(27)]


## legislative elections are missing // 
## Romania: 

rom_dic <- read_xlsx("Romania/Leg. 2012 Romania/Romania Leg. 2012 Data.xlsx")
rom_dic <- rom_dic[c(1,2,3)]
rom_dic <- row_to_names(rom_dic, row_number = 2)

RoPres_09$weird <- countrycode(RoPres_09$country,  "CoR_original", "CoR_english", custom_dict = rom_dic)
RoPres_09$weird[RoPres_09$country == 'BOSNIA - HERZEGOVINA'] <- "Bosnia"
RoPres_09$weird[RoPres_09$country == 'KUWEIT'] <- "Kuwait"
RoPres_09$weird[RoPres_09$country == 'MALAEZIA'] <- "Malaysia"
RoPres_09$weird[RoPres_09$country == 'REP. COREEA'] <- "South Korea"
RoPres_09$weird[RoPres_09$country == 'REP. MOLDOVA'] <- "Moldova"
RoPres_09$weird[RoPres_09$country == 'REP. POPULARA CHINEZA'] <- "China"
RoPres_09$weird[RoPres_09$country == 'RPD COREEANA'] <- "North Korea"
RoPres_09$weird[RoPres_09$country == 'RUSIA'] <- "Russia"
RoPres_09$weird[RoPres_09$country == 'S.U.A.'] <- "US"
RoPres_09$weird2 <- countryname(RoPres_09$weird)
RoPres_09$weird2[RoPres_09$country == 'Teatrul de operatiuni AFGANISTAN'] <- 'Afghanistan MB'
RoPres_09$weird2[RoPres_09$country == 'Teatrul de operatiuni KOSOVO'] <- 'Kosovo MB'
RoPres_09$country <- RoPres_09$weird2
RoPres_09 <- RoPres_09[-c(32,33)]

RoPres_14$weird <-  countrycode(toupper(RoPres_14$country),  "CoR_original", "CoR_english", custom_dict = rom_dic)
## problematic but no way around it: 
RoPres_14$weird[toupper(RoPres_14$country) == 'BOSNIA HERTEGOVINA'] <- "Bosnia"
RoPres_14$weird[toupper(RoPres_14$country) == 'KUWEIT'] <- "Kuwait"
RoPres_14$weird[toupper(RoPres_14$country) == 'MALAEZIA'] <- "Malaysia"
##
RoPres_14$weird[toupper(RoPres_14$country) == 'REPUBLICA COREEA (COREEA DE SUD)'] <- "South Korea"
RoPres_14$weird[toupper(RoPres_14$country) == 'REPUBLICA MOLDOVA'] <- "Moldova"
RoPres_14$weird[toupper(RoPres_14$country) == 'REPUBLICA CHINEZA'] <- "China"
RoPres_14$weird[toupper(RoPres_14$country) == 'R.P.D. COREEANA (COREEA DE NORD)'] <- "North Korea"
RoPres_14$weird[toupper(RoPres_14$country) == 'FEDERATIA RUSA'] <- "Russia"
RoPres_14$weird[toupper(RoPres_14$country) == 'SUA'] <- "US"
RoPres_14$weird[toupper(RoPres_14$country) == 'AFGANISTAN'] <- '"Afghanistan"'
RoPres_14$weird[toupper(RoPres_14$country) == 'CROATIA'] <-  "Croatia"
RoPres_14$weird[toupper(RoPres_14$country) == 'ELVETIA'] <-  "Switzerland" 
RoPres_14$weird[toupper(RoPres_14$country) == 'FRANTA'] <- 'France'
RoPres_14$weird[toupper(RoPres_14$country) == 'LETONIA'] <- 'Latvia'
RoPres_14$weird[toupper(RoPres_14$country) == 'PALESTINA'] <- "Palestine"
RoPres_14$weird[toupper(RoPres_14$country) == 'REPUBLICA ANGOLA'] <-  "Angola"
RoPres_14$weird[toupper(RoPres_14$country) == 'REPUBLICA ARABA SIRIANA'] <- 'Syria'
RoPres_14$weird[toupper(RoPres_14$country) == 'REPUBLICA BELARUS'] <- "Belarus"
RoPres_14$weird[toupper(RoPres_14$country) == 'REPUBLICA BULGARIA'] <- "Bulgaria"
RoPres_14$weird[toupper(RoPres_14$country) == 'REPUBLICA CEHA'] <- 'Czechia'
RoPres_14$weird[toupper(RoPres_14$country) == 'REPUBLICA FEDERALA GERMANIA'] <- 'Germany'
RoPres_14$weird[toupper(RoPres_14$country) == 'REPUBLICA INDONEZIA'] <- "Indonesia"
RoPres_14$weird[toupper(RoPres_14$country) == 'REPUBLICA ISLAMICA IRAN'] <- "Iran"
RoPres_14$weird[toupper(RoPres_14$country) == 'REPUBLICA SAN MARINO'] <- "San Marino"
RoPres_14$weird[toupper(RoPres_14$country) == 'REPUBLICA TURCIA'] <- "Turkey"
RoPres_14$weird2 <- countryname(RoPres_14$weird)
RoPres_14$country <- RoPres_14$weird2
RoPres_14 <- RoPres_14[-c(36,37)]

## Leg 12 
RoLeg_12$weird <- countrycode(RoLeg_12$country,  "CoR_original", "CoR_english", custom_dict = rom_dic)
RoLeg_12$weird[RoLeg_12$country == 'S.U.A.'] <- "US"
RoLeg_12$weird[RoLeg_12$country == 'AFGANISTAN'] <- '"Afghanistan"'
RoLeg_12$weird[RoLeg_12$country == 'REP. MOLDOVA'] <- "Moldova"
RoLeg_12$weird2 <- countryname(RoLeg_12$weird)
RoLeg_12$country <- RoLeg_12$weird2
RoLeg_12 <- RoLeg_12[-c(59,60)]


#### some stuff for brazil ----------------------------------------------------
## try to ignore it, just left it in 
country_names <- cbind(x_2, country_names$iso2a)
names(country_names) <- c('city_origin', 'iso2a')
country_names[60,2] <- 'CA'
country_names[111,2] <- 'NA'
country_names <- as.data.frame(country_names)
x <- BraPres_18$NM_MUNICIPIO
x_2 <- unique(x)
x_2 <- tolower(x_2)
## now we need city to country function
setwd("C:/Users/lenovo/Documents/BSE/RA/Data")
language.codes <- read.delim("helping_data/iso-languagecodes.txt")
language.codes %>% filter(Language.Name == "Brasil")
geo_city <- read.delim('helping_data/alternateNames.txt') ## btw 15 million observations
names(geo_city)
geo_city <- geo_city %>% filter(X == 'pt')
geo_city <- geo_city[-c(1, 5:8)]
geo_city <- geo_city[-c(2)]
geo_city <- geo_city[,c(2,1)]
geo_city_2 <- geo_city[c(1,3)]
geo_city_2 <- geo_city_2 %>% filter(!(is.na(iso2)))
names(geo_city) <- c('city', 'geocode')
names(geo_city_15) <- c('geocode', 'iso2')
geo_city <- left_join(geo_city, geo_city_15, by ='geocode')
geo_city <- geo_city %>% filter(!(duplicated(city)))
geo_city$city <- tolower(geo_city$city)

geo_city_15 <- read_delim("helping_data/cities15000.txt", col_names = F)
geo_city_15 <- geo_city_15[c(1,9)]
help("read_delim")
geo_city_15 
geo_city_2 <- geo_city_2 %>% filter(!(duplicated(city)))
x_3 <- countrycode(x_2, 'city', 'iso2', custom_dict =geo_city_2)
x_4 <- cbind(x_2, x_3)
write.csv(x_4, 'countrynames.csv')
country_names <- read.csv('helping_data/countrynames - countrynames.csv')
country_names <- country_names[-c(1)]
v1 <- c('amsterdã', 'copenhague', 'guatemala-guat', 'hong kong', 'kingston-jama', 'mexico', 'mitsukaido-japa', 'oizumi-japa', 'panama', 'ramallah', 'santa cruz de la sierra', 'singapura', 'suzuka-japa', 'takaoka-japa', 'toyohashi-japa', 'ueda-japa', 'windhoek'
)
v2 <- c("NL", 'DK', 'GT', 'HK', 'JM', 'MX', 'JP', 'JP', 'PA', 'PS', 'BO', 'SG', 'JP', 'JP', 'JP', 'JP', 'NA')
## not sure which one // 
x_5 <- cbind(v1, v2)
x_5 <- as.data.frame(x_5)
names(x_5) <- c('city_origin', 'iso2a')
x_5 <- x_5[-c(17),]
country_names <- rbind(country_names, x_5)

(cz_17, cz_13, Hon_01, Hon_05, Hon_09, Hon_13, Hon_17, fr_02, bra_02, col_02, 
  col_14, ita_13, mozl_04, mozp_04, mozl_09, mozp_09, mozl_14, mozp_14)