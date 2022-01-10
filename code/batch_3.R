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
# Setting working directory 
setwd("C:/Users/lenovo/Documents/BSE/RA/Data/Data/EVP")

### Latvia --------------------------------------------------------------------
lat_02 <- read.csv("Latvia/leg_2002/lat_leg_02_1.csv")
lat_02_2 <- read.csv("Latvia/leg_2002/lat_leg_02_2.csv", header = F)

# lat 05 is stupid

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

## also not working // leave that polish election for last 
# scraping for indonesia //
link = "https://pemilu2019.kpu.go.id/#/dprri/hitung-suara/"
page = read_html(link)
indo = page %>% html_nodes('data-table')
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



