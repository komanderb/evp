# -*- coding: utf-8 -*-
"""
Created on Thu Dec 30 11:37:54 2021

@author: lenovo
"""
!pip install matplotlib
import matplotlib
import camelot
import os
import pandas as pd
pip install lxml
import lxml
os.chdir(r'C:\Users\lenovo\Documents\BSE\RA\Data\Data\EVP')
file = r"Honduras\Presidente 2001.pdf"
tables = camelot.read_pdf(file, pages='13')
tables[0].to_csv("Hon_2001.csv")

file = r"Honduras\Honduras Presidente_2005.pdf"
tables = camelot.read_pdf(file, pages = '1')
tables[0].to_csv('Hon_2005.csv')
tables[0].df

file = r"Latvia\leg_2002\SourceFile. CoR. Latvian. 8.Saeima.pdf"
tables = camelot.read_pdf(file, pages ='1,2', line_scale = 35, process_background = True)
tables[0].to_csv('Latvia/leg_2002/lat_leg_02_1.csv')
tables[1].to_csv('Latvia/leg_2002/lat_leg_02_2.csv')
camelot.plot(tables[0], kind='joint').show()

file = r"Latvia\leg_2006\Latvia 2006 Leg. SourceFile_Latvian. 9.Saeima__.pdf"
tables = camelot.read_pdf(file, pages= '1,2,3', line_scale = 40)
tables
df = tables[10].df


### Webscraping ===============================================================
import selenium
from selenium import webdriver
#pip install webdriver-manager
from webdriver_manager.chrome import ChromeDriverManager
url = 'https://pemilu2019.kpu.go.id/#/dprri/hitung-suara/'
browser = webdriver.Chrome(ChromeDriverManager().install())
browser.get(url)
#//*[@id="app"]/div[2]/div[1]/div[2]/section/div/div[4]

table = browser.find_element_by_class_name('data-table')
df_indonesia = pd.read_html(table.get_attribute('outerHTML'), decimal=',', thousands='.')[0]
df_indonesia.to_csv("indo_leg_19.csv")

# Legislative 2019
url = 'https://pemilu2019.kpu.go.id/#/ppwp/hitung-suara/'

table_1 = browser.find_element_by_xpath('//*[@id="app"]/div[2]/div[1]/div[2]/section/div/div[4]/div/div[1]/table')
table_2 = browser.find_element_by_xpath('//*[@id="app"]/div[2]/div[1]/div[2]/section/div/div[4]/div/div[2]/table')

df_indo = pd.read_html(table_1.get_attribute('outerHTML'), decimal=',', thousands='.')[0]
df_indo_2 = pd.read_html(table_2.get_attribute('outerHTML'), decimal=',', thousands='.')[0]
df_indonesia = pd.concat([df_indo, df_indo_2], ignore_index=True, sort=False)

df_indonesia.to_csv('indo_pres_19.csv')

browser.close()

## Poland 
# Searching for legislative 2011
# here it will be a dataframe per party (the only way to get the data)
url = "https://wybory2011.pkw.gov.pl/rfl/pl/a37acdd2b4996b08222e85815b6da20a.html"
browser = webdriver.Chrome(ChromeDriverManager().install())
browser.get(url)

#KW Prawo i Sprawiedliwość
table_1 = browser.find_element_by_xpath('//*[@id="container-149901"]')
df_1 = pd.read_html(table_1.get_attribute('outerHTML'), decimal=',', thousands='.')[0]
# as this website is dynamic / session changes a bit 

#KW Polska Jest Najważniejsza
table_2 = browser.find_element_by_xpath('//*[@id="container-149901"]')
df_2 = pd.read_html(table_2.get_attribute('outerHTML'), decimal=',', thousands='.')[0]

#Komitet Wyborczy SLD
table_3 =  browser.find_element_by_xpath('//*[@id="container-149901"]')
df_3 = pd.read_html(table_3.get_attribute('outerHTML'), decimal=',', thousands='.')[0]

#Komitet Wyborczy Ruch Palikota
table_4 = browser.find_element_by_xpath('//*[@id="container-149901"]')
df_4 = pd.read_html(table_4.get_attribute('outerHTML'), decimal=',', thousands='.')[0]

#Komitet Wyborczy PSL
table_5 = browser.find_element_by_xpath('//*[@id="container-149901"]')
df_5 = pd.read_html(table_5.get_attribute('outerHTML'), decimal=',', thousands='.')[0]

#Komitet Wyborczy PPP - Sierpień 80
table_6 = browser.find_element_by_xpath('//*[@id="container-149901"]')
df_6 = pd.read_html(table_6.get_attribute('outerHTML'), decimal=',', thousands='.')[0]

#KW Platforma Obywatelska RP
table_7 = browser.find_element_by_xpath('//*[@id="container-149901"]')
df_7 = pd.read_html(table_7.get_attribute('outerHTML'), decimal=',', thousands='.')[0]

#main
table = browser.find_element_by_xpath('//*[@id="first"]/div/table')
df_poland = pd.read_html(table.get_attribute('outerHTML'))[0]

browser.close()

#working with the df's
df_poland = df_poland.drop([6,7,9,10], axis = 1)
df_poland.columns = ['pol_sta_nr', 'country', 'polling_station', 'registered_voters',
                     'total_votes', 'valid_cards', 'valid_votes']
df_poland = df_poland.drop([0,1,2,261])
# did a stupid mistake below -> dfs below have space at the end
df_poland.rename({'pol_sta_nr': "pol_sta_nr "}, axis='columns', inplace=True)
df_list = [df_4, df_5, df_6, df_7]
party_names = ["Prawo i Sprawiedliwość", "Polska Jest Najważniejsza", "SLD", "Ruch Palikota",
               "PSL", "PPP - Sierpień 80", "Obywatelska RP"]

# I tried a loop but I failed 
cols = [1,2,4,5]
df_1.drop(cols, axis=1, inplace=True)
df_1.rename({0: 'pol_sta_nr ', 3: party_names[0]}, axis = 'columns', inplace=True)

df_2.drop(cols, axis=1, inplace=True)
df_2.rename({0: 'pol_sta_nr ', 3: party_names[1]}, axis = 'columns', inplace=True)

df_3.drop(cols, axis=1, inplace=True)
df_3.rename({0: 'pol_sta_nr ', 3: party_names[2]}, axis = 'columns', inplace=True)

# hah but not pretty
for i in df_list:
    i.drop(cols, axis = 1, inplace=True)

counter = 3
for i in df_list:
    i.rename({0: 'pol_sta_nr ', 3: party_names[counter]}, axis = 'columns', inplace=True)
    counter += 1

# merge all the dataframes
from functools import reduce
data_frames = [df_poland, df_1, df_2, df_3, df_4, df_5, df_6, df_7]
df_merged = reduce(lambda  left,right: pd.merge(left,right,on=['pol_sta_nr '],
                                            how='outer'), data_frames)

df_merged.to_csv("pol_leg_2011.csv")


url = 'https://www.cvk.lv/cgi-bin/wdbcgiw/base/komisijas2010.GalRezs10?nr1=1&nr2=10200'
browser = webdriver.Chrome(ChromeDriverManager().install())
browser.get(url)

table = browser.find_element_by_xpath('/html/body/font/center/i/b[2]/i/table[2]')
df_lat = pd.read_html(table.get_attribute('outerHTML'))[0]

lat_dictionary = {
    2: "polling_station",
    4: "PCTVL",
    5: 'VIENOTĪBA',
    6: "Ražots Latvijā", 
    7: "Saskaņas Centrs",
    8: "Tautas kontrole",
    9: "Zaļo un Zemnieku savienība", 
    10: "Par prezidentālu republiku",
    11: "Par Labu Latviju",
    12: "ATBILDĪBA", 
    13: "Daugava - Latvijai",
    14: "Pēdējā partija",
    15: "Visu Latvijai!-Tēvzemei un Brīvībai/LNNK",
    16: "Kristīgi demokrātiskā savienība"}

df_lat.drop([0,1,2,3,68], inplace = True)
df_lat.drop([0,1,3,17], axis = 1, inplace=True)
df_lat.rename(lat_dictionary, axis='columns', inplace = True)
df_lat.to_csv("lat_leg_10.csv")

# lat 2011 
# again working with the open selenium browser so changed url there -> faster 
# to: https://www.cvk.lv/cgi-bin/wdbcgiw/base/Saeima11.GalRez_s11?nr1=1&nr2=10200#lim2

table = browser.find_element_by_xpath('/html/body/font/center/i/b[2]/i/table[2]')
df_lat = pd.read_html(table.get_attribute('outerHTML'))[0]

df_lat.drop([0,1,2,3,81,82], inplace=True)
df_lat.drop([0,1,3], axis = 1, inplace = True)

lat_dictionary = {
    2: "poling_station",
    4: "total_voters",
    5: "correct_votes", 
    6: "valid_votes", 
    7: "VIENOTĪBA",
    8: "Latvijas Sociāldemokrātiskā strādnieku partija",
    9: "Zatlera Reformu partija",
    10: "Kristīgi demokrātiskā savienība",
    11: "Šlesera Reformu partija LPP/LC",
    12:  "Saskaņas Centrs",
    13: "PCTVL",
    14: "Visu Latvijai!-Tēvzemei un Brīvībai/LNNK",
    15: "Par prezidentālu republiku",
    16: "Pēdējā partija",
    17: "Zaļo un Zemnieku savienība",
    18: "Tautas kontrole",
    19: "Brīvība. Brīvs no bailēm, naida un dusmām"}

df_lat.rename(lat_dictionary, axis = 'columns', inplace = True)
df_lat.to_csv("lat_leg_2011.csv")


## latvia 2014

table = browser.find_element_by_xpath('//*[@id="novada-izklajums"]')
df_lat = pd.read_html(table.get_attribute('outerHTML'))[0]

table = browser.find_element_by_xpath('/html/body/div[2]/div/div[5]/div')
lat_dictionary = pd.read_html(table.get_attribute('outerHTML'))[0]
browser.close()
lat_dictionary.columns = ["party_code", "party_name", "bla", "blue", "blee"]
lat_dictionary.set_index('party_code').to_dict()['party_name']
lat_dictionary.drop(['bla', 'blue', 'blee'], axis = 1, inplace = True)

lat_dictionary.to_csv("lat_14_dic.csv")
df_lat.to_csv("lat_leg_14.csv")
