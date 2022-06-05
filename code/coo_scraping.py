# -*- coding: utf-8 -*-
"""
File for scraping COO elections 
Created on Thu Jun  2 14:39:03 2022

@author: Bjoern Komander
"""

#%% load relevant libraries
import pandas as pd
import os 
import selenium
from selenium import webdriver
#pip install webdriver-manager
from webdriver_manager.chrome import ChromeDriverManager
from selenium.webdriver.common.by import By
os.chdir(r'C:\Users\lenovo\Documents\BSE\RA\Data\Data\EVP')

#%% Pol LEG 11

link = "https://wybory2011.pkw.gov.pl/wyn/pl/000000.html#tabs-1"

browser = webdriver.Chrome(ChromeDriverManager().install())
browser.get(link)

table = browser.find_element_by_xpath('//*[@id="tab-1"]/div[1]/table')
df = pd.read_html(table.get_attribute('outerHTML'), decimal=',', thousands='\xa0')[0]
df.drop([0, 3,4], axis = 1, inplace = True)
df.drop([0,1], axis = 0, inplace = True)
# now here we assume this is everything domestic + abroad 
df.to_csv("pub_coo/POL_LEG_11.csv")
# ah i forgot additional information
table = browser.find_element_by_xpath('//*[@id="tab-1"]/div[2]/table')
df = pd.read_html(table.get_attribute('outerHTML'), decimal=',', thousands='\xa0')[0]
df = df.iloc[[1,44]]
df.drop([0,1,2,6,7,9,10], axis = 1, inplace = True)
df. rename(columns = {3:'REGISTERED', 4:'maybe_total', 5: "TOTAL", 8: "VALID"},
           inplace = True)
df.drop(1, inplace = True)

df.to_csv("pub_coo/POL_LEG_11extra.csv")
# %% POL LEG 15

def table_scraper(url, xpath):
    browser =  webdriver.Chrome(ChromeDriverManager().install())
    browser.get(link)

    table = browser.find_element_by_xpath(xpath)
    df = pd.read_html(table.get_attribute('outerHTML'), decimal=',', thousands='\xa0')[0]
    return df 
link = 'https://parlament2015.pkw.gov.pl/349_Wyniki_Sejm.html'
x_path = '//*[@id="DataTables_Table_0"]'

df = table_scraper(link, x_path)
df.drop(['Lp.', 'Liczba mandatów', 'Kolor', 'Procent'], axis = 1,
        inplace = True)
df.dropna(inplace = True)
""" 

Summary voting statistics
Number of eligible voters	30629150
Number of valid cards	15595335
Number of valid votes	15200671
Number of invalid votes	394664

"""
df.to_csv("pub_coo/POL_LEG_15.csv")
#%% 2019 is weird 

#%% 2000 POL PRES 
link = 'https://prezydent2000.pkw.gov.pl/wb/wb.html'
x_path = '/html/body/table[2]/tbody/tr/td/table[6]' 

df = table_scraper(link, x_path) 

df.drop([2,3], axis = 1, inplace = True)
df.drop([0,1,2,3,4], inplace = True)
df.to_csv("pub_coo/POL_PRES_00.csv")

"""
Number of eligible voters	29 122 304
Number of ballots issued	17 798 791
Number of cards taken from the urn	17 789 231
Number of valid votes	17 598 919
Number of invalid votes	190 312
Frequency (%)	61.12
 
"""

#%% Pres 05 nada 
#%% Pres 10 

link = 'https://prezydent2010.pkw.gov.pl/PZT1/EN/WYN/W/index.htm'
x_path = '//*[@id="s0"]' 
df = table_scraper(link, x_path)
df.drop([0,1,2,5, 6,7,8], axis = 1, inplace = True)

df.drop([0,1,2,3], inplace = True)

df.to_csv('pub_coo/POL_PRES_10.csv')

"""
REGISTERED: 30813005	
TOTAL SENT: 16929088	
TOTAL: 16923832	
VALID: 16806170	

"""

#%% Pres 15
link = 'https://prezydent2015.pkw.gov.pl/319_Pierwsze_glosowanie.html'
x_path = '//*[@id="DataTables_Table_0"]'
df = table_scraper(link, x_path)
df.dropna(inplace = True)
df.drop(['Lp', 'Wynik wyborczy (%)'], axis = 1, inplace = True)
df.to_csv("pub_coo/POL_PRES_15.csv")
"""
Number of eligible voters	30688570
Number of valid cards	15023886
Number of election packages sent	42814
Number of valid votes	14898934
Number of invalid votes	124952
"""

# %% Romania Leg 00
# this is a bit annoying because the website is dynamic and 
# now I'm too lazy to write click function 

link ='http://alegeri.roaep.ro/?alegeri=alegeri-parlamentul-romaniei-2000'
x_path = '//*[@id="vizualizareRezultate"]/div[3]/div[2]/div[2]/div/table'

browser =  webdriver.Chrome(ChromeDriverManager().install())
browser.get(link)

# click here   
table = browser.find_element_by_xpath(x_path)
df = pd.read_html(table.get_attribute('outerHTML'))[0]
df.to_csv("pub_coo/RO_LEG_00.csv")
x_path_extra = '//*[@id="vizualizareRezultate"]/div[3]/div[2]/div[1]/div/table'

table = browser.find_element_by_xpath(x_path_extra)
df = pd.read_html(table.get_attribute('outerHTML'))[0]
#what do now?
df.to_csv("pub_coo/RO_LEG_00_extra.csv")

## 
#%% LEG 04 
# can use the same variables 
link = 'http://alegeri.roaep.ro/?alegeri=alegeri-parlamentul-romaniei-2004'
browser =  webdriver.Chrome(ChromeDriverManager().install())
browser.get(link)
# click here 
table = browser.find_element_by_xpath(x_path)
df = pd.read_html(table.get_attribute('outerHTML'))[0]
df.to_csv("pub_coo/RO_LEG_04.csv")

table = browser.find_element_by_xpath(x_path_extra)
df = pd.read_html(table.get_attribute('outerHTML'))[0]
df.to_csv("pub_coo/RO_LEG_04_extra.csv")
#%% LEG 08 
link = 'http://alegeri.roaep.ro/?alegeri=test-parlamentare'
browser =  webdriver.Chrome(ChromeDriverManager().install())
browser.get(link)
# click here 
table = browser.find_element_by_xpath(x_path)
df = pd.read_html(table.get_attribute('outerHTML'))[0]
df.to_csv("pub_coo/RO_LEG_08.csv")

table = browser.find_element_by_xpath(x_path_extra)
df = pd.read_html(table.get_attribute('outerHTML'))[0]
df.to_csv("pub_coo/RO_LEG_08_extra.csv")

#%% Leg 12 
link = 'http://alegeri.roaep.ro/?alegeri=alegeri-parlamentul-romaniei-2012-14_05_2014'
browser =  webdriver.Chrome(ChromeDriverManager().install())
browser.get(link)
# click here 
table = browser.find_element_by_xpath(x_path)
df = pd.read_html(table.get_attribute('outerHTML'))[0]
df.to_csv("pub_coo/RO_LEG_12.csv")

table = browser.find_element_by_xpath(x_path_extra)
df = pd.read_html(table.get_attribute('outerHTML'))[0]
df.to_csv("pub_coo/RO_LEG_12_extra.csv")

#%% Leg 16
# website not reachable 
#%% Pres 00 
# now no clicking so quick fun 

def pres_scraper(url, xpath, xpath_e, name, name_2):
    browser =  webdriver.Chrome(ChromeDriverManager().install())
    browser.get(url)
    table = browser.find_element_by_xpath(xpath)
    df = pd.read_html(table.get_attribute('outerHTML'))[0]
    df.to_csv(name)
    table = browser.find_element_by_xpath(xpath_e)
    df = pd.read_html(table.get_attribute('outerHTML'))[0]
    df.to_csv(name_2)
    
link = 'http://alegeri.roaep.ro/?alegeri=prezidentiale-2000'
link_2 = 'http://alegeri.roaep.ro/?alegeri=prezidentiale-2004'
x_path = '//*[@id="vizualizareRezultate"]/div[2]/div[2]/div[1]/div[3]/table'
x_path_e = '//*[@id="vizualizareRezultate"]/div[2]/div[2]/div[1]/div[2]/table'
name_1 = "pub_coo/RO_PRES_00.csv"
name_1e = "pub_coo/RO_PRES_00_extra.csv"
name_2 = "pub_coo/RO_PRES_04.csv"
name_2e = "pub_coo/RO_PRES_04_extra.csv"


links = [link, link_2]
names = [name_1, name_2]
names_e = [name_1e, name_2e]

for counter, item in enumerate(links):
    pres_scraper(links[counter], x_path, x_path_e, names[counter], names_e[counter])
    
#%%