# -*- coding: utf-8 -*-
import matplotlib
import camelot
import os
import pandas as pd
import lxml
from functools import reduce
## everything scraping
### Webscraping ===============================================================
import selenium
from selenium import webdriver
#pip install webdriver-manager
from webdriver_manager.chrome import ChromeDriverManager
from selenium.webdriver.common.by import By
os.chdir(r'C:\Users\lenovo\Documents\BSE\RA\Data\Data\EVP')

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

def scrape_poland_space():
    table = browser.find_element_by_xpath('//*[@id="container-149901"]')
    df = pd.read_html(table.get_attribute('outerHTML'), decimal=',', thousands='\xa0')[0]
    return(df)
    

#KW Prawo i Sprawiedliwość
df_1 = scrape_poland_space()

# as this website is dynamic / session changes a bit 
#KW Polska Jest Najważniejsza
df_2 = scrape_poland_space()

#Komitet Wyborczy SLD
df_3 = scrape_poland_space()

#Komitet Wyborczy Ruch Palikota
df_4 = scrape_poland_space()

#Komitet Wyborczy PSL
df_5 = scrape_poland_space()

#Komitet Wyborczy PPP - Sierpień 80
df_6 = scrape_poland_space()

#KW Platforma Obywatelska RP
df_7 = scrape_poland_space()

#main
table = browser.find_element_by_xpath('//*[@id="first"]/div/table')
df_poland = pd.read_html(table.get_attribute('outerHTML'), decimal=',', thousands='\xa0')[0]

browser.close()

#working with the df's
df_poland = df_poland.drop([6,7,9,10], axis = 1)
df_poland.columns = ['pol_sta_nr', 'country', 'polling_station', 'registered_voters',
                     'total_votes', 'valid_cards', 'valid_votes']
df_poland = df_poland.drop([0,1,2,261])
#
df_list = [df_1, df_2, df_3, df_4, df_5, df_6, df_7]
party_names = ["Prawo i Sprawiedliwość", "Polska Jest Najważniejsza", "SLD", "Ruch Palikota",
               "PSL", "PPP - Sierpień 80", "Obywatelska RP"]

# I tried a loop but I failed 
cols = [1,2,4,5]
counter = 0
for i in df_list:
    i.drop(cols, axis = 1, inplace=True)
    i.rename({0: 'pol_sta_nr', 3: party_names[counter]}, axis = 'columns', inplace=True)
    counter += 1

# merge all the dataframes

data_frames = [df_poland, df_1, df_2, df_3, df_4, df_5, df_6, df_7]
df_merged = reduce(lambda  left,right: pd.merge(left,right,on=['pol_sta_nr'],
                                            how='outer'), data_frames)

df_merged.to_csv("pol_leg_2011.csv")

###Poland 2015 
# is easier to scrape that than working with the excel

url = 'https://parlament2015.pkw.gov.pl/350_Wyniki_Sejm_zagranica.html'
browser = webdriver.Chrome(ChromeDriverManager().install())
browser.get(url)

# as all the tables have the same xpath but on different clicks 
# I can rerun the following website while navigating my dynamic browser
def polish_scraper():
    table = browser.find_element_by_xpath('//*[@id="DataTables_Table_1"]')
    df = pd.read_html(table.get_attribute('outerHTML'))[0]
    return(df)

# number behind df indicates the party
df_1 = polish_scraper()
df_2 = polish_scraper()
df_3 = polish_scraper()
df_4 = polish_scraper()
df_5 = polish_scraper()
df_6 = polish_scraper()
df_7 = polish_scraper()
df_8 = polish_scraper()
df_9 = polish_scraper()
df_10 = polish_scraper()

#general election results // 
#let's redefine polish_scraper as table scraper for more general approach
def table_scraper(xpath):
    table = browser.find_element_by_xpath(xpath)
    df = pd.read_html(table.get_attribute('outerHTML'))[0]
    return(df)
    
df_poland = table_scraper('//*[@id="DataTables_Table_2"]')
pol_names = table_scraper('//*[@id="DataTables_Table_0"]')

browser.close()

df_list = [df_poland, df_1, df_2, df_3, df_4, df_5, df_6, df_7, df_8, df_9, df_10]
for df in df_list:
    df.dropna(inplace=True)

pol_names.dropna(axis=1, inplace = True, how = 'all')
pol_names.dropna(inplace=True)
names = pol_names.Nazwa.tolist()

party_list = [df_1, df_2, df_3, df_4, df_5, df_6, df_7, df_8, df_9, df_10]
cols = ['Głosy ważne', 'Wynik w regionie']
counter = 0
for df in party_list: 
    df.drop(cols, axis=1, inplace=True)
    df.rename({'Głosy oddane na komitet': names[counter]}, axis = 'columns', inplace=True)
    counter += 1

df_poland.drop(['Lp', 'Frekwencja (%)'], axis=1, inplace=True)
df_poland.rename({'Nazwa jednostki': 'Nazwa'}, axis = 'columns', inplace=True)

df_merged = reduce(lambda  left,right: pd.merge(left,right,on=['Nazwa'],
                                            how='outer'), df_list)

df_merged.to_csv('pol_leg_15.csv')
### Latvia 

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

## latvia 2006
url = 'https://www.cvk.lv/cgi-bin/wdbcgiw/base/saeima9.GalRezS9.vis'
browser = webdriver.Chrome(ChromeDriverManager().install())
browser.get(url)
frame_switch('html > frameset > frame:nth-child(2)')
df_lat = table_scraper('/html/body/i/font/center/table[2]')
lat_party = table_scraper('/html/body/i/font/center/table[1]')

browser.close()

#working with the data
part_1 = lat_party.iloc[:,0:2]
part_2 = lat_party.iloc[:,2:4]
part_3 = lat_party.iloc[:, 4:6]

part_list = [part_1, part_2, part_3]
for part in part_list:
    part.columns = ['party_number', 'party_name']
lat_party = pd.concat(part_list, ignore_index=True)
lat_party.dropna(inplace=True)
lat_party.to_csv('lat_06_party.csv')
df_lat.to_csv('lat_06.csv')


## latvia 2002 
url = 'https://www.cvk.lv/cgi-bin/wdbcgiw/base/sae8dev.vel8meg.sa3'
browser = webdriver.Chrome(ChromeDriverManager().install())
browser.get(url)

def frame_switch(css_selector):
  browser.switch_to.frame(browser.find_element_by_css_selector(css_selector))

frame_switch('html > frameset > frame:nth-child(2)')
table = browser.find_element_by_xpath('/html/body/font/table[3]')
df_lat = pd.read_html(table.get_attribute('outerHTML'))[0]

lat_names = table_scraper('/html/body/font/table[1]')
lat_party = lat_names.stack().reset_index()
lat_party.columns = ['level_0', 'level_1', 'party']
lat_party = lat_party['party'].str.split(' - ', expand=True)
lat_party.drop([2], axis = 1, inplace = True)
lat_party.dropna(inplace = True)
lat_party[0] = lat_party[0].apply(pd.to_numeric)
lat_party = lat_party.sort_values(0)

lat_party.to_csv('lat_02_party.csv')
df_lat.to_csv('lat_02.csv')


## italy
url = 'https://elezionistorico.interno.gov.it/index.php?tpel=C&dtel=09/04/2006&es0=S&tpa=E&lev0=0&levsut0=0&ms=S&tpe=A'
browser = webdriver.Chrome(ChromeDriverManager().install())
browser.get(url)

def italy_scraper():
    table = browser.find_element_by_xpath('/html/body/div[4]/div/div[2]/div[2]/div[3]')
    df = pd.read_html(table.get_attribute('outerHTML'), decimal=',', thousands='.')[0]
    return(df)

argentina_df = italy_scraper()
bolivia_df = italy_scraper()
brasil_df = italy_scraper()
chile_df = italy_scraper()
colombia_df = italy_scraper()
ecuador_df = italy_scraper()
paraguay_df = italy_scraper()
peru_df = italy_scraper()
uruguay_df = italy_scraper()
venezuela_df = italy_scraper()

browser.close()

italy_list = [argentina_df, bolivia_df, brasil_df, chile_df, colombia_df,
              ecuador_df, paraguay_df, peru_df, uruguay_df, venezuela_df]

cols = ['Liste/Gruppi', '%']
country = ['Argentina', 'Bolivia', 'Brasil', 'Chile', 'Colombia',
              'Ecuador', 'Paraguay', 'Peru', 'Uruguay', 'Venezuela']

counter = 0
for df in italy_list:
    df.drop(cols, axis = 1, inplace = True)
    df.dropna(inplace = True)
    df.insert(0, 'country', country[counter])
    counter += 1
    
df_italy = pd.concat(italy_list, ignore_index=True, sort=False)
df_italy.to_csv('italy_2006.csv')


## Romania:: 
#Presidential 2004

url = 'http://alegeri.roaep.ro/?alegeri=prezidentiale-2004'

browser = webdriver.Chrome(ChromeDriverManager().install())
browser.get(url)

browser.find_element_by_xpath('//*[@id="selJudet"]/option[37]').click()


selector = browser.find_element_by_xpath('//*[@id="selLocalitate"]')
list_selection = selector.find_elements_by_tag_name('option')



from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from time import sleep
def best_romanian_scraper(start, end):
    df_list_1 = []
    df_list_2 = []
    for i in (range(start, end)):
       # browser.implicitly_wait(5)
        #WebDriverWait(list_selection, timeout=3).until(EC.element_to_be_clickable(list_selection[i]))
        browser.find_element_by_xpath('//*[@id="selJudet"]/option[37]').click()
        sleep(1)
        #browser.implicitly_wait(5)
        selector = browser.find_element_by_xpath('//*[@id="selLocalitate"]')
        #list_selection = 
        selector.find_elements_by_tag_name('option')[i].click()
        sleep(1)
        #list_selection[i].click()
        #table_1 = browser.find_element_by_xpath('//*[@id="vizualizareRezultate"]/div[2]/div[4]/div[1]/div[2]/table')
        df_1 = pd.read_html(browser.find_element_by_xpath('//*[@id="vizualizareRezultate"]/div[2]/div[4]/div[1]/div[2]/table').get_attribute('outerHTML'))[0]
        #df_1 = pd.read_html(table_1.get_attribute('outerHTML'))[0]
        df_1.insert(0, 'pol_stat', selector.find_elements_by_tag_name('option')[i].text)
        #table_2 = browser.find_element_by_xpath('//*[@id="vizualizareRezultate"]/div[2]/div[4]/div[1]/div[3]/table')
        df_2= pd.read_html(browser.find_element_by_xpath('//*[@id="vizualizareRezultate"]/div[2]/div[4]/div[1]/div[3]/table').get_attribute('outerHTML'))[0]
        #df_2= pd.read_html(table_2.get_attribute('outerHTML'))[0]
        df_2.insert(0, 'pol_stat', selector.find_elements_by_tag_name('option')[i].text)
        df_list_1.append(df_1)
        df_list_2.append(df_2)
        sleep(1)   
        #browser.refresh()
        
    
    df_1 = pd.concat(df_list_1, ignore_index=True, sort=False)
    df_2 =  pd.concat(df_list_2, ignore_index=True, sort=False)
    return(df_1, df_2)
        

df_general, df_votes = best_romanian_scraper(1,151)
  # it's 153 and pretty sure I used that but double check

browser.close()


df_general.to_csv('ro_pres_04_general.csv')
df_votes.to_csv('ro_pres_04_votes.csv')
## Romania presidential 2000

url = 'http://alegeri.roaep.ro/?alegeri=prezidentiale-2000'
browser = webdriver.Chrome(ChromeDriverManager().install())
browser.get(url)

browser.find_element_by_xpath('//*[@id="selJudet"]/option[37]').click()
df_general, df_votes = best_romanian_scraper(1,151)

df_general.to_csv('ro_pres_00_general.csv')
df_votes.to_csv('ro_pres_00_votes.csv')

## Legislative elections / 

url = 'http://alegeri.roaep.ro/?alegeri=alegeri-parlamentul-romaniei-2000'

browser = webdriver.Chrome(ChromeDriverManager().install())
browser.get(url)

browser.find_element_by_xpath('//*[@id="selTip"]/option[3]').click()
browser.find_element_by_xpath('//*[@id="selJudet"]/option[37]').click()
# have to redefine the function a little bit as there is only one round and thus position 
# of tables is defined differently

def best_romanian_scraper(start, end):
    df_list_1 = []
    df_list_2 = []
    for i in (range(start, end)):
       # browser.implicitly_wait(5)
        #WebDriverWait(list_selection, timeout=3).until(EC.element_to_be_clickable(list_selection[i]))
        browser.find_element_by_xpath('//*[@id="selJudet"]/option[37]').click()
        sleep(1)
        #browser.implicitly_wait(5)
        selector = browser.find_element_by_xpath('//*[@id="selLocalitate"]')
        #list_selection = 
        selector.find_elements_by_tag_name('option')[i].click()
        sleep(1)
        #list_selection[i].click()
        #table_1 = browser.find_element_by_xpath('//*[@id="vizualizareRezultate"]/div[2]/div[4]/div[1]/div[2]/table')
        df_1 = pd.read_html(browser.find_element_by_xpath('//*[@id="vizualizareRezultate"]/div[3]/div[4]/div[1]/div/table').get_attribute('outerHTML'))[0]
        #df_1 = pd.read_html(table_1.get_attribute('outerHTML'))[0]
        df_1.insert(0, 'pol_stat', selector.find_elements_by_tag_name('option')[i].text)
        #table_2 = browser.find_element_by_xpath('//*[@id="vizualizareRezultate"]/div[2]/div[4]/div[1]/div[3]/table')
        df_2= pd.read_html(browser.find_element_by_xpath('//*[@id="vizualizareRezultate"]/div[3]/div[4]/div[2]/div/table').get_attribute('outerHTML'))[0]
        #df_2= pd.read_html(table_2.get_attribute('outerHTML'))[0]
        df_2.insert(0, 'pol_stat', selector.find_elements_by_tag_name('option')[i].text)
        df_list_1.append(df_1)
        df_list_2.append(df_2)
        sleep(1)   
        #browser.refresh()
       
    
    df_1 = pd.concat(df_list_1, ignore_index=True, sort=False)
    df_2 =  pd.concat(df_list_2, ignore_index=True, sort=False)
    return(df_1, df_2)

df_general, df_votes = best_romanian_scraper(1,151)
browser.close()
df_general.to_csv('ro_leg_00_general.csv')
df_votes.to_csv('ro_leg_00_votes.csv')

## 2004

url = 'http://alegeri.roaep.ro/?alegeri=alegeri-parlamentul-romaniei-2004'
browser = webdriver.Chrome(ChromeDriverManager().install())
browser.get(url)

browser.find_element_by_xpath('//*[@id="selTip"]/option[3]').click()
browser.find_element_by_xpath('//*[@id="selJudet"]/option[37]').click()

df_general, df_votes = best_romanian_scraper(1, 153)
browser.close()
df_general.to_csv('ro_leg_04_general.csv')
df_votes.to_csv('ro_leg_04_votes.csv')

## 2008 -> no country names only the other stuff // 
url = 'http://alegeri.roaep.ro/?alegeri=test-parlamentare'
browser = webdriver.Chrome(ChromeDriverManager().install())
browser.get(url)

browser.find_element_by_xpath('//*[@id="selTip"]/option[3]').click()
browser.find_element_by_xpath('//*[@id="selJudet"]/option[37]').click()

df_general, df_votes = best_romanian_scraper(1, 203)
browser.close()
df_general.to_csv('ro_leg_08_general.csv')
df_votes.to_csv('ro_leg_08_votes.csv')


## Moldova //
""" Legislative 2012 """
# can't automate because of captcha // 
url = 'https://archiveresults.cec.gov.ge/'
# tried all the other links as well //

browser = webdriver.Chrome(ChromeDriverManager().install())
browser.get(url)
def moldovo_scraper(link):
    table = pd.read_html(browser.find_element(By.XPATH, '//*[@id="table36"]').get_attribute('outerHTML'))[0]
    return table
# not able to pass the captcha // 
df_1 = moldovo_scraper(url)
""" We might have a problem here, as the website does not let my browser pass the 
captcha, even if I try it several times by hand""" 


### Macedonia //
from time import sleep
url = 'https://rezultati2016.sec.mk/Parliamentary/Results?cs=en-US&r=r&rd=r1&eu=8&m=All'

def mace_scraper(link):
    df_list_1= []
    df_list_2 = []
    polling_station = []
    browser = webdriver.Chrome(ChromeDriverManager().install())
    browser.get(link)
    drop = browser.find_element_by_xpath('//*[@id="menuMunicipalities"]')
    list_pol = drop.find_elements_by_tag_name('option')
    for counter, element in enumerate(list_pol):
        drop = browser.find_element_by_xpath('//*[@id="menuMunicipalities"]')
        list_pol = drop.find_elements_by_tag_name('option')
        polling_station.append(list_pol[counter].text)
        list_pol[counter].click()
        sleep(1)
        df_1 = pd.read_html(browser.find_element_by_xpath('//*[@id="div-results-table"]/div/table').get_attribute('outerHTML'), decimal=',', thousands='.')[0]
        browser.find_element_by_xpath('//*[@id="procesim"]/img').click()
        sleep(1)
        df_2 = pd.read_html(browser.find_element_by_xpath('//*[starts-with(@id,"popover")]/div[2]/div').get_attribute('outerHTML'), decimal=',', thousands='.')[0]
        df_1.insert(0, 'pol_stat', polling_station[counter])
        df_2.insert(0, 'pol_stat', polling_station[counter])
        df_list_1.append(df_1)
        df_list_2.append(df_2)
        
    
    df_1 = pd.concat(df_list_1, ignore_index=True, sort=False)
    df_2 =  pd.concat(df_list_2, ignore_index=True, sort=False)
    return(df_1, df_2)

df_1, df_2 = mace_scraper(url)

df_1.to_csv('macedonia_2016_1.csv')
df_2.to_csv('macedonia_2016_2.csv')



## Ecuador //


### Bulgaria 
url = "https://pi2005.cik.bg/results/2-32.html"

#Legislative 2005 //
from selenium.common.exceptions import NoSuchElementException
import warnings
warnings.filterwarnings("ignore", category=DeprecationWarning) 
def bul_scraper(link):
    country_list = []
    polling_stations_list = []
    links = []
    df_list = []
    browser = webdriver.Chrome(ChromeDriverManager().install())
    browser.get(link)
    list_pol = browser.find_element_by_xpath('/html/body/table/tbody/tr[4]/td[3]')
    polling_stations = list_pol.find_elements_by_tag_name('div')
    for counter, item in enumerate(polling_stations):
        if counter < 2:
            pass
        else:
            list_pol = browser.find_element_by_xpath('/html/body/table/tbody/tr[4]/td[3]')
            polling_stations = list_pol.find_elements_by_tag_name('div')
            try:
                link = polling_stations[counter].find_element_by_tag_name('a').get_attribute('href') 
                links.append(link)
                country_list.append(country)
                polling_stations_list.append(polling_stations[counter].text)
            except NoSuchElementException:
                # this should work as every time a list of polling stations
                # begins with unclickable country 
                country = polling_stations[counter].text
                
    for counter, i in enumerate(links):
        browser.get(i)
        sleep(0.5)
        df = pd.read_html(browser.find_element_by_xpath('/html/body/table/tbody/tr[4]/td[3]/table').get_attribute('outerHTML'))[0]
        df.insert(0, 'pol_stat', polling_stations_list[counter])
        df.insert(0, 'country', country_list[counter])
        df_list.append(df)
        
    final_df = pd.concat(df_list, ignore_index=True, sort=False)
    return(final_df)
# that actually should work, but maybe it is more efficient to directly translate?

                
df = bul_scraper(url)
    
df.to_csv('bulgaria_leg_2005.csv')   

# Presidential 2011:
url = "https://results.cik.bg/mipvr2011/tur1/prezidentski/2900.html"
browser = webdriver.Chrome(ChromeDriverManager().install())
browser.get(url)
# translated countries before (because easier):
countries = browser.find_element_by_xpath('//*[@id="sidebar"]/ul')
# google translate manually
country_list = countries.find_elements_by_tag_name('li')
countries = []
for i in country_list:
    countries.append(i.text)
#g
def bul_scraper(link, country_list):
    countries = []
    df_list = []
    links= []
    browser = webdriver.Chrome(ChromeDriverManager().install())
    browser.get(link)
    #countries = browser.find_element_by_xpath('//*[@id="sidebar"]/ul')
    #country_list = countries.find_elements_by_tag_name('li')
    for counter, element in enumerate(country_list):
        countries = browser.find_element_by_xpath('//*[@id="sidebar"]/ul')
        element_list = countries.find_elements_by_tag_name('li')
        #country_list.append(country_list[counter].text)
        links.append(element_list[counter].find_element_by_tag_name('a').get_attribute('href'))
        
    for counter, i in enumerate(links):
        browser.get(i)
        sleep(0.5)
        df = pd.read_html(browser.find_element_by_xpath('//*[@id="main"]/div[2]/div[2]/table').get_attribute('outerHTML'))[0]
        df.insert(0, 'country', country_list[counter])
        df_list.append(df)
        
    final_df = pd.concat(df_list, ignore_index=True, sort=False)
    return(final_df)    


df = bul_scraper(url, countries)                                                                                        	
df.to_csv('bulgaria_pres_2011.csv')

## get a party dictionary to make life easy:
df_party = pd.read_html(browser.find_element_by_xpath('//*[@id="main"]/div[2]/div[2]/table').get_attribute('outerHTML'))[0]
df_party = df_party.iloc[:, 0:2]
# did some modifications by hand. 

df_party.to_csv('bu_pres_2011_dic.csv')


### Ukraine
#Legislative 2002
url = "https://www.cvk.gov.ua/pls/vnd2002/webproc193vb639.html?kodvib=400"
browser = webdriver.Chrome(ChromeDriverManager().install())
browser.get(url)

table = browser.find_element_by_xpath('/html/body/table[4]')
df = pd.read_html(table.get_attribute('outerHTML'))[0]
df.to_csv('ukraine_leg_2002.csv')

### ukraine leg 2006 -> problems with translating and also getting information on polling station 
url = "https://cvk.gov.ua/pls/vnd2006/W6P001-2.html"
browser = webdriver.Chrome(ChromeDriverManager().install())
browser.get(url)
df = pd.read_html(browser.find_element_by_xpath('/html/body/table[5]').get_attribute('outerHTML'))[0]
### ukraine leg 2007
url = "https://www.cvk.gov.ua/pls/vnd2007/w6p336b49a.html?pt001f01=600&pf7331=226"
browser = webdriver.Chrome(ChromeDriverManager().install())
browser.get(url)


##ukraine leg 2012
url = "https://www.cvk.gov.ua/pls/vnd2012/wp314ept001f01=900.html"
def ukraine_scraper(link):
    countries = []
    df_list = []
    links= []
    browser = webdriver.Chrome(ChromeDriverManager().install())
    browser.get(link)
    table = browser.find_element_by_xpath('//*[@id="content"]/table[5]')
    country_list = table.find_elements_by_class_name("a1small")
    for counter, item in enumerate(country_list):
        countries.append(country_list[counter].text)
        links.append(country_list[counter].get_attribute('href'))
    for counter, link in enumerate(links):
        browser.get(link)
        sleep(0.5)
        df = pd.read_html(browser.find_element_by_xpath('//*[@id="content"]/table[4]').get_attribute('outerHTML'))[0]
        df.insert(0, 'country', countries[counter])
        df_list.append(df)
        
    final_df = pd.concat(df_list, ignore_index=True, sort=False)
    return(final_df)    

df = ukraine_scraper(url)
df.to_csv('ukraine_leg_12.csv')

## ukraine leg 2014
url = "https://www.cvk.gov.ua/pls/vnd2014/wp314ept001f01=910.html"
df = ukraine_scraper(url)
df.to_csv('ukraine_leg_14.csv')

## ukraine leg 2019
#have to do slightly different approach
url = "https://www.cvk.gov.ua/pls/vnd2019/wp314pt001f01=919.html"
browser = webdriver.Chrome(ChromeDriverManager().install())
browser.get(url)
# also first step by hand in order to translate properly
table = browser.find_element_by_xpath('//*[@id="wrap"]/main/div[2]/table')
countries = table.find_elements_by_xpath('//a[@title="Voting results by country"]')
country_list_19 = []
links_ua = []
for counter, item in enumerate(countries):
    country_list_19.append(countries[counter].text)
    links_ua.append(countries[counter].get_attribute('href'))
    
df_list = []
# this will unfortunately not be translated
for counter, link in enumerate(links_ua):
    browser.get(link)
    sleep(0.5)
    df = pd.read_html(browser.find_element_by_xpath('//*[@id="wrap"]/main/div[2]/table').get_attribute('outerHTML'))[0]
    df.insert(0, 'country', country_list_19[counter])
    df_list.append(df)

final_df = pd.concat(df_list, ignore_index=True, sort=False)
final_df.to_csv('ukraine_leg_2019.csv')


## ukraine
browser = webdriver.Chrome(ChromeDriverManager().install())
browser.get(url)
df = pd.read_html(browser.find_element_by_xpath('/html/body/table[6]').get_attribute('outerHTML'))[0]
df.to_csv("ukraine_pres_2010_polstat.csv")

browser = webdriver.Chrome(ChromeDriverManager().install())
url = 'https://cvk.gov.ua/pls/vp2014/wp045pt001f01=702.html'
browser.get(url)
df = pd.read_html(browser.find_element_by_xpath('/html/body/table[5]').get_attribute('outerHTML'))[0]
df.to_csv("ukraine_pres_2014_polstat.csv")
url = 'https://www.cvk.gov.ua/pls/vp2019/wp045pt001f01=719.html'
browser.get(url)
df = pd.read_html(browser.find_element_by_xpath('//*[@id="pure-wrap"]/table').get_attribute('outerHTML'))[0]
df.to_csv("ukraine_pres_2019_polstat.csv")

# the rest is a bit problematic at least


### Venezuela:
    
url = "http://www.cne.gob.ve/divulgacionPresidencial/resultado_nacional.php?color=&c2=0&e=99"

# this will be ugly 
from selenium.common.exceptions import NoSuchElementException
import warnings
warnings.filterwarnings("ignore", category=DeprecationWarning) 
def venezuela_scraper(link):
    df_list_1 = []
    df_list_2 = []
    country_list = []
    link_list = []
    browser = webdriver.Chrome(ChromeDriverManager().install())
    browser.get(link)
    table = browser.find_element_by_xpath('/html/body/center/table[2]/tbody/tr/td[1]/table/tbody/tr[2]/td/table')

    countries = table.find_elements_by_tag_name('a')
    for counter, item in enumerate(countries):
        country_list.append(countries[counter].text)
        link_list.append(countries[counter].get_attribute('href'))
    for counter, item in enumerate(link_list):
        browser.get(item)
        try:
            df = pd.read_html(browser.find_element_by_xpath('/html/body/center/table[2]/tbody/tr/td[2]/table/tbody/tr/td/table').get_attribute('outerHTML'))[0]
            df.dropna(subset=1, inplace = True)
            df = df[df[1].apply(lambda x: len(x) > 20)]
            df.drop(0, axis = 1, inplace = True)
            df_2 = pd.read_html(browser.find_element_by_xpath('/html/body/center/table[2]/tbody/tr/td[1]/table/tbody/tr[3]/td/table').get_attribute('outerHTML'))[0]
            df.insert(0, 'country', country_list[counter])
            df_2.insert(0, 'country', country_list[counter])
            df_list_1.append(df)
            df_list_2.append(df_2)
        except KeyError:
            pass
    
    final_df_1 = pd.concat(df_list_1, ignore_index=True, sort=False)
    final_df_2 = pd.concat(df_list_2, ignore_index=True, sort=False)
    return(final_df_1, final_df_2)

df_1, df_2 = venezuela_scraper(url)

df_1.to_csv("ven_06_1.csv")
df_2.to_csv("ven_06_2.csv")



## Venezuela 2013 

url = 'http://www.cne.gob.ve/resultado_presidencial_2013/r/1/reg_990000.html'
browser = webdriver.Chrome(ChromeDriverManager().install())
browser.get(url)
table = browser.find_element_by_xpath('//*[@id="regionNavigationBar"]')
countries = table.find_elements_by_tag_name('a')
countries[1].get_attribute("href")

df = pd.read_html(browser.find_element_by_xpath('//*[@id="fichaTecnica"]/table').get_attribute('outerHTML'), decimal=',', thousands='.')[0]
# table is a bit weird -> will have to make the best out of it //

def ven_scraper(link):
    country_list = []
    link_list = []
    df_list_1 = []
    df_list_2 = []
    browser = webdriver.Chrome(ChromeDriverManager().install())
    browser.get(link)
    table = browser.find_element_by_xpath('//*[@id="regionNavigationBar"]')
    countries = table.find_elements_by_tag_name('a')
    for counter, item in enumerate(countries):
        country_list.append(countries[counter].text)
        link_list.append(countries[counter].get_attribute("href"))
    
    for counter, item in enumerate(link_list):
        browser.get(item)
        try:
            df_1 = pd.read_html(browser.find_element_by_xpath('//*[@id="tablaResultados"]/table').get_attribute('outerHTML'), decimal=',', thousands='.')[0]
            df_2 = pd.read_html(browser.find_element_by_xpath('//*[@id="fichaTecnica"]/table').get_attribute('outerHTML'), decimal=',', thousands='.')[0]
            df_1.insert(0, 'country', country_list[counter])
            df_2.insert(0, 'country', country_list[counter])
            df_list_1.append(df_1)
            df_list_2.append(df_2)
        except NoSuchElementException:
            pass
        
        
    final_df_1 = pd.concat(df_list_1, ignore_index=True, sort=False)
    final_df_2 = pd.concat(df_list_2, ignore_index=True, sort=False)
    return(final_df_1, final_df_2)
    

df_1, df_2 = ven_scraper(url)

df_1.to_csv("ven_13_1.csv")
df_2.to_csv("ven_13_2.csv")

### Czech Republic 2006


url = 'https://www.volby.cz/pls/ps2006/ps36?xjazyk=CZ'
browser = webdriver.Chrome(ChromeDriverManager().install())
browser.get(url)

table = browser.find_element_by_xpath('//*[@id="publikace"]/table')

## google translate manually!#

df = pd.read_html(table.get_attribute('outerHTML'))[0]



def special_cz_scraper(link, df):
    link_list = []
    df_list = []
    
    browser = webdriver.Chrome(ChromeDriverManager().install())
    browser.get(url)
    table = browser.find_element_by_xpath('//*[@id="publikace"]/table')
    rows = table.find_elements_by_tag_name('tr')
    for counter, item in enumerate(rows):
        if counter < 1:
            pass
        else:
            cell = rows[counter].find_element_by_class_name('cislo')
            link_list.append(cell.find_element_by_tag_name('a').get_attribute('href'))
    
    for counter, item in enumerate(link_list):
        browser.get(item)
        df_1 = pd.read_html(browser.find_element_by_xpath('//*[@id="publikace"]/table').get_attribute('outerHTML'))[0]
        df_1.set_axis(['registered_voters', 'V', 'W', 'total_votes', 'valid_votes', 'Z'], axis=1, inplace=True)
        df_1.drop(['V', 'W', 'Z'], axis = 1, inplace = True)
        
        df_2 = pd.read_html(browser.find_element_by_xpath('//*[@id="inner"]/div[1]/table').get_attribute('outerHTML'))[0]
        df_2.set_axis(['V', 'W', 'X', 'Y', 'Z'], axis=1, inplace=True)
        df_2.drop(['W', 'Y', 'Z'], axis = 1, inplace = True)
        df_2 = df_2.set_index('V').T
        df_3 = pd.read_html(browser.find_element_by_xpath('//*[@id="inner"]/div[2]/table').get_attribute('outerHTML'))[0]#
        df_3.set_axis(['V', 'W', 'X', 'Y', 'Z'], axis=1, inplace=True)
        df_3.drop(['W', 'Y', 'Z'], axis = 1, inplace = True)
        df_3 = df_3.set_index('V').T
        df_1.reset_index(drop=True, inplace=True)
        df_2.reset_index(drop=True, inplace=True)
        df_3.reset_index(drop=True, inplace=True)#

        df_temp = pd.concat([df_1, df_2, df_3], axis = 1, sort=False)
        df_list.append(df_temp)
    
    final_df = pd.concat(df_list, ignore_index=True, sort=False)
    final_df.reset_index(drop=True, inplace=True)
    final_df = pd.concat([df, final_df], axis = 1, sort = False)
    return(final_df)


cz_df = special_cz_scraper(url, df)
cz_df.to_csv('cz_06_leg.csv')

## just party dictionary missing //
url_party = 'https://www.volby.cz/pls/ps2006/ps311?xjazyk=CZ&xkraj=3&xobec=999997&xsvetadil=EV&xzeme=8&xokrsek=1'
browser.get(url_party)
# still at the same url / 
df_2 = pd.read_html(browser.find_element_by_xpath('//*[@id="inner"]/div[1]/table').get_attribute('outerHTML'))[0]
df_2.set_axis(['party_number', 'party_name', 'X', 'Y', 'Z'], axis=1, inplace=True)
df_2.drop(['X', 'Y', 'Z'], axis = 1, inplace = True)
df_3 = pd.read_html(browser.find_element_by_xpath('//*[@id="inner"]/div[2]/table').get_attribute('outerHTML'))[0]#
df_3.set_axis(['party_number', 'party_name', 'X', 'Y', 'Z'], axis=1, inplace=True)
df_3.drop(['X', 'Y', 'Z'], axis = 1, inplace = True)

df_party = pd.concat([df_2, df_3], ignore_index=True, sort = False)

df_party.to_csv('cz_leg06_partydic.csv')


