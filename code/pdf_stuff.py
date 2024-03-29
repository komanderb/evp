# -*- coding: utf-8 -*-
"""
Created on Thu Dec 30 11:37:54 2021

@author: lenovo
"""
#!pip install matplotlib
#import matplotlib
import camelot
import os
import pandas as pd
#pip install lxml
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


## italy
file = "italy/argentina.pdf"
tables = camelot.read_pdf(file, page = '1')
# not worth it

# indonesia 2014 // 

file = r"Indonesia\p.111. Buku Pilpres 2014 30112015.low.pdf"
tables = camelot.read_pdf(file, pages='122,123,124,125,126')
# okay that worked well 
df = tables[0].df
for i in range(1, len(tables)):
    print(i)
    df = pd.concat([df, tables[i].df], ignore_index=True, sort=False)
df.to_csv('indo_14.csv')


## Bolivia 2019: 
file = r"Bolivia\2019\Publicacion_Resultados_Nacionales_EG_2019-2.pdf"
tables = camelot.read_pdf(file, pages = "2,3,4,5,6,7")

##apparantly it is very hard to get the header of the corresponding file, which is a bit
# problematic
table_names = ['Germany', 'total', 'total_national', 'Argentina', 'Austria',
               'total_abroad', 'Chile', 'Belgium', 'China',  'Brasil', 'Colombia',
               'Canada', 'Costa Rica', 'Egypt', 'Spain', 'Cuba', 'US', 'Ecuador',
               'Iran', 'France', 'Italy', 'UK', 'Japan', 'India', 'Panama', 'Mexico',
               'Paraguay', 'Nicaragua', 'Netherlands', 'Peru', 'Switzerland', 'South Korea',
               'Uruguay', 'Russia', 'Venezuela', 'Sweden']
def table_scraper(list_tables, cols):
    df_list = []
    for counter, table in enumerate(list_tables):
        df = table.df
        df.drop(cols, axis = 1, inplace = True)
        df.dropna(inplace = True)
        df.insert(0, 'country', table_names[counter])
        df_list.append(df)
    
    return(df_list)

drop = 2

df_bolivia = pd.concat(table_scraper(tables, drop), ignore_index=True, sort=False)
df_bolivia.to_csv('bolivia_2019.csv')


## Peru 
# not working -> also the only one // 
file = r"Peru\Peru 2006.pdf"
tables = camelot.read_pdf(file, pages = "306, 307, 308, 309, 310, 311", process_background = True)
tables[20].df

# colombia //

file = r"Colombia\leg_02\Antilas Holondesas.pdf"
tables = camelot.read_pdf(file, page = '1', line_scale = 40, process_background = True)

# also not working // 
file = r"Colombia\leg_06\Argentina.pdf"
tables = camelot.read_pdf(file)
for counter, table in enumerate(tables):
    df = tables[counter].df
# thats'working but the last column is just very weird // 
path = r"C:\Users\lenovo\Documents\BSE\RA\Data\Data\EVP\Colombia\leg_06\Source Files"
os.chdir(path)
from os import listdir
listdir(path)

file = "Malaysia.pdf"
tables = camelot.read_pdf(file)
"""
Problematic, as every other pdf is different -> sometimes not properly read
a bit lost here
"""

## Moldova

file = r"Moldova\leg_09_july\5913_electorala_29_iulie_2009, page 440.pdf"
 
tables = camelot.read_pdf(file, pages='441, 442, 443, 444, 445, 446')
df_1 = pd.concat([tables[0].df, tables[1].df], axis = 1, ignore_index=True)
df_2 = pd.concat([tables[2].df, tables[3].df], axis = 1, ignore_index=True)
df_3 = pd.concat([tables[4].df, tables[5].df], axis = 1, ignore_index=True)

df = pd.concat([df_1, df_2, df_3], ignore_index=True)
df.to_csv('moldova_leg_09_july.csv')
## need names:
tables = camelot.read_pdf(file, pages = '429, 430')
names_1 = tables[1].df.head(1)
names_mol = ["section_nr", 'country', 'Number of voters included in the electoral lists',
             'Number of voters included in the lists more',
            'The number of voters received ballots', 
            'The number of voters who participated in the voting',
            'Difference of ballots received and voters who participated in the voting',
            'invalid_ballots', 'section_nr', 'valid_votes', "Number of newslettersvote received", 
            "Number of ballots unused and canceled", 'Partidul Comunistilor din Republica Moldova',
            "Partidul Popular Crestin Democrat", 'Alianta MOLDOVA NOASTRA', 
            'Partidul Liberal', 'Partidul Liberal Democrat din Moldova',
            'Partidul Democrat din Moldova', 'Partidul Social Democrat', 
            'Partidul Ecologist Alianta Verde din Moldova']

with open("mol_leg09_july_names.txt", "w") as output:
    output.write(str(names_mol))

# just sclicing is missing but will do in R