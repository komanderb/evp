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

