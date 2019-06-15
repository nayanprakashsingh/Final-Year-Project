# -*- coding: utf-8 -*-
"""
Created on Sun May  5 11:25:41 2019

@author: seena
"""

import numpy as np 
import pandas as pd 
import os 
from sklearn.linear_model import LinearRegression 
from sklearn.model_selection import train_test_split 
os.chdir("C:\\Users\\seena\\Downloads\\FYP-master\\Regression_folder\\Data") 
import matplotlib.pyplot as plt 
import seaborn as sns 

#%matplotlib inline 
crypto = pd.read_csv('CryptocoinsHistoricalPrices.csv') 
crypto.head() 
"""crypto.info()
crypto.describe() 
crypto.columns 
crypto.drop(['Unnamed: 0'], axis=1, inplace=True) 
crypto.head() 
crypto.dropna(axis=0, inplace=True) 
crypto['Year']=crypto['Date'].apply(lambda x: x.split('-')[0]) 

df= crypto[crypto['Market.Cap']!='-'] 
df['Market.Cap'] = df['Market.Cap'].str.replace(',', '') 
df['Market.Cap']=df['Market.Cap'].astype(float) 
df= df[df['Year']=='2017'] 
coins_type=df.groupby('coin').count().sort_values(by='Volume',ascending=False).head(10) 
top_coins_name=list(coins_type.index) 

df_top = df[df['coin'].isin(top_coins_name)] 
df_top['Month']=df_top['Date'].apply(lambda x: x.split('-')[1]) 
df_top['Volume'] = df_top['Volume'].str.replace(',', '') 
df_top['Volume']=df_top['Volume'].astype(float)"""