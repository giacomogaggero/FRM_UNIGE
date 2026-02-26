# -*- coding: utf-8 -*-
"""
Created on Sat Jan 22 08:25:03 2022

@author: Utente
"""
#Library
import numpy as np
from scipy.interpolate import interpn
#Function
def EADprediction(Credit_Line,CCF,Available_Margin): 
    EAD=(Credit_Line-Available_Margin)+CCF*Available_Margin
    return EAD
def getCCF(Rating,Years2Default):
    if (Rating<1) | (Rating>8):
        raise NameError('Rating Grades must be between 1 and 8')
    x_Table=np.linspace(1,6,6)
    y_Table=np.linspace(1,8,8)
    points=(x_Table,y_Table)
    values=np.array([[12.1,78.7,93.9,54.8,32,39.6,26.5,24.5],
             [12.1,75.5,47.2,52.1,44.9,49.8,39.7,26.7],
             [12.1,84,41.7,41.5,62.1,62.1,37.3,9.4],
             [12.1,84,100,37.5,76,62.6,97.8,9.4],
             [12.1,84,100,100,68.3,100,97.8,9.4],
             [12.1,84,100,100,68.3,100,97.8,9.4]])
    point=np.array([Years2Default,Rating])
    CCF = interpn(points, values, point, method='linear', bounds_error=False,fill_value=getExtrpValue(Rating))
    return CCF/100 
def getExtrpValue(Rating):
    if Rating==1:
        return 12.1
    elif Rating==2:
        return 84
    elif Rating==3:
        return 100
    elif Rating==4:
        return 100
    elif Rating==5:
        return 68.3
    elif Rating==6:
        return 100
    elif Rating==7:
        return 97.8       
    elif Rating==8:
        return 9.4
#Script
Credit_Line=20000
Available_Margin=5000
Rating_Grade=7
Time2Default=1
CCF=getCCF(Rating_Grade,Time2Default)
EAD=EADprediction(Credit_Line,CCF,Available_Margin)[0]
print(EAD)

