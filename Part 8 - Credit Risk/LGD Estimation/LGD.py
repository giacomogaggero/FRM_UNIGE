# -*- coding: utf-8 -*-
"""
Created on Sun Jan  9 10:00:46 2022

@author: Utente
"""
#Library
import numpy as np
#Function
def LGDestimation(T,i,EAD,RF,EF):
    DiscountedRF=RF/(1+i)**T
    DiscountedEF=EF/(1+i)**T
    LGD=1-(sum(DiscountedRF)-sum(DiscountedEF))/EAD
    Tableau=np.column_stack([T,i*100,RF,EF,DiscountedRF,DiscountedEF])
    return (LGD,Tableau)
#Script

IRfrac=np.array([0, 0.5,0.583,0.667,0.75,0.833,0.917,1,1.083,1.167,1.25,1.333,
    1.417,1.5,2,3,4,5,6,7,8,9,10,11,12,15])
IRRiskFreeRates=np.array([-0.515,-0.515,-0.511,-0.506,-0.504,-0.494,-0.489,-0.482,-0.475,
                          -0.467,-0.461,-0.454,-0.447,-0.44,-0.4583,-0.3965,-0.3277,
                          -0.2566,-0.1826,-0.1097,-0.0358,0.0345,0.1002,0.1619,0.2206,0.3591])
T=np.arange(1,25)/12
RiskFree=np.interp(T,IRfrac,IRRiskFreeRates)/100
#RiskFree=np.array([0.03,0.23,0.35,0.43,0.5,0.55,0.6,0.64,0.67,0.7,0.73,0.75,0.78,0.79,0.81,0.83,0.84,0.86,0.87,0.89,0.9,0.92,0.93,0.94])/100
spread=0.035
AdjRate=RiskFree+spread
contractual_rate=np.tile(0.04,len(T))
#i=RiskFree
i=AdjRate
#i=contractual_rate
EAD=100000
RF=np.zeros(len(T))
RF[-1]=50000
RF[-4]=10000
EF=np.tile(1000,len(T))
Output=LGDestimation(T,i,EAD,RF,EF)
LGD=Output[0]
Tableau=Output[1]
print(LGD*100)