# -*- coding: utf-8 -*-
"""
Created on Sat Feb 26 11:04:43 2022

@author: Utente
"""
#Library
import numpy as np
from scipy.stats import norm,binom,chi2
#Script
Year=np.array([1,2,3,4,5])
Losses=np.array([0,8,0,9,4])
p=binom.cdf(Losses,200,0.035)
z=norm.ppf(p)
T=len(Year)
ML=np.var(z)
terms1=-T/2*np.log(ML)-np.sum((z-np.mean(z))**2)/(2*ML)
terms2=-(np.sum(z**2))/2
LR=2*(terms1-terms2)
pvalue=chi2.sf(LR,2)