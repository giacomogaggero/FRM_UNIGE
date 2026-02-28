# -*- coding: utf-8 -*-
"""
Created on Mon Feb 21 16:24:56 2022

@author: Utente
"""

#Library
import numpy as np
import csv
from scipy.stats import norm
import matplotlib.pyplot as plt
#Function
def importdata(Filepath): 
    with open(Filepath) as csv_file:
        csv_reader = csv.reader(csv_file, delimiter=';')
        DatasetRows = 0
        for row in csv_reader: 
            DatasetRows += 1
    LoanID=np.zeros([DatasetRows-1])
    PD=np.zeros([DatasetRows-1])
    LGD=np.zeros([DatasetRows-1])
    EAD=np.zeros([DatasetRows-1])
    W=np.zeros([DatasetRows-1])
    with open(Filepath) as csv_file:
        csv_reader = csv.reader(csv_file, delimiter=';')
        line_count = 0
        for row in csv_reader:
            if line_count == 0:
                headers=[row[0],row[1],row[2]]
                print(headers)
                line_count += 1
            else:
                LoanID[line_count-1]=int(row[0])
                PD[line_count-1]=float(row[1])
                LGD[line_count-1]=float(row[2])
                EAD[line_count-1]=float(row[3])
                W[line_count-1]=float(row[4])
                line_count += 1
    Dataset=np.column_stack((LoanID,PD,LGD,EAD,W))
    return Dataset
def MonteCarloSim(PortfolioDataset,NSim,Confidence,graphics): 
    PD=PortfolioDataset[:,1]
    LGD=PortfolioDataset[:,2]
    EAD=PortfolioDataset[:,3]
    W=PortfolioDataset[:,4]
    Portfolio_Loss=np.zeros(NSim)
    for i in range(NSim):
        Z=np.random.randn(1)
        d=norm.ppf(PD)
        A=W*Z+((1-W)**0.5)*np.random.randn(len(W))
        Loss=LGD*EAD*(A<d)
        Portfolio_Loss[i]=sum(Loss)
    if(graphics):
        plt.hist(x=Portfolio_Loss, bins=25, color='#0504aa',alpha=0.7, rwidth=0.85, range=(0,800))
        plt.grid(axis='y', alpha=0.75)
        plt.xlabel('Loss')
        plt.ylabel('Frequency')
        plt.title('Loss Distribution')
    VaR=np.percentile(Portfolio_Loss,Confidence*100)
    ES=np.mean(Portfolio_Loss[Portfolio_Loss>=VaR])
    return (Portfolio_Loss, VaR,ES)
    
#Script
#Investment Grades
Filepath = 'C:\\Users\\Utente\\Desktop\\CreditRisk\\CreditPortfolioSample.csv'
PortfolioDataset = importdata(Filepath)
NSim=100000
Confidence=0.95
graphics=True
Output=MonteCarloSim(PortfolioDataset,NSim,Confidence,graphics)
print('VaR: ', Output[1],'ES: ',  Output[2])

