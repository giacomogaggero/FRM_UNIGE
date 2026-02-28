"""
Created on Tue Dec 21 15:34:07 2021

@author: Utente
"""
#Library
import csv
import warnings
import numpy as np
from scipy.stats import norm, chi2, skew, kurtosis

#Classes

class DescriptiveStatistics():
    def __init__(self, average, median, Stdev,Skewness,Kurtosis,
                 Minimum,Maximum,Percentiles):
        self.average = average
        self.median = median
        self.Stdev = Stdev
        self.Skewness = Skewness
        self.Kurtosis = Kurtosis
        self.Minimum=Minimum
        self.Maximum=Maximum
        self.Percentiles=Percentiles
        
#Functions

def importdata(Filepath): 
    with open(Filepath) as csv_file:
        csv_reader = csv.reader(csv_file, delimiter=';')
        DatasetRows = 0
        for row in csv_reader: 
            DatasetRows += 1
    FirmID=np.zeros([DatasetRows-1])
    Year=np.zeros([DatasetRows-1])
    Default=np.zeros([DatasetRows-1])
    WC_TA=np.zeros([DatasetRows-1])
    RE_TA=np.zeros([DatasetRows-1])
    EBIT_TA=np.zeros([DatasetRows-1])
    ME_TL=np.zeros([DatasetRows-1])
    S_TA=np.zeros([DatasetRows-1])
    with open(Filepath) as csv_file:
        csv_reader = csv.reader(csv_file, delimiter=';')
        line_count = 0
        for row in csv_reader:
            if line_count == 0:
                headers=[row[0],row[1],row[2],row[3],row[4],
                         row[5],row[6],row[7]]
                print(headers)
                line_count += 1
            else:
                FirmID[line_count-1]=int(row[0])
                Year[line_count-1]=int(row[1])
                Default[line_count-1]=int(row[2])
                WC_TA[line_count-1]=float(row[3])
                RE_TA[line_count-1]=float(row[4])
                EBIT_TA[line_count-1]=float(row[5])
                ME_TL[line_count-1]=float(row[6])
                S_TA[line_count-1]=float(row[7])
                line_count += 1
    Dataset=np.column_stack((Default,WC_TA,RE_TA,EBIT_TA,ME_TL,S_TA))
    return Dataset

def logit(Dataset, constant, stats): 
    y=Dataset[:,0]
    xraw=Dataset[:,1:len((Dataset[0])-1)]
    N=len(y)
    K=len(xraw[0])+1 if constant else len(xraw[0])
    x=np.column_stack((np.ones(N),xraw)) if constant else xraw
    ybar=np.mean(y)
    b=np.zeros(K)
    bx=np.zeros(N)
    Lambda=np.zeros(N)
    sens=1e-11
    maxiter=50
    lnL=np.zeros(maxiter)
    change=sens+1
    Iter=0
    while ((np.abs(change)) and (Iter < maxiter-1)):
        Iter+=1
        dlnL=np.zeros(K)
        hesse=np.zeros((K,K))
        for i in range(N):
            Lambda[i]=1/(1+np.exp(-bx[i]))
            for j in range(K):
                dlnL[j] = dlnL[j] + (y[i] - Lambda[i]) * x[i, j]
                for jj in range(K):
                    hesse[jj,j]=hesse[jj,j]-Lambda[i]*\
                               (1-Lambda[i])*x[i,jj]*x[i,j]
            lnL[Iter]=lnL[Iter]+y[i]*np.log(1/(1+np.exp(-bx[i])))+\
                     (1-y[i])*np.log(1-1/(1+np.exp(-bx[i])))
        hinv=np.linalg.inv(hesse)
        hinvg=np.matmul(dlnL, hinv)
        change=lnL[Iter]-lnL[Iter-1]
        if np.abs(change)<=sens:
            break
        for j in range(K):
            b[j]=b[j]-hinvg[j]
        for i in range(N):
            bx[i]=0
            for j in range(K):
                bx[i]=bx[i]+b[j]*x[i,j]
        if Iter >= maxiter:
            warnings.warn('Increase number of iterations')
    relogit=np.zeros((7,K)) if stats else np.zeros((1,K)) 
    for j in range(K):
        relogit[0,j]=b[j]
    if stats:
        for j in range(K):
            relogit[1,j]= np.sqrt(-hinv[j,j])
            relogit[2,j]=relogit[0,j]/relogit[1,j]
            relogit[3,j]=(1-norm.cdf(np.abs(relogit[2,j])))*2
        lnL0 =N*(ybar*np.log(ybar)+(1-ybar)*np.log(1-ybar))
        relogit[4,0]= 1-lnL[Iter]/lnL0     
        relogit[4,1]=Iter           
        relogit[5,0]=2*(lnL[Iter]-lnL0)   
        relogit[5,1] = chi2.sf(relogit[5, 0],K-1) 
        relogit[6,0]=lnL[Iter]
        relogit[6,1]=lnL0
    return relogit

def getPDfromLogit(Dataset,constant): 
    b=logit(Dataset,constant,False)
    x=Dataset[:,1:(len(Dataset[0]))]
    N=len(x[:,0])
    if constant:
        x=np.column_stack((np.ones(N),x))
    DefaultProb=np.zeros(N)
    for i in range(N):
        DefaultProb[i]=1/(1+np.exp(-np.dot(b,x[i])))
    return DefaultProb

def getPDsensitivityfromLogit(Dataset,constant,bump):
    b=logit(Dataset,constant,False)
    x=Dataset[:,1:(len(Dataset[0]))]
    N=len(x[:,0])
    if constant:
        x=np.column_stack((np.ones(N),x))
    K=len(x[0])+1 if constant else len(x[0])
    x_left_bump=np.column_stack((x[:,0], x[:,1:K]-bump*x[:,1:K]))\
                if constant else x-bump*x
    x_right_bump=np.column_stack((x[:,0],x[:,1:K]+bump*x[:,1:K]))\
                if constant else x+bump*x
    Sensitivity_Matrix=np.zeros((N,K)) if constant else np.zeros((N,K+1))
    for i in range(N):
        DefaultProb_left_bump=1/(1+np.exp(-np.dot(b,x_left_bump[i])))
        DefaultProb_right_bump=1/(1+np.exp(-np.dot(b,x_right_bump[i])))
        Sensitivity_Matrix[i,0]=\
            (DefaultProb_right_bump-DefaultProb_left_bump)/(2*bump)
    if constant:
        for i in range(N):
            for j in range(K-2):
                x_left_bump=x[i,1:K]
                x_left_bump[j]=x_left_bump[j]-bump*x_left_bump[j]
                x_left_bump=np.append(1,x_left_bump)
                DefaultProb_left_bump=1/(1+np.exp(-np.dot(b,x_left_bump)))
                x_right_bump=x[i,1:K]
                x_right_bump[j]=x_right_bump[j]-bump*x_right_bump[j]
                x_right_bump=np.append(1,x_right_bump)
                DefaultProb_right_bump=1/(1+np.exp(-np.dot(b,x_right_bump)))
                Sensitivity_Matrix[i,j+1]=\
                    (DefaultProb_right_bump-DefaultProb_left_bump)/(2*bump)
    else:
        for i in range(N):
            for j in range(K):
                x_left_bump=x[i,:]
                x_left_bump[j]=x_left_bump[j]-bump*x_left_bump[j]
                DefaultProb_left_bump=1/(1+np.exp(-np.dot(b,x_left_bump)))
                x_right_bump=x[i,:]
                x_right_bump[j]=x_right_bump[j]-bump*x_right_bump[j]
                DefaultProb_right_bump=1/(1+np.exp(-np.dot(b,x_right_bump)))
                Sensitivity_Matrix[i,j+1]=\
                    (DefaultProb_right_bump-DefaultProb_left_bump)/(2*bump)
    return Sensitivity_Matrix

def getDatasetDescriptiveStats(Dataset): 
    x=Dataset[:,1:(len(Dataset[0]))]
    K=len(x[0])
    DescriptiveStats = []
    for i in range(K):
        avg=np.mean(x[:,i])
        median=np.median(x[:,i])
        stdev=np.std(x[:,i])
        skewness=skew(x[:,i])
        kurt=kurtosis(x[:,i])
        Minimum=min(x[:,i])
        Maximum=max(x[:,i])
        Percentiles=np.percentile(x[:,i],q=[0.5,1,5,95,99,99.5])
        DescriptiveStats.append(DescriptiveStatistics(avg,median,stdev,skewness,kurt,Minimum,Maximum,Percentiles))
    return(DescriptiveStats)

def Winsor(x,level): 
   N=len(x)
   low=np.percentile(x,level)
   up=np.percentile(x,100-level)
   Winsorized_x=np.zeros(N)
   for i in range(N):
       Winsorized_x[i]=min(max(x[i],low),up)
   return Winsorized_x

def XTransform(defaultdata,x,numranges):
    N=len(x)
    defsum=0
    obssum=0
    bound=np.zeros(numranges)
    numdefaults=np.zeros(numranges)
    obs=np.zeros(numranges)
    defrate=np.zeros(numranges)
    for j in range(numranges):
        bound[j]=np.percentile(x,(j+1)/numranges*100)
        numdefaults[j]=sum((x<=bound[j])*defaultdata)-defsum
        defsum=defsum+numdefaults[j]
        obs[j]=sum(x<=bound[j])-obssum
        obssum=obssum+obs[j]
        defrate[j]=numdefaults[j]/obs[j]
    transform=np.zeros(N)
    for i in range(N):
        j=1
        while (x[i]-bound[j])>0:
            j+=1
        transform[i]=max(defrate[j],0.0000001)
        transform[i]=np.log(transform[i]/(1-transform[i]))
    return transform

#Script
Filepath = 'C:\\Users\\Utente\\Desktop\\CreditRisk\\CreditDataset.csv'
CreditDataset = importdata(Filepath)
constant=True
stats=True
LogitModelOutput=logit(CreditDataset, constant, stats)
LogitPD=getPDfromLogit(CreditDataset, constant)
bump=1/100
LogitModelSensitivity=getPDsensitivityfromLogit(CreditDataset,constant,bump)
DatasetDescriptiveStats=getDatasetDescriptiveStats(CreditDataset)
DatasetDescriptiveStats[0].Percentiles
#xraw=np.column_stack((RE_TA,EBIT_TA,ME_TL))
LogitModelOutput=logit(CreditDataset[:,[0,2,3,4]], constant, stats)

chi2.sf(2*(-280.526-(-282.219)),2) 

x=CreditDataset[:,1]
Winsorlevel=2
Winsor_x=Winsor(x,Winsorlevel)

#Table1.11

Winsorlevel=1
WinsorDataset=np.column_stack((CreditDataset[:,0],Winsor(CreditDataset[:,1],Winsorlevel),Winsor(CreditDataset[:,2],Winsorlevel),Winsor(CreditDataset[:,3],Winsorlevel),Winsor(CreditDataset[:,4],Winsorlevel),Winsor(CreditDataset[:,5],Winsorlevel))) 
Winsorstat=getDatasetDescriptiveStats(WinsorDataset)
LogitModelOutput=logit(WinsorDataset, True, True)

Winsorlevel=1
WinsorDataset=np.column_stack((CreditDataset[:,0],Winsor(CreditDataset[:,1],Winsorlevel),Winsor(CreditDataset[:,2],Winsorlevel),Winsor(CreditDataset[:,3],Winsorlevel),np.log(Winsor(CreditDataset[:,4],Winsorlevel)),Winsor(CreditDataset[:,5],Winsorlevel))) 
Winsorstat=getDatasetDescriptiveStats(WinsorDataset)
LogitModelOutput=logit(WinsorDataset, True, True)

CreditDatasetlog=CreditDataset
CreditDatasetlog[:,4]=np.log(CreditDatasetlog[:,4])
LogitModelOutput=logit(CreditDatasetlog, True, True)


numranges=20
DatasetTransform=np.column_stack((CreditDataset[:,0],XTransform(CreditDataset[:,0],CreditDataset[:,1],numranges),XTransform(CreditDataset[:,0],CreditDataset[:,2],numranges),XTransform(CreditDataset[:,0],CreditDataset[:,3],numranges),XTransform(CreditDataset[:,0],CreditDataset[:,4],numranges),XTransform(CreditDataset[:,0],CreditDataset[:,5],numranges))) 
LogitModelOutput=logit(DatasetTransform, True, True)