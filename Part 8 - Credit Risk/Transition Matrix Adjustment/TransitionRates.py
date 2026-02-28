# -*- coding: utf-8 -*-
"""
Created on Thu Feb 17 10:09:22 2022

@author: Utente
"""
#Library
import numpy as np
import csv
from scipy.stats import norm
#Function
def importdata(Filepath): 
    with open(Filepath) as csv_file:
        csv_reader = csv.reader(csv_file, delimiter=';')
        DatasetRows = 0
        for row in csv_reader: 
            DatasetRows += 1
    AAA=np.zeros([DatasetRows-1])
    AA=np.zeros([DatasetRows-1])
    A=np.zeros([DatasetRows-1])
    BBB=np.zeros([DatasetRows-1])
    BB=np.zeros([DatasetRows-1])
    B=np.zeros([DatasetRows-1])
    C=np.zeros([DatasetRows-1])
    D=np.zeros([DatasetRows-1])
    with open(Filepath) as csv_file:
        csv_reader = csv.reader(csv_file, delimiter=';')
        line_count = 0
        for row in csv_reader:
            if line_count == 0:
                headers=[row[0],row[1],row[2],row[3],row[4]]
                print(headers)
                line_count += 1
            else:
                AAA[line_count-1]=float(row[0])
                AA[line_count-1]=float(row[1])
                A[line_count-1]=float(row[2])
                BBB[line_count-1]=float(row[3])
                BB[line_count-1]=float(row[4])
                B[line_count-1]=float(row[5])
                C[line_count-1]=float(row[6])
                D[line_count-1]=float(row[7])
                line_count += 1
    Dataset=np.column_stack((AAA,AA,A,BBB,BB,B,C,D))
    return Dataset
def getThresholdsMatrix(TransitionDataset):
    Thresholds=np.zeros([len(TransitionDataset),len(TransitionDataset[0])-1])
    for i in range(len(TransitionDataset)):
        for j in range(len(TransitionDataset[0])-1,0,-1):
           Thresholds[i,j-1]=norm.ppf(sum(TransitionDataset[i,j:((len(TransitionDataset[0])))])/100)
    return Thresholds
def getAdjustedTransitionMatrix(ThresholdsMatrix,CreditIndex):
    ModifiedTransitionMatrix=np.zeros([len(ThresholdsMatrix),len(ThresholdsMatrix[0])+1])
    for i in range(len(ModifiedTransitionMatrix)):
        for j in range((len(ModifiedTransitionMatrix[0])-1),0,-1):
            ModifiedTransitionMatrix[i,j]=norm.cdf(ThresholdsMatrix[i,j-1]-CreditIndex)-sum(ModifiedTransitionMatrix[i,j:((len(ModifiedTransitionMatrix[0])))])
    ModifiedTransitionMatrix[:,0]=1-np.sum(ModifiedTransitionMatrix,axis=1)
    return(ModifiedTransitionMatrix*100)
#Script
Filepath = 'C:\\Users\\Utente\\Desktop\\CreditRisk\\TransitionRatesDataset.csv'
TransitionMatrix = importdata(Filepath)
ThresholdsMatrix=getThresholdsMatrix(TransitionMatrix)
CreditIndex=-0.25
AdjustedTransitionMatrix=getAdjustedTransitionMatrix(ThresholdsMatrix,CreditIndex)

