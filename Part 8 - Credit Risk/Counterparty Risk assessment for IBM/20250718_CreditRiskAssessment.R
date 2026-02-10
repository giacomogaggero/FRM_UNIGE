#Part A

#Function

AltmanZscore = function(StatementData){
  X1=StatementData$net_working_capital/StatementData$Total_asset
  X2=StatementData$Retained_earnings/StatementData$Total_asset
  X3=StatementData$Earnings_before_interest_expense_taxes/StatementData$Total_asset
  X4=StatementData$Equity_Market_value/StatementData$Total_liability
  X5=StatementData$Net_sales_revenues/StatementData$Total_asset
  #Zscore=1.2*X1+1.4*X2+3.3*X3+0.6*X4+1*X5
  ALtman_coeff=c(1.2,1.4,3.3,0.6,1)
  X=c(X1,X2,X3,X4,X5)
  return(ALtman_coeff%*%X)
}

#Script

StatementData=list(net_working_capital=230,
                   Total_asset=67210,
                   Total_liability=118714,
                   Retained_earnings=150703,
                   Equity_Market_value=230573.55,
                   Earnings_before_interest_expense_taxes=8606,
                   Net_sales_revenues= 0.93*67210)


out=AltmanZscore(StatementData)

#Part B

#Function

getPDfromCDS = function(T_array,s_array,RR){
  lambda_array=(s_array/10000)/(1-RR)
  PD_array=1-exp(-lambda_array*T_array)
  return(PD_array)
}

#Script

RR=0.4
T_array = c(0.5, 1, 2, 3, 5, 7, 10)
s_array = c(7.5, 10.5, 14.0, 21.0, 35, 53, 63.8)
PD_array=getPDfromCDS(T_array,s_array,RR)
print(PD_array*100)

#Part C

CreditGrades_PS = function(S,sigma_E,D,Lambda,sigma_B,t){
  d1=(S+Lambda*D)*exp(sigma_B**2)/(Lambda*D)
  alpha=sqrt((((sigma_E*S)/(S+Lambda*D))^2)*t+sigma_B^2)
  CG_PS=pnorm(-(alpha/2)+(log(d1)/alpha))-d1*pnorm(-(alpha/2)-(log(d1)/alpha))
  return(CG_PS)
}


#Script
Lambda=0.4
t=1
S=272.08
NumShares=(230573.55)/S
D=(118714)/NumShares
sigma_E=28.098/100
sigma_B=0.1
PD=1-CreditGrades_PS(S,sigma_E,D,Lambda,sigma_B,t)
print(PD*100)

