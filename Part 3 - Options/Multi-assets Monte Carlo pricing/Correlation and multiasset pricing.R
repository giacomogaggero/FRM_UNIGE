#MKT Data

library(readxl)
EquityData <- read_excel("EquityData.xlsx",range = "B4:D1309", col_names = TRUE)

#Close Prices

S1_ClosePrices=EquityData[['SGO FP Equity']]
S2_ClosePrices=EquityData[['MBG GR Equity']]
S3_ClosePrices=EquityData[['VOW GR Equity']]

#Spot Prices - S

S1=S1_ClosePrices[length(S1_ClosePrices)]
S2=S2_ClosePrices[length(S2_ClosePrices)]
S3=S3_ClosePrices[length(S3_ClosePrices)]

#Daily returns

S1_returns=diff(log(S1_ClosePrices))
S2_returns=diff(log(S2_ClosePrices))
S3_returns=diff(log(S3_ClosePrices))

#Annualized Volatility estimation - sigma

sigma1=sd(S1_returns)*sqrt(252)
sigma2=sd(S2_returns)*sqrt(252)
sigma3=sd(S3_returns)*sqrt(252)

#Correlations - rho

rho12=cor(S1_returns,S2_returns)
rho13=cor(S1_returns,S3_returns)
rho23=cor(S2_returns,S3_returns)

#Risk Free rate (Euribor 6 months) - r

r=2.053/100.0

#Continuous Dividend Yields - q

q1=2.15/100
q2=7.77/100
q3=6.16/100

#Cost-of-Carry - b

b1=r-q1
b2=r-q2
b3=r-q3

#Option data
K=50
T=0.5
z=-1

#GBM Monte Carlo parameters
NSim=10^6

Drift1=(b1-sigma1^2/2)*T;
Drift2=(b2-sigma2^2/2)*T;
Drift3=(b3-sigma3^2/2)*T;

v1Sqrt=sigma1*sqrt(T);
v2Sqrt=sigma2*sqrt(T);
v3Sqrt=sigma3*sqrt(T);

epsilon1=rnorm(NSim)
epsilon2=rnorm(NSim)
epsilon3=rnorm(NSim)

#Correlation Structure

alpha1=epsilon1
alpha2=rho12*epsilon1+epsilon2*sqrt(1-rho12^2)
g=sqrt((1-rho13^2)/(1-rho12^2-rho13^2-rho23^2+2*rho12*rho13*rho23))
alpha3=epsilon3/g+(rho23-rho13*rho12)*epsilon2+rho13*epsilon1*sqrt(1/(1-rho12^2))

#Projections

ST1=S1*exp(Drift1+v1Sqrt*alpha1);
ST2=S2*exp(Drift2+v2Sqrt*alpha2);
ST3=S3*exp(Drift3+v3Sqrt*alpha3);

#PayOff

payoff_array=pmax(z*(ST1-ST2-K),z*(ST3-ST2-K),0)

#MC Values

Value=mean(payoff_array)*exp(-r*T)

#Convergence Test

#gap1=mean(ST1)-S1*exp(b1*T)
#gap2=mean(ST2)-S2*exp(b2*T)
#gap3=mean(ST3)-S3*exp(b3*T)

#