# Input 

S = 100; X = 100; r = 0.1; T = 0.5; sigmaBlack = 0.2; q = 0.05; NSim=10^5

# Black-Scholes-Merton (or Log-Normal) model - SDE: Geometric Brownian Motion

GBMdynamics <- function(S, r, q , T, sigma, NSim){
  epsilon=rnorm(NSim)
  S_T=S*exp((r-q-(sigma^2)/2)*T+sigma*epsilon*sqrt(T))
  return(S_T)
}

S_T_GBM=GBMdynamics(S, r, q , T, sigmaBlack, NSim)

Momento1_GBM_MC=mean(S_T_GBM)
Momento2_GBM_MC=var(S_T_GBM)

Momento1_GBM_th=S*exp((r-q)*T)
Momento2_GBM_th=S^2*exp(2*(r-q)*T)*(exp(sigmaBlack^2*T)-1)

vol_ST_GBM=sqrt(Momento2_GBM_th)

ValueAtRisk_GBM = quantile(S_T_GBM, probs = 0.05)

# Bachelier (or Normal) model - SDE: Arithmetic Brownian Motion

getNormalVol = function(sigmaBlack,Fwd,Strike,Time){
  sigmaBachelier=sigmaBlack*((Fwd-Strike)/(log(Fwd/Strike)))*(1-(((sigmaBlack^2)*Time)/(24)))
  return(sigmaBachelier)
}

ABMdynamics <- function(S, r, q , T, sigma, NSim){
  epsilon=rnorm(NSim)
  S_T=S+(r-q)*T+sigma*epsilon*sqrt(T)
  return(S_T)
}

Fwd=S*exp((r-q)*T)

sigma_Bachelier=getNormalVol(sigmaBlack,Fwd,X,T)

S_T_ABM=ABMdynamics(S, r, q , T, sigma_Bachelier, NSim)

Momento1_ABM_MC=mean(S_T_ABM)
Momento2_ABM_MC=var(S_T_ABM)

Momento1_ABM_th=S+(r-q)*T
Momento2_ABM_th=(sigma_Bachelier^2)*T

vol_ST_ABM=sqrt(Momento2_ABM_th)

ValueAtRisk_ABM = quantile(S_T_ABM, probs = 0.05)
