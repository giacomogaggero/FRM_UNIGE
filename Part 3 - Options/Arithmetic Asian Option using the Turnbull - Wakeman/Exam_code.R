# Implied dividend yield from call-put parity 

S=146.67
X=142.5
T=120/360
r=3.71/100
Cmkt=(8.4+9.55)/2
Pmkt=(3.85+4)/2

implied_q=-(1/T)*log((1/S)*(Cmkt-Pmkt+X*exp(-r*T)))

# Implied volatility

BSMprice <- function(S, X, r, q, T, sigma ){
  d1=(log(S/X)+(r-q+sigma^2/2)*T)/(sigma*sqrt(T))
  d2=d1-sigma*sqrt(T)
  Call=(S*exp(-q*T)*pnorm(d1))-(X*exp(-r*T)*pnorm(d2))
  return(Call)
}

getImplVol <- function(S, X, r, q, T, sigma, Cmkt ){
vLow=0.01
vHigh=1
epsilon=1e-8
CLow=BSMprice(S,X,r,q,T,vLow)
CHigh=BSMprice(S,X,r,q,T,vHigh)
counter=0
vi=vLow+(Cmkt-CLow)*(vHigh-vLow)/(CHigh-CLow)
while (abs(Cmkt-BSMprice(S,X,r,q,T,vi))>epsilon) {
  counter=counter+1
  if(counter==100){
    return(NA)
  }
  if(BSMprice(S,X,r,q,T,vi)<Cmkt){
    vLow=vi
  }
  else{
    vHigh=vi
  }
  CLow=BSMprice(S,X,r,q,T,vLow)
  CHigh=BSMprice(S,X,r,q,T,vHigh)
  vi=vLow+(Cmkt-CLow)*(vHigh-vLow)/(CHigh-CLow)
}
return(vi)
}

implied_vol=getImplVol(S,X,r,implied_q,T,sigma,Cmkt)
#ATM_fwd_approx=(((5.9+6.10)/2)*sqrt(2*pi))/(S*exp((-implied_q)*T)*sqrt(T))
#Turnbull and Wakeman - Asian Option

asian_option_tw <- function(S, K, T, r, b, sigma) {
  M1 <- (exp(b * T) - 1) / (b * T)
  M2 <- ((2 * exp((2 * b + sigma^2) * T))/ ((b + sigma^2) * (2 * b + sigma^2) * T^2)) + (2 / (b * T^2)) * (1 / (2 * b + sigma^2) - exp(b * T) / (b + sigma^2))
  b_ADJ <- log(M1) / T
  sigma_ADJ <- sqrt(log(M2) / T - 2 * b_ADJ)
  d1 <- (log(S / K) + (b_ADJ + (sigma_ADJ^2) / 2) * T) / (sigma_ADJ * sqrt(T))
  d2 <- d1 - sigma_ADJ * sqrt(T)
  Asiancall_price <- S * exp((b_ADJ - r) * T) * pnorm(d1) - K * exp(-r * T) * pnorm(d2)
  return(Asiancall_price)
}


AsianPrice=asian_option_tw(S, X, T, r, r-implied_q, implied_vol)


