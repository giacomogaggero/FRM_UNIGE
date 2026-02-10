GBSformula <- function(S,X,T,sigma,r,b,optiontype){
  d1 <- (log(S/X)+(b+sigma^2/2)*T)/(sigma*sqrt(T))
  d2 <- d1-sigma*sqrt(T)
  if(optiontype=='Call'){
    optionprice <- S*exp((b-r)*T)*pnorm(d1)-X*exp(-r*T)*pnorm(d2)
  } else if(optiontype=='Put'){
    optionprice <- X*exp(-r*T)*pnorm(-d2)-S*exp((b-r)*T)*pnorm(-d1)
  } else {
    stop('Option Type:Call or Put')
  }
  return(optionprice)
}

S=1555.64;X=1555.64;T=0.25;sigma=12.658/100;r=3.459/100;q=6.805/100;b=r-q
OptionPrice_Call=GBSformula(S,X,T,sigma,r,b,'Call')
OptionPrice_Put=GBSformula(S,X,T,sigma,r,b,'Put')

StraddlePrice=OptionPrice_Call+OptionPrice_Put

#S=50;X=55;T=0.5;sigma=30/100;r=5/100;q=0/100;b=r-q check formula

Delta_Call =pnorm((log(S/X)+(b+sigma^2/2)*T)/(sigma*sqrt(T)))
Delta_Put =pnorm(((log(S/X)+(b+sigma^2/2)*T)/(sigma*sqrt(T))))-1

Delta_Straddle=Delta_Call+Delta_Put

Gamma_Call = (dnorm((log(S/X)+(b+sigma^2/2)*T)/(sigma*sqrt(T))))/(S*sigma*sqrt(T))
Gamma_Put = Gamma_Call

Gamma_Straddle=Gamma_Call+Gamma_Put

Rho_Call = T*X*exp(-r*T)*pnorm((log(S/X)+(b+sigma^2/2)*T)/(sigma*sqrt(T))-sigma*sqrt(T))
Rho_Put = T*X*exp(-r*T)*(pnorm((log(S/X)+(b+sigma^2/2)*T)/(sigma*sqrt(T))-sigma*sqrt(T))-1)

Rho_Straddle=Rho_Call+Rho_Put

Vega_Call = S*sqrt(T)*dnorm((log(S/X)+(b+sigma^2/2)*T)/(sigma*sqrt(T)))
Vega_Put = Vega_Call

Vega_Straddle=Vega_Call+Vega_Put

S_bump=50

OptionPrice_Call_Exact_bump=GBSformula(S+S_bump,X,T,sigma,r,b,'Call')
OptionPrice_Put_Exact_bump=GBSformula(S+S_bump,X,T,sigma,r,b,'Put')

StraddlePrice_Exact_bump=OptionPrice_Call_Exact_bump+OptionPrice_Put_Exact_bump

StraddlePrice_Approx_bump=StraddlePrice+Delta_Straddle*S_bump+0.5*Gamma_Straddle*S_bump^2

Gap=StraddlePrice_Exact_bump-StraddlePrice_Approx_bump

