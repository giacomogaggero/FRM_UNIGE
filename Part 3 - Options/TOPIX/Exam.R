MKT_Info_provider <- read.csv("C:/Users/U23126/Desktop/MKT_Info_provider.csv")

getOptionValue = function(S,X,T,r,q,sigma,optiontype){
  d1=(log(S/X)+(r-q+sigma^2/2)*T)/(sigma*sqrt(T))
  d2=d1-sigma*sqrt(T)
  if(optiontype=='c'){
    Value=S*exp(-q*T)*pnorm(d1)-X*exp(-r*T)*pnorm(d2)  
  } else if(optiontype=='p'){
    Value=X*exp(-r*T)*pnorm(-d2)-S*exp(-q*T)*pnorm(-d1)
  } else {
    Value=NA
  }
  return(Value)
  
}

S=MKT_Info_provider$Spot.Price[1]
X=MKT_Info_provider$Strike.Price
DF=MKT_Info_provider$Discount.Factor
sigma=MKT_Info_provider$Volatility
q=MKT_Info_provider$Dividend.Yield
ValDate=as.Date(MKT_Info_provider$ValuationDate[1],"%Y/%m/%d")
Maturities=as.Date(MKT_Info_provider$Expiration.Date,"%Y/%m/%d")
T=as.numeric((Maturities-ValDate)/360)
r=-(log(DF)/T)

Values=numeric(length(Maturities))

for (i in (1:length(Maturities))) {
    if(S>=X[i]){
      Values[i]=getOptionValue(S,X[i],T[i],r[i],q[i],sigma[i],'c')
    } else {
      Values[i]=getOptionValue(S,X[i],T[i],r[i],q[i],sigma[i],'p')
  }
}

Prices_matrix=t(matrix(Values,nrow = 15,byrow = FALSE))
Vol_matrix=t(matrix(sigma,nrow = 15,byrow = FALSE))

#install.packages("plot3D")
library(plot3D)

# Create surface plots

Volsurf <- persp3D(unique(T),unique(X) , Vol_matrix, theta = 30, phi = 30,
                       col = "lightblue", border = NA)

Pricingsurf <- persp3D(unique(T),unique(X) , Prices_matrix, theta = 30, phi = 30,
                col = "lightblue", border = NA)
