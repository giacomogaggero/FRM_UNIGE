Spot=4294.09
PaymentDates=c(0.25,1.25,2.25)
Strike_Prices = c(4150,4175,4200)
Coupons=c(3,3.5,4)/100
r=approx(c(0.25,1,1.5,2,3),c(3.76,3.923,3.824,3.673,3.451),PaymentDates)$y/100
q=approx(c(0.25,1,1.5,2,3),c(1.567,3.041,2.522,2.966,2.888),PaymentDates)$y/100
b=r-q
sigma1=approx(c(0.25,1,1.5,2,3),c(18.80,18.99,19.35,18.99,18.95),PaymentDates[1])$y/100
sigma2=approx(c(0.25,1,1.5,2,3),c(18.40,18.77,19.18,18.85,18.83),PaymentDates[2])$y/100
sigma3=approx(c(0.25,1,1.5,2,3),c(17.99,18.56,19.02,18.70,18.72),PaymentDates[3])$y/100
sigma=c(sigma1,sigma2,sigma3)

d <- (log(Spot/Strike_Prices)+(b-sigma^2/2)*T)/(sigma*sqrt(T))
Prices=Coupons*exp(-r*PaymentDates)*pnorm(d)
Strip=sum(Prices)*100

#MonteCarlo
NSim=10^8
C1=numeric(NSim)
C2=numeric(NSim)
C3=numeric(NSim)
ST1=Spot*exp((b[1]-sigma[1]^2/2)*PaymentDates[1]+sigma[1]*rnorm(NSim)*sqrt(PaymentDates[1]))
ST2=Spot*exp((b[2]-sigma[2]^2/2)*PaymentDates[2]+sigma[2]*rnorm(NSim)*sqrt(PaymentDates[2]))
ST3=Spot*exp((b[3]-sigma[3]^2/2)*PaymentDates[3]+sigma[3]*rnorm(NSim)*sqrt(PaymentDates[3]))

C1[ST1>Strike_Prices[1]]=Coupons[1]
C2[ST2>Strike_Prices[2]]=Coupons[2]
C3[ST3>Strike_Prices[3]]=Coupons[3]

P_C1=mean(C1)*exp(-r[1]*PaymentDates[1])
P_C2=mean(C2)*exp(-r[2]*PaymentDates[2])
P_C3=mean(C3)*exp(-r[3]*PaymentDates[3])

Strip_Price_MC=(P_C1+P_C2+P_C3)*100

gap=Strip_Price_MC-Strip
