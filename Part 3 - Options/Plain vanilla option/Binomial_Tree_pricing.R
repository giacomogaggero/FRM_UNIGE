#Input parameter for the Put Option

z=-1;S=34364.97;X=34364.97;r=3.766/100;q=2.592/100;b=r-q;T=180/360;sigma=14.921/100;NStep=500

#BS closed formula pricing


d1=(log(S/X)+(r-q+sigma^2/2)*T)/(sigma*sqrt(T))
d2=d1-sigma*sqrt(T)
BS_EUR_Put=(X*exp(-r*T)*pnorm(-d2))-(S*exp(-q*T)*pnorm(-d1))


#CRR European Option pricing

DeltaT=T/NStep
u=exp(sigma*sqrt(DeltaT));d=1/u;p=(exp(b*DeltaT)-d)/(u-d)
OptionValue<-vector(mode="numeric",length=NStep+1)
CRR_EUR_OptionPrice_Array<-vector(mode="numeric",length=NStep)
for (k in 1:NStep) {
  for (i in 0:k) {
    OptionValue[i+1]=max(0,z*(S*u^i*d^(k-i)-X));
  }
  for (j in seq(k-1,0,-1)) {
    for (i in 0:j) {
      OptionValue[i+1]=(p*OptionValue[i+2]+(1-p)*OptionValue[i+1])*exp(-r*DeltaT);
    }
  }
  CRR_EUR_OptionPrice_Array[k]=OptionValue[1]
}
CRR_EUR_Put=CRR_EUR_OptionPrice_Array[length(CRR_EUR_OptionPrice_Array)]
plot(c(1:NStep),CRR_EUR_OptionPrice_Array,type="l",xlab="Step",ylab="Price")


#CRR American Option pricing

DeltaT=T/NStep
u=exp(sigma*sqrt(DeltaT));d=1/u;p=(exp(b*DeltaT)-d)/(u-d)
OptionValue<-vector(mode="numeric",length=NStep+1)
CRR_AM_OptionPrice_Array<-vector(mode="numeric",length=NStep)
for (k in 1:NStep) {
  for (i in 0:k) {
    OptionValue[i+1]=max(0,z*(S*u^i*d^(k-i)-X));
  }
  for (j in seq(k-1,0,-1)) {
    for (i in 0:j) {
      AssetPrice=S*u^i*d^(j-i);
      OptionValue[i+1]=max((p*OptionValue[i+2]+(1-p)*OptionValue[i+1]),z*(AssetPrice-X))*exp(-r*DeltaT);
    }
  }
  CRR_AM_OptionPrice_Array[k]=OptionValue[1]
}
CRR_AM_Put=CRR_AM_OptionPrice_Array[length(CRR_AM_OptionPrice_Array)]
plot(c(1:NStep),CRR_AM_OptionPrice_Array,type="l",xlab="Step",ylab="Price")

#LR American Option pricing

h_f <- function(z,n){
  return(0.5+sign(z)*(0.25-0.25*exp(-(z/(n+1/3))^2*(n+1/6)))^0.5)
}

OptionValue<-vector(mode="numeric",length=NStep+1)
LR_AM_OptionPrice_Array<-vector(mode="numeric",length=NStep)
for (k in 1:NStep) {
  DeltaT=T/NStep
  p=h_f(d2,k)
  u=exp(b*DeltaT)*h_f(d1,k)/h_f(d2,k)
  d=(exp(b*DeltaT)-p*u)/(1-p)
  for (i in 0:k) {
    OptionValue[i+1]=max(0,z*(S*u^i*d^(k-i)-X));
  }
  for (j in seq(k-1,0,-1)) {
    for (i in 0:j) {
      AssetPrice=S*u^i*d^(j-i);
      OptionValue[i+1]=max((p*OptionValue[i+2]+(1-p)*OptionValue[i+1]),z*(AssetPrice-X))*exp(-r*DeltaT);
    }
  }
  LR_AM_OptionPrice_Array[k]=OptionValue[1]
}
LR_AM_Put=LR_AM_OptionPrice_Array[length(LR_AM_OptionPrice_Array)]
plot(c(1:NStep),LR_AM_OptionPrice_Array,type="l",xlab="Step",ylab="Price")

#Convergence speed comparison

mat <- matrix(1:2,2,1)
layout(mat)
plot(c(1:NStep),CRR_AM_OptionPrice_Array,type="l",xlab="Step",ylab="Price",main='American CRR Tree')
plot(c(1:NStep),LR_AM_OptionPrice_Array,type="l",xlab="Step",ylab="Price",main='American LR Tree')




