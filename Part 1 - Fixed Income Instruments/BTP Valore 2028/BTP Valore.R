################### Gross Values ####################

library(lubridate)
# from BTP Valore termsheet
ValuationDate=as.Date('2024-03-01','%Y-%m-%d')
SettleDate=as.Date('2024-03-05','%Y-%m-%d')
LastPaymentDate=as.Date('2024-01-10','%Y-%m-%d')
FuturePaymentDates=LastPaymentDate %m+% months(seq(from=3,to=3*19,by=3))
Face=100
Coupon_Amount=c(1.025,1.125)
FutureCashFlows=c(rep(Coupon_Amount[1],each=11),rep(Coupon_Amount[2],each=8))
FutureCashFlows[length(FutureCashFlows)]=FutureCashFlows[length(FutureCashFlows)]+Face
T=time_length(difftime(FuturePaymentDates, ValuationDate), "years")
DiscountRate=1/100
DFs=1/((1+DiscountRate)^T)
NPV=FutureCashFlows*DFs
DirtyPrice=sum(NPV)
Accrued=time_length(difftime(SettleDate,LastPaymentDate), "days")/time_length(difftime(FuturePaymentDates[1],LastPaymentDate), "days")*Coupon_Amount[1]
CleanPrice=DirtyPrice-Accrued

################### goal seeking for YTM ####################

MKTPrice=102.66

findYTM <- function(YTM,T_in,FutureCashFlows_in,Accrued_in,MKTPrice_in)
{
  DFs=1/((1+YTM)^T_in)
  NPV=FutureCashFlows_in*DFs
  DirtyPrice=sum(NPV)
  CleanPrice=DirtyPrice-Accrued_in
  loss_function=(CleanPrice-MKTPrice_in)^2
}

Solver_output <- optim(par=DiscountRate,fn=findYTM,method='BFGS',T_in=T,FutureCashFlows_in=FutureCashFlows,Accrued_in=Accrued,MKTPrice_in=MKTPrice)

YTM=Solver_output$par
print(round(YTM*100,3))

################### Macaulay Duration ####################

DFs=1/((1+YTM)^T)
NPV=FutureCashFlows*DFs
MacauLayDUR=sum(NPV/sum(NPV)*T)

print(MacauLayDUR)

################### Modified Duration ####################

Modified_DUR=MacauLayDUR/(1+YTM)
print(Modified_DUR)

################### Net Values ####################
tau_tax=12.5/100
# from BTP Valore termsheet
ValuationDate=as.Date('2024-03-01','%Y-%m-%d')
SettleDate=as.Date('2024-03-05','%Y-%m-%d')
LastPaymentDate=as.Date('2024-01-10','%Y-%m-%d')
FuturePaymentDates=LastPaymentDate %m+% months(seq(from=3,to=3*19,by=3))
Face=100
Coupon_Amount=c(1.025,1.125)*(1-tau_tax)
FutureCashFlows=c(rep(Coupon_Amount[1],each=11),rep(Coupon_Amount[2],each=8))
FutureCashFlows[length(FutureCashFlows)]=FutureCashFlows[length(FutureCashFlows)]+Face
T=time_length(difftime(FuturePaymentDates, ValuationDate), "years")
DiscountRate=1/100
DFs=1/((1+DiscountRate)^T)
NPV=FutureCashFlows*DFs
DirtyPrice=sum(NPV)
Accrued=time_length(difftime(SettleDate,LastPaymentDate), "days")/time_length(difftime(FuturePaymentDates[1],LastPaymentDate), "days")*Coupon_Amount[1]
CleanPrice=DirtyPrice-Accrued

################### goal seeking for YTM ####################

MKTPrice=102.66

findYTM <- function(YTM,T_in,FutureCashFlows_in,Accrued_in,MKTPrice_in)
{
  DFs=1/((1+YTM)^T_in)
  NPV=FutureCashFlows_in*DFs
  DirtyPrice=sum(NPV)
  CleanPrice=DirtyPrice-Accrued_in
  loss_function=(CleanPrice-MKTPrice_in)^2
}

Solver_output <- optim(par=DiscountRate,fn=findYTM,method='BFGS',T_in=T,FutureCashFlows_in=FutureCashFlows,Accrued_in=Accrued,MKTPrice_in=MKTPrice)

YTM=Solver_output$par
print(round(YTM*100,3))
