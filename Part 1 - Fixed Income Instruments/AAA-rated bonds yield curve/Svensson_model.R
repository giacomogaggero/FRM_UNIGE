# libraries
library(readxl)
# Function
SvenssonModel = function(T,beta0,beta1,beta2,beta3,tau1,tau2){
  spotvalue=beta0+beta1*((tau1/T)*(1-exp(-T/tau1)))+beta2*(((tau1/T)*(1-exp(-T/tau1)))-exp(-T/tau1))+beta3*(((tau2/T)*(1-exp(-T/tau2)))-exp(-T/tau2))
  return(spotvalue)
}
#Script

path='C:/Users/Utente/Desktop/Risk Management Techniques/Exams/20240119/MKT_Data.xlsx'
T=read_excel(path, sheet = 'AAA_bond_yield',range='A2:A35')$Time2Maturity
Zerorates=read_excel(path, sheet = 'AAA_bond_yield',range='C2:C35')$SpotRates


NLS_obj=nls(Zerorates ~ beta0+beta1*((tau1/T)*(1-exp(-T/tau1)))+beta2*(((tau1/T)*(1-exp(-T/tau1)))-exp(-T/tau1))+beta3*(((tau2/T)*(1-exp(-T/tau2)))-exp(-T/tau2)), start=list(beta0=0.5,beta1=3,beta2=-3,beta3=4,tau1=2,tau2=10))

coeffhat=summary(NLS_obj)

beta0=coeffhat$coefficients['beta0','Estimate']
beta1=coeffhat$coefficients['beta1','Estimate']
beta2=coeffhat$coefficients['beta2','Estimate']
beta3=coeffhat$coefficients['beta3','Estimate']
tau1=coeffhat$coefficients['tau1','Estimate']
tau2=coeffhat$coefficients['tau2','Estimate']

th_spot= SvenssonModel(T,beta0,beta1,beta2,beta3,tau1,tau2)

ModelGap=mean(abs(th_spot-Zerorates))

split.screen(c(1,1))
plot(T,th_spot, type="l", lwd=2, col="blue")
screen(1, new=FALSE) 
plot(T,Zerorates, type="p", lwd=2, col="red", xaxt="n",
     yaxt="n", ylab="", xlab="", main="", bty="n")



# using ECB parameter

ECBparams=c(0.956762,  #beta0
            3.078775,  #beta1
            -3.322735, #beta2
            4.64512,   #beta3
            2.286809,  #tau1
            11.676781  #tau2
)


ECB_th_spot= SvenssonModel(T,ECBparams[1],ECBparams[2],ECBparams[3],ECBparams[4],ECBparams[5],ECBparams[6])

ModelGap=mean(abs(ECB_th_spot-Zerorates))

split.screen(c(1,1))
plot(T,ECB_th_spot, type="l", lwd=2, col="blue")
screen(1, new=FALSE) 
plot(T,Zerorates, type="p", lwd=2, col="red", xaxt="n",
     yaxt="n", ylab="", xlab="", main="", bty="n")

