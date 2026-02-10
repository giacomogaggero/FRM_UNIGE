# Arithmetic Asian Option (Fixed strike) pricing
S=4425.75
K=4425.75
tau=c(1/12,2/12,3/12)
sigma=13.407/100
r=3.988/100
q=1.265/100
nSim=10^5
eps1=rnorm(nSim)
eps2=rnorm(nSim)
eps3=rnorm(nSim)
S_t1=S*exp((r-q)*tau[1]+sigma*eps1*sqrt(tau[1]))
S_t2=S_t1*exp((r-q)*(tau[2]-tau[1])+sigma*eps2*sqrt(tau[2]-tau[1]))
S_t3=S_t2*exp((r-q)*(tau[3]-tau[2])+sigma*eps3*sqrt(tau[3]-tau[2]))
Smatrix=matrix(c(S_t1,S_t2,S_t3),nrow=nSim)
pay_off=pmax(rowMeans(Smatrix)-K,0)
NPV=mean(pay_off)*exp(-r*tau[3])
print(NPV)
#check fwd
check1=mean(S_t1)-S*exp((r-q)*tau[1])
check2=mean(S_t2)-S*exp((r-q)*tau[2])
check3=mean(S_t3)-S*exp((r-q)*tau[3])
