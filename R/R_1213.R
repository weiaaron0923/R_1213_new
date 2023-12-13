obs = 500
M = 170
S = 10
Coler = "blue"
dist = rnorm(obs,mean=M,sd=S)
hist(dist,col=Coler)

Sig = 0.05 #two-sided significance level
p = 0.5 #The expected rate
X = 0.03 #Acceptable error range
Z = qnorm(1-Sig/2) #The Z value based on the two-sided significance level
n = p*(1-p)*Z^2/X^2 #Calculate the needed number of sample
n #Print the outcome
Samplesize = function(Sig,p,X) {
  Z = qnorm(1-Sig/2)
  n = p*(1-p)*Z^2/X^2
  n
}
Samplesize(Sig = 0.05,p = 0.5, X = 0.03) #Test 1
Samplesize(Sig = 0.05,p = 0.2, X = 0.03) #Test 2
Samplesize(Sig = 0.05,p = 0.2, X = 0.05) #Test 3

Samplesize_OR = function(alpha,power,OR,r,p0,corr=FALSE) {
  Za=qnorm(1-alpha/2)
  Zb=qnorm(power)
  q0=1-p0
  p1=OR*p0/(OR*p0+q0)
  q1=1-p1
  pbar=(p0+r*p1)/(r+1)
  qbar=1-pbar
  n0=(Za*sqrt((r+1)*pbar*qbar)+Zb*sqrt(r*p0*q0+p1*q1))^2/(r*(p0-p1)^2)
  n1=r*n0
  n0corr=n0/4*(1+sqrt(1+(2*(1+r)/(n1*r*sqrt((p0-p1)^2)))))^2
  n1corr=r*n0corr
  if (corr==FALSE) {return(paste("Control:"
                                 ,round(n0)+1,
                                 "; Case:"
                                 ,round(n1)+1,
                                 "; Total:"
                                 ,round(n0)+round(n1)+2,sep=""))}
  if (corr==TRUE) {return(paste("Control:"
                                ,round(n0corr)+1,
                                "; Case:"
                                ,round(n1corr)+1,
                                "; Total:"
                                ,round(n0corr)+round(n1corr)+2,sep=""))}
}
Samplesize_OR(alpha=0.05,power=0.8,OR=1.5,r=1,p0=0.2,corr=FALSE)
Samplesize_OR(alpha=0.05,power=0.8,OR=1.5,r=1,p0=0.2,corr=TRUE)
