#q2
## x:n*p y: n*1,  n*1=(n*p)(p*1)
library(dplyr)
library(ggplot2)
## a. function----------------------------------
# return p*mc_rep: p_value for hypothesis test
## t-statistic=(beta-null)/se(beta) , p-value=(1-pnorm(t))*2
## qr decomposition  r*beta=q'y
#  x=n*p, beta=*mcrep,y=n*mcrep


p_value=function(X,beta,sigma,mc_rep){
  n=1000
  p=100
  set.seed(1)
  Y=rnorm(n*mc_rep,mean=X%*%beta,sd=diag(sigma,p,p))  #n*mcrep
  dim(Y)=c(n,mc_rep)
  QR=qr(X)
  beta_hat=solve(qr.R(QR),t(qr.Q(QR))%*%Y)  #dim:p*crep
  Y_hat=X%*%beta_hat
  sigma2_hat=colSums((Y-Y_hat)^2)/(n-p)   #dim=mcrep*1
  dim(sigma2_hat)=c(1,mc_rep)
  v=diag(solve(t(qr.R(QR))%*%qr.R(QR)))%*%sigma2_hat  # dim=p*mcrep   p*1
  z=beta_hat/sqrt(v)   
  return (2*(1-pnorm(z)))
}

#testing:--------------------------------------------------
# Sigma=1, sigma=1
#function computation
n=1000
p=100
X=matrix(rnorm(n*p,0,1),n,p)
beta=c(rep(1,10),rep(0,p-10))
pvalue0=p_value(X,beta,sigma=1,mc_rep=1)

#linear regression:
mc_rep=1
set.seed(1)
Y=rnorm(n*mc_rep,mean=X%*%beta,sd=diag(1,p,p))
dim(Y)=c(n,mc_rep)
fit=lm(Y~0+X)
pvalue1=summary(fit)$coef[,4]

#compared with 2 results
max(pvalue1-pvalue0)<1e-3


## b. chosen Sigma (correlation of xi,xj=0.1^abs(i-j)) and sigma=1----------------------------------
Sigma=matrix(0,nrow=p,ncol=p)
for (i in 1:p){
  for (j in 1:p){
    Sigma[i,j]=0.01^abs(i-j) 
  }
}
R=chol(Sigma)
X=matrix(rnorm(n*p,0,1),n,p)
X=X%*%R
beta=c(rep(1,10),rep(0,p-10))
pvalue=p_value(X,beta,sigma=1,mc_rep=1e3)

##graph-------------------
#ggplot() +
#  geom_errorbar(aes(x=1:100, ymin=apply(pvalue,1,min), ymax=apply(pvalue,1,max)), 
#                width=0.2,size=1.5,colour="grey50") +
#  scale_y_log10() + xlab("x") + ylab("p-value")



## c. evaluation function ------------------------------------------
# null: beta=0 
# set: first ten not 0, last 90 be 0
# type1: if reject last 90,   
# type2: if not reject first 10
# true positive: not reject last 90
# fmwe=probablity of making at least one type 1 error, 
#              (times of reject last 90)/(nm of tests)
# fdr= proportion of false discoveries among the discoveries
# sensi=correctly detect ill patients who do have the condition
#          (times of not reject last 90)/(nm of tests)
evaluate=function(beta,pvalue,mc_rep,alpha){
  p=length(beta)
  m1=sum(beta==1)
  m0=p-m1
  tp=colSums(pvalue[(m1+1):p,]>alpha)
  fn=colSums(pvalue[(m1+1):p,]<alpha)
  tn=colSums(pvalue[1:m1,]<alpha)
  fp=colSums(pvalue[1:m1,]>alpha)
  fwer=mean(fn>=1)
  fdr=mean(fn/(fn+tn))
  sen=mean(tp/m0)  #tp+fn
  spe=mean(tn/m1)  #tn+fp
  c('fwer'=fwer,'fdr'=fdr,'sensitivity'=sen,'specifity'=spe)
}


## d. evaluation function ------------------------------------------
test=function(method){
  pval=p.adjust(pvalue, method)
  mc_rep=1e3
  dim(pval)=c(100,mc_rep)
  evaluate(beta,pval,mc_rep,alpha=0.025) 
}
method=c("none","bonferroni","holm","BH","BY")

## outout table
#t(sapply(method,test))%>%kable(digit=3)

