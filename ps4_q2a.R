#q2a
#library
#library(dplyr)
#library(tidyverse)

#source
source('ps4_q2_funcs.R')

#function
func_a=function(i,n = 1e3, p = 1e2, r = .1, sigma = 1, mc_rep = 1e4){
  # Arguments:
  #   X : an n by p numeric matrix 
  #   beta: a p by 1 numeric matrix 
  #   sigma: std deviation for Y|X,  Y|X ~ N(XB, sigma^2 I)
  #   mc_rep: The number of Monte Carlo replications to use
  #
  # Output: table of results 
  #rho
  rho = .25*i
  
  #beta: p*1 vector
  beta = c(rep(.1, floor(r*p)), rep(0, p - floor(r*p)) ) 
  dim(beta) = c(p, 1)
  
  #X matrix
  Sigma=beta%*%t(beta)*rho
  diag(Sigma)=1
  R = chol(Sigma)
  X = matrix(rnorm(n*p), n, p) %*%  R
  
  # Simulate Y from Y|X ~ N(XB, sigma^2 I) and compute p-values corresponding to
  # Wald tests for B != 0. Repeat mc_rep times.
  P=sim_beta(X, beta, sigma, mc_rep)
  
  #compare hypothesis tests with adjusted p-values:
  all0 =
    lapply( c('holm', 'bonferroni', 'BH', 'BY'), function(x){
      evaluate( apply(P, 2, p.adjust, method = x), tp_ind = 1:10)
    })
  
  
  #data.table to data.frame #rbindlist
  all=data.frame(matrix(unlist(all0),16,2,byrow = T))
  names(all)=c('est','se')
  #compute se and estimates
  #m = qnorm(.975)
  #ci = list()
  #for(x in c('fwer', 'fdr', 'sens', 'spec')){
  #  ci[[x]] = all[ , .(est = .SD[[x]], se = .SD[[paste0(x,'_se')]])]
  #}
  
  #return a data.table
  #data.table(rho=rep(rho,16),sigma= rep(sigma,16))%>%
  #  .[,`:=`(metric=rep(attributes(ci)$names,each=4), method=rep(c('holm', 'bonferroni', 'BH', 'BY') ,4))]%>%
  #  cbind(do.call('rbind',ci))
  result=data.frame(rho=rep(rho,16),sigma= rep(sigma,16),
             metric=rep(c('fwer', 'fdr', 'sens', 'spec'),4), 
             method=rep(c('holm', 'bonferroni', 'BH', 'BY'),each=4))
  cbind(result,all)
}

#data.table to data.frame
results_q4a=parallel::mclapply(-3:3,func_a)
results_q4a=as.data.frame(do.call('rbind', results_q4a))

#create table
knitr::kable(results_q4a,digits = 3)



