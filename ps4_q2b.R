#2b
#Setup a 4 core cluster using doParallel
#sigma=.25,.5,1
#use PBS file to run

#library 
#install.packages("doParallel")
library(parallel)
library(doParallel)

#core used in the cluster
ncores=4

#set up cluster called 'c1'
c1=makeCluster(ncores)

#regiter the cluster
registerDoParallel(c1)

#source
# Part a, Monte Carlo simulation function for fixed X: ------------------------
sim_beta = function(X, beta, sigma = 1, mc_rep = 1e3){
  # Simulate Y from Y|X ~ N(XB, sigma^2 I) and compute p-values corresponding to
  # Wald tests for B != 0. Repeat mc_rep times.
  #
  # Arguments:
  #   X : an n by p numeric matrix
  #   beta: a p by 1 numeric matrix
  #   sigma: std deviation for Y|X,  Y|X ~ N(XB, sigma^2 I)
  #   mc_rep: The number of Monte Carlo replications to use
  #
  # Output: A p by mc_rep matrix of p-values
  
  # This part doesn't need to change for each replication
  QR = qr( crossprod(X) )
  QX = X %*% qr.Q(QR) 
  XtXinv = solve( qr.R(QR), t( qr.Q(QR) ))
  
  n = nrow(X)
  p = ncol(X)
  
  # Generate mc_rep copies of Y at once, each in a column.
  Y = as.numeric(X %*% beta) + rnorm(n*mc_rep)
  dim(Y) = c(n, mc_rep)
  
  # estimate betas and residual standard errors
  b = solve(qr.R(QR), crossprod( QX, Y ) )
  
  # It's okay if you divide by {n - p} outside the sum, but this
  # is more comparable to what is done by .lm.fit()
  s_sq = colSums( {Y - as.numeric(X %*% b)}^2 / {n - p})
  
  # standard error of b
  v = sqrt( diag(XtXinv) * rep(s_sq, each = p) )
  
  # return a matirx of p-values
  # Use pt to replicate lm, but the normal approximation is fine here. 
  matrix( 2*pt( abs( b / v ), df = {n-p}, lower.tail = FALSE ), p, mc_rep )  
}

# Part c, evaluate: -----------------------------------------------------------
evaluate = function(P, tp_ind, alpha = .05){
  P = P < alpha
  
  p = nrow(P)
  n = ncol(P)
  
  # Compute TP, FP, TN, FN for each replcation
  TP = colSums(P[tp_ind, ])
  FP = colSums(P[-tp_ind,])
  TN = colSums(!P[-tp_ind,])
  FN = colSums(!P[tp_ind,])
  
  # Call FDR 0 when no discoveries. 
  P = FP + TP
  fdr = ifelse(P > 0, FP  / {FP + TP}, 0)
  fwer = mean( FP > 0 )
  sens = TP / {TP + FN}
  spec = TN / {FP + TN}
  
  list( fwer = fwer, fwer_se = sqrt(fwer*{1-fwer} / n), 
        fdr =  mean(fdr), fdr_se = sd(fdr) / sqrt(n),
        sens = mean(sens), sens_se = sd(sens) / sqrt(n),
        spec = mean(spec), spec_se = sd(spec) / sqrt(n)
  )
}

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

#parallel computations with foreach
#return as a list   7*3=21 (permutation times); (21*4*4=21*16=336 rows)

results_q4b=foreach(n=0:20) %dopar%{
  func_a(i=n%/%3-3,n = 1e3, p = 1e2, r = .1, sigma=.25*2^(n%%3), mc_rep = 1e4)
}


#reshape to data.frame
results_q4b=as.data.frame(do.call('rbind', results_q4b))

#save file 
save(results_q4b,file="results_q4b.RData")
print(results_q4b)
ls()

#shut down
stopCluster(c1)










batting_tbl%>%
  filter(teamID=='DET' & yearID==2016 & AB>=100)%>%
  transmute(playerID, avg=H / AB)%>%

batting_tbl%>%
  filter(yearID>=2000)%>%
  group_by(yearID,playerID)%>%
  summarize(n=sum(H))%>%  #still group by yearid
  filter(n>=200)%>%
  summarize(no=n())%>%
  arrange(-yearID)
  
data.table:
  select(a,b,c=c,d)

cast(h as float)/ cast(b as float)  as 
max(a,na.rm=T)
filter(a==max(a,na.rm = T))

summaarzie(sum(h)/sum(ab))

query='
select count(playerID) as n
from( select playerID, sum(H) as H
 from batting
 where yearid>=2000
 group by playerID, yearid
)
group by playerID
having min(H)>=200
'

query='
select yearID as year, count(playerID) as n
from( 
select playerID, yearID，sum(H) as H
from batting
where yearid>=2000
group by playerID, yearID
having H>=200
)
group by yearID
order by -yearID
'

lahman %>% tbl(sql(query)) %>% collect()

library(dplyr)
library(data.table)
sw_dt=as.data.table(starwars%>%select(name,height,mass,homeworld,species))
sw_tbl=starwars

sw_tbl%>%
  group_by(homeworld)%>%
  summarise(n=(unique(species))%>%
  summarise(n=length(unique(species)))%>%
  filter(n>1)%>%
  arrange(-n)

sw_tbl%>%
  mutate(rt=mass/height)%>%
  group_by(species)%>%
  summarise(n=sum(!is.na(rt)),rt=mean(rt,na.rm = T))%>%
  filter(n>1)%>%
  filter(rt==max(rt))

sw_tbl%>%
  mutate(rt=mass/height)%>%
  group_by(species)%>%
  summarise(rt=mean(rt,na.rm = T))%>%
  filter(rt==max(rt,na.rm=T))



sw_dt[,unique(species),by=homeworld][,.(n=.N),by=homeworld][n>1][order(-n)]


sw_dt[,.(n=length(unique(species))),by=homeworld][n>1][order(-n)]
sw_dt=na.omit(sw_dt)

sw_dt[,.SD[which.min(height)],by=.(homeworld,species),.SDcols=c('mass','height')]
