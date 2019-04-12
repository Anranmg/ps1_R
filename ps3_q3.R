#q3
### Libraries: --------------------------------
library(data.table)
library(dplyr)
library(tidyr)

### working directory-------------------------
#setwd("./Downloads/ps")

###a. translation using data.table for the computations---------------------
mtcars=as.data.table(mtcars)
dt=mtcars[,lapply((.SD-lapply(.SD,mean))*mpg,sum),by=cyl,.SDcols=c("disp","hp","wt")]/mtcars[,lapply((.SD-lapply(.SD,mean))^2,sum),by=cyl,.SDcols=c("disp","hp","wt")]
dt_a=data.table(cyl=unique(mtcars[,cyl]),dt[,-1])%>%
  .[order(cyl)]
fwrite(dt,"mpg_betas_by_cyl.csv")


###b. functions to compute coefficient---------------------
#Use data.table for computations within your function
reg=function(x){
  fm=as.formula(paste("mpg~",x))
  dt=mtcars[,lm(fm,data=.SD)$coef[2],keyby=cyl]
  names=c(key(dt),x)
  setnames(dt,names)
  return(dt)
}
dt_b=reg("disp")[reg("hp")][reg("wt")]

#check whether same results-----------------------
all(dt_a-dt_b<1e-3)

#mtcars[, {assign('mpg', mpg, environment(fm)); assign('disp', disp, environment(fm));lm(fm)}$coef[2],by=cyl]

## c. Compute the regression coefficients using the dplyr verb summarize_at()----------------
# calculate sum of mpg and disparity of each variable with the average value
df=mtcars%>%
  gather(key=type,val=measure,disp,hp,wt)%>%
  left_join(mtcars%>%
              group_by(cyl)%>%
              summarize_at(c("disp","hp","wt"),mean)%>%
              gather(key=type,val=mn,disp,hp,wt),by = c("cyl", "type"))%>%
  mutate(measure=(measure-mn)*mpg)%>%
  select(-mn)%>%
  spread(key=type,value=measure)%>%
  group_by(cyl)%>%
  summarize_at(c("disp","hp","wt"),sum)

# calculate sum squares: disparity of each variable with the average value  
van=function(x){
  sum((x-mean(x))^2)
}
df1=mtcars%>%
  group_by(cyl)%>%
  summarize_at(c("disp","hp","wt"),van) 

#coef  
df_c=cbind(df[1],(df/df1)[-1])

## d. function, dplyr within function--------------------------
reg2=function(x){
  fm=as.formula(paste("mpg~",x))  
  mtcars%>%group_by(cyl)%>%
    do(fit=lm(fm,data=.)$coef[2])%>%
    select(fit)%>%
    unlist(.)
}
df_d=sapply(c("disp","hp","wt"),reg2)
rownames(df_d)=NULL
df_d=cbind(df[1],df_d)


#reg2=function(x){
#  dt=mtcars%>%group_by(cyl)%>%filter(cyl==unique(mtcars$cyl)[i])
#  fm=as.formula(paste("mpg~",x))
#  lm(fm,dt)$coef[2]
#}  
#broom::tidy()



# ####tables & graphs------
# #a
# knitr::kable(dt_a)
# #b
# knitr::kable(dt_b)
# #c
# knitr::kable(df_c)
# #c
# knitr::kable(df_d)
