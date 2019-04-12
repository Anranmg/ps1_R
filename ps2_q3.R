###library packages: -----------------------------------------------------
library(dplyr)

###import data
# Obtain or restore data: -----------------------------------------------------
file = './demo_d.RData'
if (!file.exists(file)) {
  demo_d = foreign::read.xport("./DEMO_D.XPT")
  save(demo_d, file = file)
} else {
  load(file) #demo_d
}

file = './ohx_d.RData'
if (!file.exists(file)) {
  ohx_d = foreign::read.xport("./OHX_D.XPT")
  save(ohx_d, file = file)
} else {
  load(file) #ohx_d
}

# merge two: -----------------------------------------------------
full=ohx_d%>%
  left_join(demo_d,by="SEQN")


# b. run regression: -----------------------------------------------------
full=full%>%
  filter(OHX04HTC!=9)%>%
  transmute(y=as.numeric(OHX04HTC!=1),age=RIDAGEMN,gender=RIAGENDR,race=RIDRETH1,rt=INDFMPIR)

# b. calculate age at prob of 0.25 0.5 0.75 respectively : -----------------------------------------------------
reg=glm(y~age,family=binomial,full)
prob=data.frame(age=1:1200)
prob$y=predict(reg,prob,type="response")
prob_quantile=prob%>%
  mutate(dif1=abs(y-0.25),dif2=abs(y-0.5),dif3=abs(y-0.75))%>%
  filter(dif1==min(dif1)|dif2==min(dif2)|dif3==min(dif3))%>%
  transmute(age,prob=sprintf('%.2f',y))%>%
  arrange(prob)

# b. representative ages in years: -------------------------------------------
rep=floor(prob_quantile[1,1]/12):ceiling(prob_quantile[3,1]/12)


# c. run regression: -----------------------------------------------------
#add gender 
reg_gd=glm(y~age+gender,family=binomial(link = 'logit'),full)
BIC=data.frame(model1=BIC(reg))
BIC$model2=BIC(reg_gd)
#### Not add in 

#ad race/ethnicity
full=full%>%
  mutate(mexican=ifelse(race==1,1,0),
            others=ifelse(race==2 | race==5,1,0),
            black=ifelse(race==4,1,0))
#order of adding indicators
table(full$race)   

reg_3=glm(y~age+black,family=binomial(link = 'logit'),full)
BIC$model3=BIC(reg_3)
##add in

reg_4=glm(y~age+black+mexican,family=binomial(link = 'logit'),full)
BIC$model4=BIC(reg_4)
##not add in

reg_5=glm(y~age+black+others,family=binomial(link = 'logit'),full)
BIC$model5=BIC(reg_5)
##not add in

#ad poverty income ratio
reg_rt=glm(y~age+black+rt,family=binomial(link = 'logit'),full)
BIC$model6=BIC(reg_rt)
#add in

# final model selected: 
fit=reg_rt
summary(reg_rt)


# d. margins: -----------------------------------------------------
pred1=prediction::prediction(fit, at=list(age=rep*12), type='link')%>%
  summary()
pred1$prob=plogis(pred1$value)
pred1$age=rep


# margin effects at black at same representative ages: -----------------------------------------------------

mm=margins::margins(fit, at=list(age=rep*12))%>%
  summary()%>%
  filter(factor=="black")



#average margns at representative ages: -----------------------------------------------------
avg=margins::margins(fit, change='iqr')%>%
  summary()%>%
  filter(factor=="black")




