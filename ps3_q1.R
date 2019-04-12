#q1
# Libraries: --------------------------------
library(data.table)
library(dplyr)
library(ggplot2)
#Question 1
# Obtain or restore data: -----------------------------------------------------
#setwd("./Downloads/ps")
file = './recs2015_public.RData'
if (!file.exists(file)) {
  recs_tib = fread(
    "https://www.eia.gov/consumption/residential/data/2015/csv/recs2015_public_v3.csv")
  save(recs_tib, file = file)
} else {
  load(file) #recs_tib
}

#decode division: -----------------------------------------------------
deco_div=function(x){
  switch(x,"New England", "Middle Atlantic", "East North Central",
         "West North Central","South Atlantic","East South Central",
         "West South Central","Mountain North","Mountain South","Pacific")
}

deco_all_div=function(x){
  sapply(x,deco_div)
}


#decode type (urban/rural): -----------------------------------------------------
decode_type=function(x){
  if(!is.character(x)) stop('decode_loca_type expects character input!')
  switch(x,U="urban",C="urban",R="rural")
}

deco_all_typ=function(x){
  sapply(x,decode_type)
}

recs_tib[,':='(DIVISION=deco_all_div(DIVISION),TYPE=deco_all_typ(UATYP10))]

#convert weights to long-----------------------------------------------------
recs_rep=
  melt(recs_tib,measure.vars=patterns("^BRRWT"),variable.name = "repl",value.name="w")

#a:-----------------------------------------------------
#percent of homes have stucco construction as the major outside wall material
#within each division? 

#point estimation----------------------------------------------------- 

#pct_stucco=recs_tib[,.(n=sum(NWEIGHT)),by=.(DIVISION,WALLTYPE)]%>%
#  .[,.(WALLTYPE,pct=n/sum(n)*100),by=.(DIVISION)]%>%
#  .[WALLTYPE==4,.(DIVISION,pct)]

pct_stucco=recs_tib[,.(pct=100*sum(NWEIGHT*{WALLTYPE==4})/sum(NWEIGHT)),by=.(DIVISION)]

#replicate weighted percent----------------------------------------------------- 
#pct_stucco_r=recs_rep[,.(n=sum(w)),by=.(repl,DIVISION,WALLTYPE)]%>%
#  .[,.(WALLTYPE,pct_r=n/sum(n)*100),by=.(repl,DIVISION)]%>%
#  .[WALLTYPE==4,.(repl,DIVISION,pct_r)]

pct_stucco_r=recs_rep[,.(pct_r=100*sum(w*{WALLTYPE==4})/sum(w)),by=.(repl,DIVISION)]

#standard errors----------------------------------------------------- 
pct_stucco=pct_stucco[pct_stucco_r,,on='DIVISION']%>%
  .[,.(pct=mean(pct),std=2*sqrt(mean({pct_r-pct}^2))),by='DIVISION']%>%
  .[,':='(upr=pct+std*qnorm(.975),lwr=pmax(pct-std*qnorm(.975),0))]%>%
  .[order(-pct)]


# table and graph-----------------------------------------------------   
# pct_stucco_tab=
#   pct_stucco[,.(`Census Division`=DIVISION, 
#                 `% Stucco (95% CI)`=sprintf('%4.1f%%(%4.1f,%4.1f)',pct,lwr,upr))]
# 
# knitr::kable(pct_stucco_tab,
#              caption= "Estimated percent of homes within each census division with major wall type of stucco")
# pct_stucco%>%
#   ggplot(aes(x=DIVISION,y=pct))+
#            geom_col(fill='navy')+
#            geom_errorbar(aes(ymin=lwr,ymax=upr),col='darkslategrey')+
#            theme_bw()+
#            ylab("% Stucco")+
#            xlab("Census Division")+
#   theme(axis.text.x = element_text(size=8,angle=90))

### lowest division: East South Central; highest division: Mountain South



#b:-----------------------------------------------------
#average total electricity usage in kilowatt hours in each division? 
#  Answer the same question stratified by urban and rural status.

#point estimation----------------------------------------------------- 
ele_avg=recs_tib[,.(kwh=sum(KWH*NWEIGHT)/sum(NWEIGHT)),by=.(DIVISION)]

#replicate weighted average----------------------------------------------------- 
ele_avg_r=recs_rep[,.(kwh_r=sum(KWH*w)/sum(w)),by=.(repl,DIVISION)]

#standard errors----------------------------------------------------- 
ele_avg=ele_avg[ele_avg_r,on='DIVISION']%>%
  .[,.(kwh=mean(kwh),std=2*sqrt(mean({kwh_r-kwh}^2))),keyby=DIVISION]%>%
  .[,':='(lwr=pmax(kwh-std*qnorm(.975),0),upr=kwh+std*qnorm(.975))]


#point estimation(urban/rural) ----------------------------------------------------- 
ele_avg_u=recs_tib[,.(kwh=sum(KWH*NWEIGHT)/sum(NWEIGHT)),by=.(DIVISION,TYPE)]

#replicate weighted average----------------------------------------------------- 
ele_avg_u_r=recs_rep[,.(kwh_r=sum(KWH*w)/sum(w)),by=.(repl,DIVISION,TYPE)]

#standard errors----------------------------------------------------- 
ele_avg_u=ele_avg_u[ele_avg_u_r,on=c('DIVISION','TYPE')]%>%
  .[,.(kwh=mean(kwh),std=2*sqrt(mean({kwh_r-kwh}^2))),keyby=.(DIVISION,TYPE)]%>%
  .[,.(DIVISION,TYPE,kwh,lwr=pmax(kwh-std*qnorm(.975),0),upr=kwh+std*qnorm(.975))]



# table and graph-----------------------------------------------------  
# pwc=function(x){
#   format(x,big.mark = ',',digits = 1)
# }
# 
# ele_avg_tab=
#   ele_avg[,.(`Census Division`=DIVISION, 
#                 `Electricity (95% CI)`=sprintf('%s (%s,%s)',pwc(kwh),pwc(lwr),pwc(upr)))]
# 
# ele_avg_u_tab=dcast(ele_avg_u,DIVISION ~ TYPE, value.var=c("kwh","lwr","upr"))%>%
#   .[,.('Census Division'=DIVISION,
#     `Electricity Urban(95% CI)`=sprintf('%s (%s,%s)',pwc(kwh_urban),pwc(lwr_urban),pwc(upr_urban)),
#     `Electricity Rural(95% CI)`=sprintf('%s (%s,%s)',pwc(kwh_rural),pwc(lwr_rural),pwc(upr_rural)))]
# 
# knitr::kable(ele_avg_tab,align='r',
#              caption= "Estimated average total electricity usage in kilowatt hours in each division")
# 
# knitr::kable(ele_avg_u_tab,align='r',
#              caption= "Estimated average total electricity usage in kilowatt hours for urban and rural areas in each division")
# 
# ele_avg%>%
#   ggplot(aes(y=DIVISION,x=kwh))+
#   geom_point()+
#   geom_errorbarh(aes(xmin=lwr,xmax=upr))+
#   theme_bw()+
#   xlab("Avg Kwh")+
#   ylab("Census Division")
# 
# ele_avg_u%>%
#   ggplot(aes(y=DIVISION,x=kwh,color=TYPE))+
#   geom_point()+
#   geom_errorbarh(aes(xmin=lwr,xmax=upr))+
#   theme_bw()+
#   xlab("Avg Kwh")+
#   ylab("Census Division")


#c:-----------------------------------------------------
#division has the largest disparity between urban and rural areas 
#in terms of the proportion of homes with internet access?
#point estimation(urban/rural) ----------------------------------------------------- 
int_u=recs_tib[,.(int=100*sum({INTERNET==1}*NWEIGHT)/sum(NWEIGHT)),by=.(DIVISION,TYPE)]
int_u=dcast(int_u,DIVISION ~ TYPE, value.var = "int")
int_u=int_u[,.(disp=urban-rural),by=DIVISION]

#replicate weighted average----------------------------------------------------- 
int_u_r=recs_rep[,.(int_r=100*sum({INTERNET==1}*w)/sum(w)),by=.(repl,DIVISION,TYPE)]
int_u_r=dcast(int_u_r,repl+DIVISION ~ TYPE, value.var = "int_r")
int_u_r=int_u_r[,.(disp_r=urban-rural),by=DIVISION]

#standard errors----------------------------------------------------- 
int_u=int_u[int_u_r,on=c('DIVISION')]%>%
  .[,.(disp=mean(disp),std=2*sqrt(mean({disp_r-disp}^2))),keyby=DIVISION]%>%
  .[,.(DIVISION,disp,lwr=disp-std*qnorm(.975),upr=disp+std*qnorm(.975))]%>%
  .[order(-disp)]


# table and graph-----------------------------------------------------  
# int_u_tab=int_u[,.(`Census Division`=DIVISION,`Internet Access Disparity % (95% CI)`
#                    =sprintf('%4.1f %% (%4.1f,%4.1f)',disp,lwr,upr))]
# 
# knitr::kable(int_u_tab,caption="Urban and rural disparity in internet access")

#int_u%>%
#  ggplot(aes(y=disp,x=DIVISION))+
  # geom_col(fill='navy')+
  # geom_errorbar(aes(ymin=lwr,ymax=upr),col='darkslategrey')+
  # ylab("Internet Disparity")+
  # xlab("Census Division")+
  # theme_bw()+
  # theme(axis.text.x = element_text(size=8,angle=90))

  
#d question:-----------------------------------------------------
#average total fuel oil/kerosene cost in dollars in each division? 

#point estimation----------------------------------------------------- 
dol_avg=recs_tib[,.(dol=sum(DOLLARFO*NWEIGHT)/sum(NWEIGHT)),by=.(DIVISION)]

#replicate weighted average----------------------------------------------------- 
dol_avg_r=recs_rep[,.(dol_r=sum(DOLLARFO*w)/sum(w)),by=.(repl,DIVISION)]

#standard errors----------------------------------------------------- 
dol_avg=dol_avg[dol_avg_r,on='DIVISION']%>%
  .[,.(dol=mean(dol),std=2*sqrt(mean({dol_r-dol}^2))),keyby=DIVISION]%>%
  .[,':='(lwr=dol-std*qnorm(.975),upr=dol+std*qnorm(.975))]

# table and graph-----------------------------------------------------  
#dol_avg_tab=dol_avg[,.(`Census Division`=DIVISION,`Average cost of fuel oil/kerosene (95% CI)`
#                   =sprintf('%4.1f  (%4.1f,%4.1f)',dol,lwr,upr))]

#knitr::kable(dol_avg_tab,caption="Average cost of fuel oil/kerosene within each division")

#dol_avg%>%
#  ggplot(aes(y=dol,x=DIVISION))+
#  geom_col(fill='navy')+
#  geom_errorbar(aes(ymin=lwr,ymax=upr),col='darkslategrey')+
#  ylab("Average fuel oil/kerosene cost")+
#  xlab("Census Division")+
#  theme_bw()+
#  theme(axis.text.x = element_text(size=8,angle=90))
