#q3
# Libraries: --------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
#Question 3
# Obtain or restore data: -----------------------------------------------------
file = '/Users/meng/Downloads/ps1/recs2015_public.RData'
if (!file.exists(file)) {
  recs_tib = readr::read_delim(
    "https://www.eia.gov/consumption/residential/data/2015/csv/recs2015_public_v3.csv",
    delim = ',' )
  save(recs_tib, file = file)
} else {
  load(file) #recs_tib
}

#location types (urban/rural):-----------------------------------------------------
decode_loca_type=function(x){
  if(!is.character(x)) stop('decode_loca_type expects character input!')
  switch(x,U="urban",C="urban",R="rural")
}

decode_all_loca_type=function(x){
  sapply(x,decode_loca_type)
}

#key values used in question b-----------------------------------------------------
qb=
  recs_tib%>%
  mutate(TYPE=decode_all_loca_type(UATYP10))%>%
  select(DIVISION,TYPE,WALLTYPE,NWEIGHT,KWH,INTERNET,BRRWT1:BRRWT92)%>%
  replace_na(list(NWEIGHT=0))

#convert weights to long-----------------------------------------------------
qb_rep=
  qb%>%
  gather(key="repl",value="w",BRRWT1:BRRWT92)


#percent of homes have stucco construction as the major outside wall material
#within each division? 
#a
#point estimate of percent-----------------------------------------------------
pct_stucco=
  qb%>%
  group_by(DIVISION,WALLTYPE)%>%
  summarise(num=sum(NWEIGHT))%>%
  group_by(DIVISION)%>%
  mutate(pct=num/sum(num)*100)%>%
  filter(WALLTYPE==4)%>%
  select(DIVISION,pct)

#replicate weighted percent----------------------------------------------------- 
pct_stucco_repl=
  qb_rep%>%
  group_by(DIVISION,WALLTYPE,repl)%>%
  summarise(num_r=sum(w))%>%
  group_by(DIVISION,repl)%>%
  mutate(pct_r=num_r/sum(num_r)*100)%>%
  filter(WALLTYPE==4)%>%
  ungroup()%>%
  select(DIVISION,pct_r)

#compute standard errors-----------------------------------------------------
pct_stucco=
  pct_stucco%>%
  left_join(pct_stucco_repl,by="DIVISION")%>%
  group_by(DIVISION)%>%
  summarize(pct=pct[1],std_err=sqrt(mean({pct_r-pct}^2)*2))%>%
  mutate(lwr=pct-qnorm(.975)*std_err,
         upr=pct+qnorm(.975)*std_err)

#produce a table-----------------------------------------------------
knitr::kable(pct_stucco, digits=2, 
             caption='Percent of Homes in Stucco Construction within Each Division.')

#produce a scatter plot-----------------------------------------------------
ggplot()+
  geom_point(data=pct_stucco,aes(x=factor(DIVISION),y=pct))+
  #geom_errorbar(data=pct_stucco,
  #             aes(x=factor(DIVISION),pct,ymin=lwr,ymax=upr),
  #            color="red",width=0.2)+
  labs(x="Division",y="Percent",
       title = "Scatter Plot of Stucco Percent within Each Division")
#Which division has the highest proportion? Which the lowest?
#division 9, divison 3     


#What is average total electricity usage in kilowatt hours in each division?     
#b1
#point estimate of average-----------------------------------------------------
avg_e=
  qb%>%
  group_by(DIVISION)%>%
  summarise(avg=sum(KWH*NWEIGHT)/sum(NWEIGHT))


#replicate weighted average----------------------------------------------------- 
avg_e_repl=
  qb_rep%>%
  group_by(DIVISION,repl)%>%
  summarise(avg_r=sum(KWH*w)/sum(w))


#compute standard errors-----------------------------------------------------  
avg_e=
  avg_e%>%
  left_join(avg_e_repl,by="DIVISION")%>%
  group_by(DIVISION)%>%
  summarize(avg=avg[1],std_err=sqrt(mean({avg_r-avg}^2)*2))%>%
  mutate(lwr=avg-qnorm(.975)*std_err,
         upr=avg+qnorm(.975)*std_err)

#produce a table-----------------------------------------------------
knitr::kable(avg_e, digits=2, 
             caption='Average Total Electricity Usage within Each Division.')



#Answer the same question stratified by urban and rural status.
#b2
#point estimate of average-----------------------------------------------------
avg_e_type=
  qb%>%
  group_by(DIVISION,TYPE)%>%
  summarise(avg_elec=sum(KWH*NWEIGHT)/sum(NWEIGHT))
#replicate weighted average----------------------------------------------------- 
avg_e_type_r=
  qb_rep%>%
  group_by(DIVISION,TYPE,repl)%>%
  summarise(avg_r=sum(KWH*w)/sum(w))
#compute standard errors-----------------------------------------------------  
avg_e_type=
  avg_e_type%>%
  left_join(avg_e_type_r,by=c("DIVISION","TYPE"))%>%
  group_by(DIVISION,TYPE)%>%
  summarise(avg_elec=avg_elec[1],
            std_err=sqrt(mean({avg_r-avg_elec}^2))*2)%>%
  mutate(lwr=avg_elec-qnorm(.975)*std_err,
         upr=avg_elec+qnorm(.975)*std_err)

#produce a table-----------------------------------------------------
knitr::kable(avg_e_type, digits=2, 
             caption='Average Total Electricity Usage (Urban/Rural) within Each Division.')

#produce a plot-----------------------------------------------------
ggplot(data=avg_e_type, aes(x=factor(DIVISION), y=avg_elec, fill=TYPE)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
  theme_minimal() + scale_fill_manual(values=c('#999999','#E69F00'))+
  labs(x="Division",y="Average",
       title = "Average Total Electricity Usage within Each Division")


#qc
#Which division has the largest disparity between urban and rural areas 
#in terms of the proportion of homes with internet access?


#point estimate of percent-----------------------------------------------------
prop_inte=
  qb%>%
  group_by(DIVISION,TYPE)%>%
  summarise(prop=sum(NWEIGHT*INTERNET)/sum(NWEIGHT)*100)
prop_inte_diff=
  inner_join(filter(prop_inte,TYPE=="urban"),
             filter(prop_inte,TYPE=="rural"),by="DIVISION")%>%
  ungroup()%>%
  transmute(DIVISION, prop_diff=prop.x-prop.y)

#replicate weighted percent----------------------------------------------------- 
prop_inte_repl=
  qb_rep%>%
  group_by(DIVISION,TYPE,repl)%>%
  summarise(prop_r=sum(w*INTERNET)/sum(w)*100)
prop_inte_diff_repl=
  inner_join(filter(prop_inte_repl,TYPE=="urban"),
             filter(prop_inte_repl,TYPE=="rural"),by="DIVISION")%>%
  ungroup()%>%
  transmute(DIVISION, prop_diff_r=prop_r.x-prop_r.y)


#compute standard errors-----------------------------------------------------
prop_inte_diff=
  prop_inte_diff%>%
  left_join(prop_inte_diff_repl,by="DIVISION")%>%
  group_by(DIVISION)%>%
  summarize(prop_diff=prop_diff[1],
            std_err=sqrt(mean({prop_diff_r-prop_diff}^2))*2)%>%
  mutate(lwr=prop_diff-qnorm(.975)*std_err,
         upr=prop_diff+qnorm(.975)*std_err)

#produce a table-----------------------------------------------------
knitr::kable(prop_inte_diff, digits=2, 
             caption='Proportion of Homes with Internet Access within Each Division.')
