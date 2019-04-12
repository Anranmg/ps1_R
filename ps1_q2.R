#q2
# Libraries: --------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)

# Obtain or restore data: -------------------
nyc_dt_13=nycflights13::flights
#nyc_dt_14 = data.table::fread( 'https://raw.githubusercontent.com/wiki/arunsrinivasan/flights/NYCflights14/flights14.csv' )
file = '/Users/meng/Downloads/ps1/nyc_dt_14.RData'
if (!file.exists(file)) {
  nyc_dt_14 = readr::read_delim(
    "https://raw.githubusercontent.com/wiki/arunsrinivasan/flights/NYCflights14/flights14.csv",
    delim = ',' )
  save(nyc_dt_14, file = file)
} else {
  load(file) #nyc_dt_14
}


#a
#find airlines have at least 1% of the flights: ----------------
al_1pct_13=
  nyc_dt_13%>%
  filter(month<=10)%>%
  group_by(carrier)%>%
  summarize(num13=n())%>%
  mutate(pct13=num13/sum(num13)*100)%>%
  left_join(nycflights13::airlines,by="carrier")%>%
  filter(pct13>=1)%>%
  arrange(desc(pct13))



#b
#compute the number/ proption of flights among selected airlines : ----------------
al_1pct_13_14=
  bind_rows(nyc_dt_13,nyc_dt_14)%>%
  filter(month<=10)%>%
  right_join(al_1pct_13,by="carrier")%>%
  group_by(name,year)%>%
  summarize(num=n())%>%
  group_by(year)%>%
  mutate(total=sum(num),prop=num/sum(num))

#compute std errors and 95% CI: ----------------
al_1pct_13_14=
  al_1pct_13_14%>%
  mutate(std_err=sqrt(prop*(1-prop)/total),
         lwr = prop - qnorm(.975)*std_err,
         upr = prop + qnorm(.975)*std_err
  )

#convert long to wide
al_1pct_13_14=
  al_1pct_13_14%>%   
  group_by(name)%>%  
  transmute(num13=num[1],prop13=prop[1]*100,std_err13=std_err[1],
            lwr13=lwr[1]*100,upr13=upr[1]*100,
            num14=num[2],prop14=prop[2]*100,std_err14=std_err[2],
            lwr14=lwr[2]*100,upr14=upr[2]*100)

#covert na to 0: ----------------
al_1pct_13_14[is.na(al_1pct_13_14)]=0 

#compute std errors between 2 props and 95% CI: ----------------
al_1pct_13_14=
  al_1pct_13_14%>% 
  mutate(change=prop14-prop13, 
         std_err=sqrt((std_err13^2+std_err14^2)),
         lwr_change=change-qnorm(.975)*std_err,
         upr_change=change+qnorm(.975)*std_err)%>% 
  select(name,num13,prop13,lwr13,upr13,
         num14,prop14,lwr14,upr14,
         change,lwr_change,upr_change)
#delete repeatable values ----------------
al_1pct_13_14=unique(al_1pct_13_14)
#produce a table ----------------
knitr::kable(al_1pct_13_14, digits=2, 
             caption='Number and Percent of Annual Flights (%) in First 10 Months Each Year.')  

#largest increase dl  largest decrease ev
#as total number of flights decline  



#q3
#percent of flights each airline is responsible for----------------
al_pct=
  bind_rows(nyc_dt_13,nyc_dt_14)%>%
  group_by(origin)%>%
  mutate(total=n())%>%
  group_by(origin,carrier)%>%
  summarize(num=n(),total=total[1],pct=num/total*100)%>%
  left_join(nycflights13::airlines,by="carrier")%>%
  arrange(origin,desc(pct))
#produce a table ----------------
knitr::kable(select(al_pct,origin,name,pct), digits=2, caption='Proprtion of Flights (%) by Airlines.')

##percent of flights only airline in part a----------------
al_pct_pa=
  al_pct%>%
  right_join(al_1pct_13,by=c("carrier","name"))%>%
  mutate(std_err=sqrt(pct*(100-pct)/total[1]),
         lwr=pct-qnorm(0.975)*std_err,
         upr=pct+qnorm(0.975)*std_err)%>%
  select(origin,name,pct,lwr,upr)%>%
  arrange(origin,desc(pct))

#produce a table ----------------
knitr::kable(al_pct_pa, digits=2, 
             caption='Proprtion of Flights by Airlines in Part A.')

#compute largest carrier at each---------------
al_pct_max=
  al_pct%>%
  group_by(origin)%>%
  summarize(name=name[1],pct=pct[1])








