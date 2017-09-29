library(highcharter)
library(dplyr)
library(tidyr)
library(purrr)
library(hts)


X=aggts(infantgts,3)%>%
  as_data_frame()%>%
  mutate(Year=1933:2003)%>%
  filter(Year>=2000)%>%
  gather(v,val,-Year)%>%
  separate(v,c("state","sex"),sep=" ")

Total=X%>%
  group_by(Year)%>%
  summarize(val=sum(val))%>%
  mutate(drilldown=Year)
  
State=X%>%
  group_by(Year,state)%>%
  summarize(val=sum(val))%>%
  nest(-Year)%>%
  mutate(data=map(data,list_parse))

Sex=X%>%

as.data.frame(infantgts)
data=data_frame(
  
)

h1list=lapply()

h0=hchart(Total,"column",hcaes(y=val,x=Year,name="Year"))
h0
h0%>%
  hc_drilldown(
    allowPointDrilldown=TRUE,
    series=
  )
