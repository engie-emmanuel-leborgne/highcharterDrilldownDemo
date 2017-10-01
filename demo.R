library(highcharter)
library(dplyr)
library(tidyr)
library(purrr)
library(hts)


X=aggts(infantgts,3)%>%
  as_data_frame()%>%
  mutate(Year=1933:2003)%>%
  filter(Year>=1990)%>%
  gather(v,val,-Year)%>%
  separate(v,c("state","sex"),sep=" ")

Total=X%>%
  rename(name=Year)%>%
  group_by(name)%>%
  summarize(y=sum(val))%>%
  mutate(drilldown=name)%>%
  arrange(desc(name))
  
State=X%>%
  group_by(Year,state)%>%
  summarize(y=sum(val))%>%
  nest(-Year)%>%
  mutate(data=map2(Year,data,function(id,d){
    data=d%>%
      mutate(drilldown=paste(id,state,sep="_"))%>%
      select(name=state,y,drilldown)%>%
      arrange(desc(y))%>%
      list_parse()
    list(name="State",id=id,data=data,type="bar")
  }
    )
  )

Sex=X%>%
  mutate(id=paste(Year,state,sep="_"))%>%
  nest(-id)%>%
  mutate(data=map2(id,data,function(.id,d){
    data=d%>%
      select(name=sex,y=val)%>%
      arrange(desc(y))%>%
      list_parse()
    list(name="Sex",id=.id,data=data,type="bar")
  }
  ))



h0=highchart() %>%
  hc_title(text = "Multi-level Drilldown") %>%
  hc_xAxis(type = "category") %>%
  hc_legend(enabled = FALSE) %>%
  hc_plotOptions(
    series = list(
      boderWidth = 0,
      dataLabels = list(enabled = TRUE)
    )
  )%>%
  hc_add_series(
    data = Total,
    name = "Year",
    type = "bar"
    )

h0=h0%>%
  hc_drilldown(
    allowPointDrilldown=TRUE,
    series=c(State$data,Sex$data)
  )

h0
