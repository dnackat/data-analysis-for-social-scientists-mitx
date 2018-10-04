rm(list=ls())
library("utils")
library("tidyverse")
library(stringr)

# Question 1
gender_data<-as_tibble(read.csv("Gender_StatsData.csv"))
# Question 2 Did by inspection
head(gender_data)
# Question 3
teenager_fr<-filter(gender_data,Indicator.Code=="SP.ADO.TFRT")
# Question 4
round(mean(teenager_fr$X1975, na.rm = TRUE),2)
round(sd(teenager_fr$X1975, na.rm = TRUE),2)
# Question 6
round(mean(teenager_fr$X1960, na.rm = TRUE),2)
round(sd(teenager_fr$X1960, na.rm = TRUE),2)
# Question 7
round(mean(teenager_fr$X2000, na.rm = TRUE),2)
round(sd(teenager_fr$X2000, na.rm = TRUE),2)
# Question 9
byincomelevel<- filter(teenager_fr,Country.Code%in%c("LIC","MIC","HIC"))
# Question 10
plotdata_bygroupyear<-gather(byincomelevel,Year,FertilityRate,X1960:X2015) %>%
  select(Year, Country.Name,Country.Code, FertilityRate)
plotdata_byyear<-plotdata_bygroupyear
# Question 11
plotdata_byyear<-select(plotdata_byyear,Country.Code,Year,FertilityRate)%>%
  spread(Country.Code,FertilityRate)
# Question 12 TRUE answer (incorrect)
# Going back to Q.10 output
plotdata_byyear<-gather(byincomelevel,Year,FertilityRate,X1960:X2015) %>%
  select(Year, Country.Name,Country.Code, FertilityRate)
plotdata_byyear<-spread(plotdata_byyear,Country.Code,FertilityRate)
# Question 12 FALSE answer (correct)
plotdata_byyear<-gather(byincomelevel,Year,FertilityRate,X1960:X2015) %>%
  select(Year, Country.Name,Country.Code, FertilityRate)
plotdata_byyear<-select(plotdata_byyear,Country.Code,Year,FertilityRate)%>%
  spread(Country.Code,FertilityRate)
# Question 13
ggplot (plotdata_bygroupyear, aes(x=Year,y=FertilityRate, group=Country.Code,colour=Country.Code)) +
  geom_line(  )+
  labs(title='Fertility Rate by Country-Income-Level over Time')
plotdata_bygroupyear<-mutate(plotdata_bygroupyear,Year=as.numeric(str_sub(Year,-4)))
# Further questions are by inspection 