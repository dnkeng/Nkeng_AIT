rm(list=ls())
library(ggplot2)
library(tidyverse)
library(corrplot)
pdata <- read.csv(file.choose())
# cleaning Data

cleandata <- pdata[complete.cases(pdata),]
pdata1<-na.omit(cleandata)
summary(pdata1)
View(pdata1)

##scatter plot for states and their frequency

filter_states <- pdata1$StateAbbr
States <- filter_states
plt <- as.data.frame(table(States))
ggplot(plt,aes(x=States,y=Freq))+
  geom_point(col='Blue')+
  labs(x="States",y="Frequency",title="States vs Frequency")

#scatter plot for state and data value

pdata1_state<- pdata1$StateAbbr
pdata1_data<-pdata1$Data_Value
summary(pdata1_data)
sctplot <- data.frame(pdata1_state,pdata1_data)
ggplot(sctplot,aes(x=pdata1_state,y=pdata1_data))+
  geom_point(col='Blue')+
  labs(x="states",y="Data value",title="States vs data value")

#scatter plot for cities vs population 

pdata1_population<- pdata1$PopulationCount
sctplot <- as.data.frame (pdata1$PopulationCount)
ggplot(sctplot,aes(x=pdata1$CityName,y=pdata1_population))+
  geom_point(col='Blue')+
  labs(x="cities",y="Population Count",title="States vs Population Count")




#co-relation crudelow vs crudehigh
crudevalue<-subset(pdata$Data_Value,pdata$Data_Value_Type=="Crude prevalence")
population_count<- subset(pdata$PopulationCount,pdata$Data_Value_Type=="Crude prevalence")
crude<- data.frame(crudevalue,population_count)
cor.test(crudevalue,population_count)
crude <- data.frame(crudelow,crudehigh)
m<- cor(crude)
corrplot(m, method = "circle")

#linear regression for crudelow vs crudehigh
crude1 <- data.frame(crudelow,crudehigh)
crudelow <- subset(pdata1$Low_Confidence_Limit,pdata1$Data_Value_Type=="Crude prevalence")
crudehigh <- subset(pdata1$High_Confidence_Limit,pdata1$Data_Value_Type=="Crude prevalence")

cruderegg <- lm(crudelow~crudehigh,data=pdata)
summary(cruderegg)
ggplot(crude1, aes(x = crudelow, y = crudehigh)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "Red")+
  labs(x="CrudePrev LowConfLimit",y="CrudePrev HighConfLimit",title="CrudePrev LowConf Vs HighConf (Regression Plot)")

