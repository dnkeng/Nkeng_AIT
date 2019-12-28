rm(list=ls())
library(igraph)
install.packages("igraph")
library(tidyverse)
library(corrplot)
pdata <- read.csv(file.choose())
# cleaning Data

cleandata <- pdata[complete.cases(pdata),]
pdata1<-na.omit(cleandata)
summary(pdata1)
View(vdata1)

##scatter plot for cities and their frequency
summary(filter_cities)
filter_cities <- pdata1$CityName
plt <- as.data.frame(table(filter_cities))
summary(plt)
max.col(plt$filter_cities)
ggplot(plt,aes(x=filter_cities,y=Freq))+geom_point(col='Red')+labs(x="cities",y="Frequency",title="Cities VS Frequency")
print(plt)


#scatter plot for state and data value

pdata1_state<- pdata1$StateAbbr
vdata1_data<-pdata1$Data_Value
summary(vdata1_data)
sctplot <- data.frame(pdata1_state,pdata1_data)
ggplot(sctplot,aes(x=pdata1_state,y=pdata1_data))+
  geom_point(col='Blue')+
  labs(x="states",y="Data value",title="States vs data value")
#scatter plot for cities vs population 
pdata1_city<- pdata1$CityName
pdata1_population<- pdata1$PopulationCount
sctplot <- as.data.frame (pdata1$PopulationCount)
ggplot(sctplot,aes(x=pdata1_city,y=pdata1_population))+
  geom_point(col='Red')+
  labs(x="states",y="Population Count",title="States vs Population Count")

pdata1_state<- pdata1$StateAbbr
pdata1_population<- pdata1$PopulationCount
sctplot <- as.data.frame (pdata1$PopulationCount)
ggplot(sctplot,aes(x=pdata1_state,y=pdata1_population))+
  geom_point(col='Blue')+
  labs(x="states",y="Population Count",title="States vs Population Count")

#scatter plot for category and their frequency
filter_category <- vdata1$Category
category <- filter_category
plt <- as.data.frame(table(category))
ggplot(plt,aes(x=category,y=Freq))+
  geom_point(col='Blue')+
  labs(x="Category",y="Frequency",title="category vs Frequency")

#box plot for agelow vs agehigh

agelow <- subset(vdata$Low_Confidence_Limit,vdata$Data_Value_Type=="Age-adjusted prevalence")
agehigh <- subset(vdata$High_Confidence_Limit,vdata$Data_Value_Type=="Age-adjusted prevalence")
boxplot(agelow,agehigh, col = c("Blue","Orange"),names =c("AgeLow","AgeHigh"))

#box plot for crudelow vs crudehigh
boxplot(pdata1$PopulationCount)
crudelow <- subset(pdata1$Low_Confidence_Limit,pdata1$Data_Value_Type=="Crude prevalence")
crudehigh <- subset(pdata1$High_Confidence_Limit,data1$Data_Value_Type=="Crude prevalence")
boxplot(crudelow,crudehigh, col = c("Light Blue","Yellow"),names =c("CrudeLow","CrudeHigh"))

#co-relation crudelow vs crudehigh
population_count <- subset(pdata1$PopulationCount,pdata1$Data_Value_Type=="Crude prevalence")
crudevalue<- subset(pdata1$Data_Value,pdata1$Data_Value_Type=="Crude prevalence")
crude1<- data.frame(population_count,crudevalue)
cor.test(population_count,crudevalue)

crude <- data.frame(crudelow,crudehigh)
cor.test(crudelow,crudehigh)
m <- cor(crude1)
corrplot(m, method = "circle")

#linear regression for crudelow vs crudehigh

cruderegg <- lm(crudelow~crudehigh,data=vdata)
summary(cruderegg)
ggplot(crude, aes(x = crudelow, y = crudehigh)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "Yellow")+
  labs(x="CrudePrev LowConfLimit",y="CrudePrev HighConfLimit",title="CrudePrev LowConf Vs HighConf (Regression Plot)")
