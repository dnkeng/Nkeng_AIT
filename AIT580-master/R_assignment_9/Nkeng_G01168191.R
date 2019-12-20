###------------------
###Hypothesis Testing
###------------------

###Students Name:
###GNumber:
getwd()
setwd("/Users/MacBook Pro/Downloads/AIT580-master 2/AIT580_9")

rm(list=ls())

data <- read.csv('~/Downloads/AIT580-master 2/AIT580_9/AIT580-master/data/EmployeeAttrition.csv')


# Your hypothesis testings here...
#1. If the MonthlyIncome of Males is greater than Females 
M_data = which(data$Gender=='Male')
F_data = which(data$Gender=='Female')
t.test(data$MonthlyIncome[M_data],data$MonthlyIncome[F_data],alternative="greater", var.equal=T)
print('1. JUSTIFICATION: we reject the alternative hypothesis here as we observed a high p value of 0.8889 which is greater than 0.05 which means 
that we accept null hypothesis which is MonthlyIncome of males is not higher than females')

#2. If the WorkLifeBalance of Males is less than Females 
t.test(data$WorkLifeBalance[M_data],data$WorkLifeBalance[F_data],alternative="less", var.equal = T)
print('2. JUSTIFICATION:As we got a high p value of 0.458 which is greater than 0.05 we reject alternate hypothesis 
      and accept null hypothesis that WorklifeBalance of males isnt less that Females'  )
#3. If the YearsAtCompany of Single is less than Married 
Singles=which(data$MaritalStatus=='Single')
Married=which(data$MaritalStatus=='Married')
t.test(data$YearsAtCompany[Singles],data$YearsAtCompany[Married],alternative = "less")
print('3.JUSTIFICATION:We accept alterntive hypothesis that YearsAtCompany of single is less than married
      as we p value is 0.004973 which is less than 0.05 and we reject the null hypothesis')

#4. If the EnvironmentalSatisfaction of Attrition=Yes is less than Attrition=No
attrition_YES=which(data$Attrition=='Yes')
attrition_NO=which(data$Attrition=='No')
t.test(data$EnvironmentSatisfaction[attrition_YES],data$EnvironmentSatisfaction[attrition_NO],alternative = "less")
print('4.JUSTIFICATION:we reject null hypothesis and accept alternate hypothesis as we got a small p valueof 0.0001046 which is less that 0.05
      , so we accept that EnvironmentalSatisfaction of attrition is less than that of no attrition ')

#5. If the MonthlyIncome of Manager is greater than Laboratory Technician 
Manager=which(data$JobRole=='Manager')
Lab_tech=which(data$JobRole=='Laboratory Technician')
t.test(data$MonthlyIncome[Manager],data$MonthlyIncome[Lab_tech],
       alternative="greater")
print('5. JUSTIFICATION: The small P-value of < 2.2e-16 is a strong evidence against null hypothesis ,
      so we reject it , and accept alternative hypothesis that MonthlyIncome of manager is greater than Laboratory Technician')

#6. If YearsAtCompany and DailyRate are correlated with each other 
cor.test(data$YearsAtCompany,data$DailyRate)
print('6.JUSTIFICATION:we accept null hypothesis that there is no correlation between the two as the pearsons R-value which is 
      -0.03405477 is less than critical value , and we reject alternate hypothesis that there is correlation')
#7. If YearsAtCompany and MonthlyIncome are correlated with each other 
cor.test(data$YearsAtCompany,data$MonthlyIncome)
print('7.JUSTIFICATION:we accept null hypothesis that there is no correlation as obtained pearsons R value is 0.5142848 which is less
      than critical value and there is no correlation between the two')
#8. If YearsAtCompany varies depending on individual’s MaritalStatus 
summary(aov(YearsAtCompany ~ MaritalStatus,data=data))
print('8.JUSTIFICATION: As the P-value- 0.024 is lesser than 0.05 we reject null hypothesis and accept alternate Hypothesis 
      that  YearsAtCompany does vary depening on individual Martial Status  ')
#9. If MonthlyIncome varies depending on individual’s PerformanceRating 
summary(aov(MonthlyIncome ~ PerformanceRating,data=data))
print('9.JUSTIFICATION:As the P-value- 0.512 is greater than 0.05 we accept null hypothesis that
      MonthlyIncome doesnt vary depening on individual PerformanceRating.')
#10. If MonthlyIncome varies depending on individual’s WorkLifeBalance
summary(aov(MonthlyIncome ~ WorkLifeBalance,data=data))
print('10.JUSTIFICATION:As the P-value- 0.24 is greater than 0.05 we accept null hypothesis that
      MonthlyIncome doesnt vary depening on individual WorkLifeBalance')

