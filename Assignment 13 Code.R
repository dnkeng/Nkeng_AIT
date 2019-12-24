###------------------
###Regression and Clustering
###------------------

###Students Name:Nkeng Derek
###GNumber:G01168191


rm(list=ls())

install.packages(ggplot)

library(ggplot2)

data <- read.csv('~/Downloads/AIT580-master 2/Assignment 13/AIT580-master/data/EmployeeAttrition.csv')



# Your answers here...
#a. Show the scatter plot with relationship curve between TotalWorkingYears and MonthlyIncome. Briefly explain your observation in the plot (Hint: Use scatter.smooth() function in R) 

scatter.smooth(data$TotalWorkingYears,data$MonthlyIncome,main="WORKINGYEARS vs MONTHLYINCOME", degree =1, xlab="TOTAL WORKING YEARS", ylab = "MONTHLY INCOME",  family= c("symmetric", "gaussian"),lpars = list(col= "red",lwd = 4, lty = 5))

#b.	Show the scatter plot with relationship curve between Age and DistanceFromHome. Briefly explain your observation in the plot.

scatter.smooth(data$Age,data$DistanceFromHome, main="AGE vs DISTANCEfromHOME", span= 3/2, degree =1, xlab="Age", ylab = "Distance from Home",  family= c("symmetric", "gaussian"),lpars = list(col= "red",lwd = 4, lty = 10))  

#c.Calculate Correlation for (a) and (b) and explain the values to support your answer in (a) and (b)

#d. d. Using Linear Regression, find details of the relationship between TotalWorkingYears and
#MonthlyIncome. Explain results in terms of p-value at 95% confidence interval and
#determine whether the relationship is significant or not (Hint: Use lm() to create linear 
#regression model. Use print() to show coefficients. Use summary() to show more details) 

data_lire<-lm(data$TotalWorkingYears ~ data$MonthlyIncome, data = data)

print(data_lire)

summary(data_lire)
# As the p valiue is very small <0.05 , we can reject the null hypothesis.

#2. Clustering
#a. Use K-means Clustering algorithm to find groups between HourlyRate and
#TotalWorkingYears. Use number of clusters as 3. Explain how each group is different
#from another in terms of employees representing those groups. 
data_1<-data.frame(data$HourlyRate,data$TotalWorkingYears)
data_1
datakmean3<-kmeans(data_1,3)
datakmean3
datakmean5<-kmeans(data_1,5)
datakmean5
#creating data frame for the kmean when k=3 and k=5
kmean_results <- data.frame(data$HourlyRate,data$TotalWorkingYears,datakmean3$cluster,datakmean5$cluster)
kmean_results 
plotkmean3<- ggplot(kmean_results, aes(kmean_results$data.HourlyRate,kmean_results$data.TotalWorkingYears,color=kmean_results$datakmean3.cluster))+geom_point()
plotkmean3

#b. Use number of clusters as 5. What did you observe? Did you see any split of groups
#observed in (a)? Observe the splitting groups and explain in terms of employees
#representing those groups. (Hint: Use kmeans() for clustering algorithm. Install
#ggplot2 library in R and use ggplot() function to visualize the clustering results)

plotkmean5<- ggplot(kmean_results, aes(kmean_results$data.HourlyRate,kmean_results$data.TotalWorkingYears,color=kmean_results$datakmean5.cluster))+geom_point()
plotkmean5


