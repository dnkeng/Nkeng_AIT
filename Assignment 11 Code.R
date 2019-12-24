
###------------------
###Visualization
###------------------

###Student Name:Nkeng A. Derek
###GNumber:G01168191


rm(list=ls())

data <- read.csv('/Users/koppakapremsai/Downloads/EmployeeAttrition(1).csv')
install.packages("ggplot2") 
library(ggplot2)
#a. Create Histogram for Age using R 
#visualized image is attached in this same folder with name 'a.png'
ggplot(data, aes(Age))+geom_histogram( binwidth = 1,color="black",fill = "grey")+ggtitle("HISTOGRAM FOR AGE")
ggsave("i.png")
print("In the above visualization we use ggplot function in ggplot2 library to create a histogram. here x axis show the age and y axis is the count,mentioning geom_histogram parameter in ggplot will plot a histogram")

#b. Create Histogram for Age using Tableau
#visualized image is attached in this same folder with name 'ii.png'
print("using tableau we created a histogram with age on x axis and frequency of age on y axis, the insights found we the age groups between 32-36 has the highest frequency and age groups between 56-60 has the lowest frequency")

# c. Create Scatter Plot for Age and Monthly Income using R
#visualized image is attached in this same folder with name 'iii.png'
ggplot(data,aes(Age,MonthlyIncome))+geom_point(colour='red')+ggtitle("SCATTER PLOT FOR AGE AND MONTHLY INCOME")
ggsave("iii.png")
print("to visualize a scatter plot we used ggplot with geom_point as the parameter, here aes means aesthetics of geometry where we mentions the the attributes which should be mapped into plots, here x axis is age and y axis is monthly income")

#d. Create Scatter Plot for Age and Monthly Income using Tableau 
#visualized image is attached in this same folder with name 'iv.png'
print("using tableau a scatter plot has been created with age as x axis and monthly income on y axis")
