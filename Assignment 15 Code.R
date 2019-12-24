install.packages("selectr")
library("selectr")
install.packages("xml2")
library("xml2")
install.packages("rvest")
library("rvest")

url <- "https://nytimes.com"
webpage <- read_html(url)
print(url)
print(webpage)
# Just sample code. 
titles <- html_nodes(webpage, "div h2")
without_tags <- gsub("<.*?>", "", titles) 
print(without_tags)

# scrape data

# Assignment 15 Task 1: write scripts that extract "titles" and "news summary" of articles out of the scrapped data. 
titles<-webpage %>% html_nodes("div h2") %>% html_text()
title<-head(titles,15)
print(title)
newssum<-webpage %>% html_nodes("p") %>% html_text()
print(newssum)
# Then, print them out using "print()" statement. 
print(title)
print(newssum)

# Assignment 15 Task 2: write scripts that oraganize your data as dataframe with column names, "title" and "news summary", respectively.
# Then, save this dataframe as a CSV file. Name it as "NYT_titles.csv". 
x <- data.frame(Titless = title, newssummary = newssum)
write.table(x, file = "nytimesscrapping.csv", sep = ",", 
            qmethod = "double",col.names = NA)


# Assignment 15 Task 3: once you save the CSV file, commit and push it back to your repository (no R scripts involved for Task 3). 


