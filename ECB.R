library(dplyr)
library(tidyr)
library(zoo)
library(text2vec)



#load and merge the datasets
speeches <- read.csv("speeches.csv", header = TRUE, sep="|" )
speeches <- speeches[,c("date","contents")]

speeches %>%  
  group_by(date) %>%
  summarise(content = toString(contents)) %>%
  arrange(date)

fx <- read.csv("fx.csv", sep=",", skip = 5, na.strings="NA")
colnames(fx) <- c("date","value","obs status", "obs comment")

fx %>%
  group_by(date) %>%
  arrange(date)


fxwithspeeches <- left_join(fx,speeches, by="date", all.x=TRUE)

#change to correct column classes
fxwithspeeches$date <- as.Date(fxwithspeeches$date, format= "%Y-%m-%d")
fxwithspeeches$value <- as.numeric(fxwithspeeches$value)

#attempt to locf with day after data for missing values
fxwithspeeches$value <- na.locf(fxwithspeeches$value, na.rm=TRUE, fromLast=TRUE)

summary(fxwithspeeches)

#check for outliers
plot(fxwithspeeches$date, fxwithspeeches$value, type='o', xlab='Date', 
     ylab='Exchange Rate', main='Changes in exchange rate over time')

boxplot(value ~ Year, data=fxwithspeeches)


  
  
  
tail(fxwithspeeches)

head(fx,n=1)