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

#remove rows with empty, na or wrong values in date column
fxwithspeeches$date <- as.Date(fxwithspeeches$date, format= "%Y-%m-%d")
fxwithspeeches %>% drop_na(date)

#check for outliers
plot(fxwithspeeches$date, dxwithspeeches$value, type='0', xlab='Date', 
     ylab='Exchange Rate', main='Changes in exchange rate over time')

boxplot(value ~ Year, data=fxwithspeeches)


  
  
  
tail(fxwithspeeches)

head(fx,n=1)