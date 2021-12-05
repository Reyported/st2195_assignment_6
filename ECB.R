library(dplyr)
library(tidyr)
library(zoo)
library(text2vec)
library(tidyverse)



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

#check for outliers in 2 rows 2 columns
par(mfrow=c(2,1))
plot(fxwithspeeches$date, fxwithspeeches$value, type='l', xlab='Date', 
     ylab='Exchange Rate', main='Changes in exchange rate over time')
hist(fxwithspeeches$value, xlab="value", main="Histogram of value", breaks = sqrt(nrow(fxwithspeeches)))

#since histogram shows possible few values that are outliers on the right side, boxplot of entire dataset is graphed
(ggplot(fxwithspeeches) + aes(x="", y=value)+
  geom_boxplot() )
#no outliers in full dataset boxplot, investigating further by splicing over 10 year breaks
fxwithspeeches %>% mutate(dategroup = cut(date, breaks = "10 years")) %>%
  ggplot() +
  geom_boxplot(aes(x=dategroup, y=value, color =dategroup))
#"outliers" are present every year, however this is normal since fx is volatile daily
fxwithspeeches %>% mutate(dategroup = cut(date, breaks = "1 year")) %>%
  ggplot() +
  geom_boxplot(aes(x=dategroup, y=value, color =dategroup))


#KIV- outlier detection based on z-score/IQR since "outliers do not exist in this scenario"
#shift fxwithspeeches to x before removing outliers
 #x <- fxwithspeeches
 #outliers using z-score (insignificant when z>)
 #x$z_scores <- as.data.frame(abs((x$value-mean(x$value))/sd(x$value)))


 
                           