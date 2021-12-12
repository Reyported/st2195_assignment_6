library(dplyr)
library(tidyr)
library(zoo)
library(text2vec)
library(tidytext)
library(tidyverse)
library(reshape2)



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

#change to correct column classes (this would remove any non-conforming date/value entries)
fxwithspeeches$date <- as.Date(fxwithspeeches$date, format= "%Y-%m-%d")
fxwithspeeches$value <- as.numeric(fxwithspeeches$value)

summary(fxwithspeeches)


#check for outliers in 2 rows 2 columns
par(mfrow=c(2,1))
plot(fxwithspeeches$date, fxwithspeeches$value, type='l', xlab='Date', 
     ylab='Exchange Rate', main='Changes in exchange rate over time')
hist(fxwithspeeches$value, xlab="value", main="Histogram of value", breaks = sqrt(nrow(fxwithspeeches)))

#boxplot of entire dataset is graphed
ggplot(fxwithspeeches) + aes(x="", y=value)+
  geom_boxplot() 
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

#attempt to locf with day after data for missing values
fxwithspeeches$value <- na.locf(fxwithspeeches$value, na.rm=TRUE, fromLast=TRUE)

summary(fxwithspeeches)

#calculate rate of return
ror <- fxwithspeeches %>%
  arrange(date)%>%
  mutate(return= (value-lag(value))/lag(value))
#extend the dataset with good news and bad news
ror$good_news <- ifelse(ror$return>"0.005",1,0)
ror$bad_news <- ifelse(ror$return<"-0.005",1,0)

#remove contents NA
Finaljoined <- ror[complete.cases(ror$contents),]

#splitting final dataframe to analyse text from good and bad respectively
good <- Finaljoined %>%
  filter(good_news == 1)

bad <- Finaljoined %>%
  filter(bad_news == 1)

goodwords <- tibble(text = good$contents)
badwords <- tibble(text = bad$contents)

#loading stop words from txt file
stop_words <- read.delim("stop_words_english.txt", header =FALSE, as.is=TRUE)
colnames(stop_words)<- c("word")

#counting words
goodwordstoken <- goodwords %>%
  unnest_tokens(output=word,input=text) %>%
  anti_join(stop_words)%>%
  count(word, sort=TRUE)

badwordstoken <- badwords %>%
  unnest_tokens(output=word,input=text) %>%
  anti_join(stop_words) %>%
  count(word, sort=TRUE)

head(goodwordstoken, n=20)
head(badwordstoken, n=20)

#remove specific weird words in both data sets
stop_words_pog<- c("â","la","en")
stop_words_pog<- melt(stop_words_pog)
colnames(stop_words_pog) <- c("word")
stop_words_pog <- full_join(stop_words_pog,stop_words)


#removing stop words again
goodwordstoken <- goodwords %>%
  unnest_tokens(output=word,input=text) %>%
  anti_join(stop_words_pog)%>%
  count(word, sort=TRUE)

badwordstoken <- badwords %>%
  unnest_tokens(output=word,input=text) %>%
  anti_join(stop_words_pog)%>%
  count(word, sort=TRUE)

#save to csv
good_csv <- head(goodwordstoken,n=20)
bad_csv <- head(badwordstoken,n=20)

write.csv(good_csv, "good_csv.csv")
write.csv(bad_csv, "bad_csv.csv")

##note that there are many common words amongst the speeches during good and bad news
##keywords used during such speeches may be similar, but perhaps splitting into a training/testing set for
#calculating a count of COMBINATIONS of words (since meaning is derived only from full phrases/sentences
#rather than individual words)
                           