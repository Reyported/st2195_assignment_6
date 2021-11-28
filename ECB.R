library(dplyr)

#load and merge the datasets
speeches <- read.csv("speeches.csv")
head(speeches)

fx <- read.csv("fx.csv")
head(fx)

fxwithspeeches <- fx %>% inner_join(speeches)
head(fxwithspeeches)