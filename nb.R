library(tm)
library(naivebayes)
library(readr)
train <- read_csv("Downloads/train.csv")
View(train)

clean <- function(train) {
  cleantweets = gsub('http\\S+\\s*',"", train$text)
  cleantweets = gsub('@\\w+',"", cleantweets)
  cleantweets = gsub('http.*',"", cleantweets)
  cleantweets = gsub('&amp;', "", cleantweets)
  cleantweets = gsub('[[:punct:]]', '', cleantweets)
  cleantweets = gsub('%\\S+\\s*', "", cleantweets)
  cleantweets = gsub('RT\\s?@\\w+', '', cleantweets)
}

train$clean <- clean(train)
View(train)

#Keeping cleaned tweets
tweets <- train$clean
View(tweets)

#Coverting to corpus
corpus <- SimpleCorpus(VectorSource(tweets))

#Document Term Matrix
DTM <- DocumentTermMatrix(corpus)
inspect(DTM)

#Frequency Table of Word Counts
dtmatrix <- as.matrix(DTM)
View(dtmatrix)
freq <- sort(colSums(dtmatrix), decreasing = TRUE)
View(freq)

#Histogram
hist(freq, xlim = c(0,600))
#Coverting to DF
DTM_DF <- data.frame(as.matrix(DTM))
View(DTM_DF)

sentiments <- as.factor(train$sentiment)

for(i in 1: ncol(DTM_DF)) {
  DTM_DF[,i] <- as.factor(DTM_DF[,i] > 0)
}

#Training NB
nb <- naive_bayes(DTM_DF,sentiments)
nb$tables$fantastic

predict(nb)

newdf <- DTM_DF[1,]
newdf <- newdf == 6
newdf[,"fantastic"] <- TRUE
predict(nb,newdf)
predict(nb,newdf, type = "prob")
