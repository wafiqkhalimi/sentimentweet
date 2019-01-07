install.packages("tm")
install.packages("RTextTools")
install.packages("e1071")
install.packages("dplyr")
install.packages("caret")
install.packages("doMC", repos="http://R-Forge.R-project.org")


library(tm)
library(RTextTools)
library(e1071)
library(dplyr)
library(caret)


library(doMC)
registerDoMC(cores=detectCores())  # Use all available cores

df<- read.csv("E:/Perkuliahan/cobaR/data_uji.csv", stringsAsFactors = FALSE)
glimpse(df)

df$class <- as.factor(df$class)

custom_stopwords <- read.csv("E:/Perkuliahan/cobaR/stopword.csv", header = FALSE)
custom_stopwords <- as.character(custom_stopwords$V1)
custom_stopwords <- c(custom_stopwords, stopwords())

corpus <- Corpus(VectorSource(df$text))
corpus
inspect(corpus[1:3])


corpus.clean <- corpus %>%
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, custom_stopwords) %>%
  tm_map(stripWhitespace)


dtm <- DocumentTermMatrix(corpus.clean)
inspect(dtm[1:5, 1:4])

df.train <- df[1:30,]
df.test <- df[31:40,]

dtm.train <- dtm[1:30,]
dtm.test <- dtm[31:40,]
dim(dtm.train)

corpus.clean.train <- corpus.clean[1:30]
corpus.clean.test <- corpus.clean[31:40]


dtm.train.nb <- DocumentTermMatrix(corpus.clean.train)
dim(dtm.train.nb)

dtm.test.nb <- DocumentTermMatrix(corpus.clean.test)
dim(dtm.test.nb)



convert_count <- function(x) {
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("0", "1"))
  y
}

trainNB <- apply(dtm.train.nb, 2, convert_count)
testNB <- apply(dtm.test.nb, 2, convert_count)
system.time( classifier <- naiveBayes(trainNB, df.train$class, laplace = 1) )

predict(classifier, newdata=testNB)
system.time( pred <- predict(classifier, newdata=testNB) )


table("Predictions"= pred,  "Actual" = df.test$class )



conf.mat <- confusionMatrix(pred, df.test$class)

conf.mat


library("wordcloud")
library("RColorBrewer")

dta <- TermDocumentMatrix(corpus.clean)
m <- as.matrix(dta)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 9)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))



head(d, 9)
barplot(d[1:9,]$freq, las = 2, names.arg = d[1:9,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")

dr<- read.csv("E:/Perkuliahan/cobaR/data_test.csv", stringsAsFactors = FALSE)
factor(dr$class)
w=table(dr$class,paste(dr$username,sep ="."))
w=table(dr$username, dr$class)
class(w)
t=as.data.frame(w)
names(t)[1] = 'username'
t
