library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)
library(e1071)
library("syuzhet")
library("RTextTools")
library(stringr)
library(tm)
set.seed(123)
data=read.delim("C:\\Falguni\\Data Science\\training.txt",sep="\t",quote="")
colnames(data)=c("positive","tweet")
data$positive=as.factor(data$positive)
table(data$positive)
data$tweet = as.character(data$tweet)
dim(data)
str(data)

data$tweet= gsub("&amp", "", data$tweet)
data$tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", data$tweet)

data$tweet = gsub("@\\w+", "", data$tweet)
data$tweet = gsub("[[:punct:]]", "", data$tweet)
data$tweet = gsub("[[:digit:]]", "", data$tweet)
data$tweet = gsub("http\\w+", "", data$tweet)
data$tweet = gsub("[ \t]{2,}", "", data$tweet)
data$tweet = gsub("^\\s+|\\s+$", "", data$tweet) 
data$tweet <- str_replace_all(data$tweet, "http://t.co/[a-z,A-Z,0-9]","")
data$tweet <- str_replace_all(data$tweet, "https://t.co/[a-z,A-Z,0-9]","")
data$tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", data$tweet)

# Get rid of URLs
data$tweet <- str_replace_all(data$tweet, "http://t.co/[a-z,A-Z,0-9]*","")
# Take out retweet header, there is only one
data$tweet <- str_replace(data$tweet,"RT @[a-z,A-Z]*: ","")
# Get rid of hashtags
data$tweet <- str_replace_all(data$tweet,"#[a-z,A-Z]*","")
# Get rid of references to other screennames
data$tweet <- str_replace_all(data$tweet,"@[a-z,A-Z]*","")   
#clean_tweet=unique(clean_tweet)
clean_data=data
dim(clean_data)
nrow(clean_data)
tweets_text = clean_data$tweet
tweets_text_corpus = Corpus(VectorSource(tweets_text))

#clean up
tweets_text_corpus=tm_map(tweets_text_corpus,content_transformer(function(x) iconv(x, to='UTF-8',sub='byte')))
tweets_text_corpus = tm_map(tweets_text_corpus,removePunctuation)
tweets_text_corpus = tm_map(tweets_text_corpus,content_transformer(tolower),mc.cores=1)
tweets_text_corpus = tm_map(tweets_text_corpus,content_transformer(function(x)removeWords(x,c("https","http","youtube",stopwords()))))
tweets_text_corpus = tm_map(tweets_text_corpus,stemDocument,language="english")
#tweets_text_corpus=tm_map(tweets_text_corpus,PlainTextDocument)
frequencies=DocumentTermMatrix(tweets_text_corpus)
inspect(frequencies[1000:1005,505:515])
findFreqTerms(frequencies,lowfreq = 15)

sparse= removeSparseTerms(frequencies,0.995)
tweetsSparse=as.data.frame(as.matrix(sparse))
colnames(tweetsSparse)=make.names(colnames(tweetsSparse))
positive=clean_data$positive
final_data=cbind(positive,tweetsSparse)
split=sample.split(final_data$positive,SplitRatio = 0.7)
trainData=subset(final_data,split==TRUE)
testData=subset(final_data,split==FALSE)
head(trainData$positive)
classifier = naiveBayes(positive ~.,data=trainData )
predicted = predict(classifier, newdata=trainData,type="class")
table(trainData$positive, predicted)

recall_accuracy(trainData$positive, predicted)
predictedTest  = predict(classifier,newdata = testData)
2679/(2679+537)
recall_accuracy(testData$positive,predictedTest)

rpartTrain=rpart(positive ~.,data=trainData,method="class")
prp(rpartTrain)
#lmTweets=lm(sentimentScore ~ .,data=trainmodel)
#summary(lmTweets)
predictRpartTrain = predict(rpartTrain,newdata=trainData,type = "class")
table(trainData$positive, predictRpartTrain)
recall_accuracy(trainData$positive, predictRpartTrain)

predictRpartTest = predict(rpartTrain,newdata=testData,type = "class")
table(testData$positive, predictRpartTest)
recall_accuracy(testData$positive, predictRpartTest)

