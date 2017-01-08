# This is an in-class contest hosted by University of Michigan SI650 (Information Retrieval)
# 
# This is a text classification task - sentiment classification.
#Every document (a line in the data file) is a sentence extracted 
#from social media (blogs). 
#Your goal is to classify the sentiment of each sentence into "positive" or "negative". 
# 
# The training data contains 7086 sentences, already labeled with 1 (positive sentiment) or 0 (negative sentiment).
#The test data contains 33052 sentences that are unlabeled. The submission should be a .txt file with 33052 lines.
#In each line, there should be exactly one integer, 0 or 1, according to your classification results. 

library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)
library(e1071)
library("syuzhet")
library("RTextTools")
library(stringr)
library(tm)
library("wordcloud")
set.seed(123)
data=read.delim("C:\\Falguni\\Data Science\\Work\\Springboard_project\\training.txt",sep="\t",quote="")
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
counts = table(clean_data$positive)
barplot(counts, axes = TRUE,main="Tweets",beside=TRUE,legend.text = c("0-Negative, 1-Positive"),  xlab = "Positive/Negative Tweets", ylab = "Count")
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

col=brewer.pal(6,"Dark2")
wordcloud(tweets_text_corpus, min.freq=15, scale=c(5,2),rot.per = 0.25,random.color=T, random.order=F,colors=col)



tweetsSparse=as.data.frame(as.matrix(sparse))
colnames(tweetsSparse)=make.names(colnames(tweetsSparse))
positive=clean_data$positive
final_data=cbind(positive,tweetsSparse)
split=sample.split(final_data$positive,SplitRatio = 0.7)
trainData=subset(final_data,split==TRUE)
splitTestData=subset(final_data,split==FALSE)
head(trainData$positive)
##############Using naiveBayes to predict###########################
classifier = naiveBayes(positive ~.,data=trainData )
predicted = predict(classifier, newdata=trainData,type="class")
table(trainData$positive, predicted)
recall_accuracy(trainData$positive, predicted)
predictedTest  = predict(classifier,newdata = splitTestData)
2679/(2679+537)
recall_accuracy(splitTestData$positive,predictedTest)
##############Using RPART to predict###########################
rpartTrain=randomForest(positive ~.,data=trainData,method="class")
prp(rpartTrain)
#lmTweets=lm(sentimentScore ~ .,data=trainmodel)
#summary(lmTweets)
predictRpartTrain = predict(rpartTrain,newdata=trainData,type = "class")
table(trainData$positive, predictRpartTrain)
recall_accuracy(trainData$positive, predictRpartTrain)

predictRpartTest = predict(rpartTrain,newdata=splitTestData,type = "class")
table(splitTestData$positive, predictRpartTest)
recall_accuracy(splitTestData$positive, predictRpartTest)


###############################Test data###############################################

testdata=read.delim("C:\\Falguni\\Data Science\\Work\\Springboard_project\\testdata.txt",sep="\t",quote="")
dim(testdata)
colnames(testdata)=c("tweet")
#testdata$positive=as.factor(testdata$positive)
testdata$tweet = as.character(testdata$tweet)
dim(testdata)
str(testdata)

testdata$tweet= gsub("&amp", "", testdata$tweet)
testdata$tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", testdata$tweet)

testdata$tweet = gsub("@\\w+", "", testdata$tweet)
testdata$tweet = gsub("[[:punct:]]", "", testdata$tweet)
testdata$tweet = gsub("[[:digit:]]", "", testdata$tweet)
testdata$tweet = gsub("http\\w+", "", testdata$tweet)
testdata$tweet = gsub("[ \t]{2,}", "", testdata$tweet)
testdata$tweet = gsub("^\\s+|\\s+$", "", testdata$tweet) 
testdata$tweet <- str_replace_all(testdata$tweet, "http://t.co/[a-z,A-Z,0-9]","")
testdata$tweet <- str_replace_all(testdata$tweet, "https://t.co/[a-z,A-Z,0-9]","")
testdata$tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", testdata$tweet)

# Get rid of URLs
testdata$tweet <- str_replace_all(testdata$tweet, "http://t.co/[a-z,A-Z,0-9]*","")
# Take out retweet header, there is only one
testdata$tweet <- str_replace(testdata$tweet,"RT @[a-z,A-Z]*: ","")
# Get rid of hashtags
testdata$tweet <- str_replace_all(testdata$tweet,"#[a-z,A-Z]*","")
# Get rid of references to other screennames
testdata$tweet <- str_replace_all(testdata$tweet,"@[a-z,A-Z]*","")   
#clean_tweet=unique(clean_tweet)
testdata_clean=testdata
dim(testdata_clean)
nrow(testdata_clean)
test_tweets_text = testdata_clean$tweet
test_tweets_text_corpus = Corpus(VectorSource(test_tweets_text))

#clean up
test_tweets_text_corpus=tm_map(test_tweets_text_corpus,content_transformer(function(x) iconv(x, to='UTF-8',sub='byte')))
test_tweets_text_corpus = tm_map(test_tweets_text_corpus,removePunctuation)
test_tweets_text_corpus = tm_map(test_tweets_text_corpus,content_transformer(tolower),mc.cores=1)
test_tweets_text_corpus = tm_map(test_tweets_text_corpus,content_transformer(function(x)removeWords(x,c("https","http","youtube",stopwords()))))
test_tweets_text_corpus = tm_map(test_tweets_text_corpus,stemDocument,language="english")
#tweets_text_corpus=tm_map(tweets_text_corpus,PlainTextDocument)
test_frequencies=DocumentTermMatrix(test_tweets_text_corpus)
inspect(test_frequencies[1000:1005,505:515])
findFreqTerms(test_frequencies,lowfreq = 15)
testsparse= removeSparseTerms(test_frequencies,0.995)
testTweetsSparse=as.data.frame(as.matrix(testsparse))
colnames(testTweetsSparse)=make.names(colnames(testTweetsSparse))
colnames(testTweetsSparse)
predictRpartTestData = predict(rpartTrain,newdata=head(testTweetsSparse),type = "class")
