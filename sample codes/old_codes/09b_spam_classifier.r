library(tm)		# load all required packages, install if not loaded
library(SnowballC)
library(wordcloud)

################### importing files ##########################

### import a spam SMS file
docs=read.csv(file.choose())
docs2=docs$Text
x1 = Corpus(VectorSource(docs2))  	# Constructs a source for a vector as input
x1 = tm_map(x1, stripWhitespace) 	# removes white space
x1 = tm_map(x1, tolower)		# converts to lower case
x1 = tm_map(x1, removePunctuation)	# removes punctuation marks
x1 = tm_map(x1, removeNumbers)		# removes numbers in the documents
x1 = tm_map(x1, removeWords, c(stopwords('english'), "project", "null"))

tdm=TermDocumentMatrix(x1)

dtm=DocumentTermMatrix(x1)



spam_cloud=which(docs$Type == "spam")
ham_cloud=which(docs$Type =="ham")

library(wordcloud)
wordcloud(x1[spam_cloud], min.freq = 20)    # already cleaned corpus
wordcloud(x1[ham_cloud], min.freq = 20)


################### Naive Bayes Classifier #########################

library(dplyr)

# split into test and train data for datascience
train=sample_frac(docs, 0.7)
sid=as.numeric(rownames(train)) # because rownames() returns character
test=docs[-sid,]

#label for dataset
data_train_labels=train$Type
data_test_labels=test$Type

# document term matrix dataset
sms_dtm_train=dtm[sid,]
sms_dtm_test=dtm[-sid,]

# corpus data set
sms_corpus_clean_train=x1[sid]
sms_corpus_clean_test=x1[-sid]

spam=subset(train, Type=="spam")
ham=subset(train, Type=="ham")

# finding freqent words
frequent_words= findFreqTerms(sms_dtm_train, 5)
length(frequent_words)
frequent_words[1:10]

# createing term matrix using freq terms
sms_freq_word_train=sms_dtm_train[, frequent_words]
sms_freq_word_test=sms_dtm_test[, frequent_words]

yes_or_no=function(x){
  y=ifelse(x>0, 1,0)
  y=factor(y, levels=c(0,1), labels=c("No","Yes"))
  y
}


sms_train=apply(sms_freq_word_train,2,yes_or_no)
sms_test=apply(sms_freq_word_test,2,yes_or_no)


library(e1071)

sms_classifier=naiveBayes(sms_train, data_train_labels)  
class(sms_classifier)

sms_test_pred=predict(sms_classifier, newdata=sms_test)   # will take about 30 seconds
table(sms_test_pred, data_test_labels)   # see our confusion matrix



###### Classify your own document

newww=read.csv(file.choose())   # make sure column names are same
new2=newww$Text

x2 = Corpus(VectorSource(new2))  	# Constructs a source for a vector as input
x2 = tm_map(x2, stripWhitespace) 	# removes white space
x2 = tm_map(x2, tolower)		# converts to lower case
x2 = tm_map(x2, removePunctuation)	# removes punctuation marks
x2 = tm_map(x2, removeNumbers)		# removes numbers in the documents
x2 = tm_map(x2, removeWords, c(stopwords('english'), "project", "null"))

tdm2=TermDocumentMatrix(x2)

dtm2=DocumentTermMatrix(x2)

sms_test2=apply(dtm2,2,yes_or_no)

sms_test_pred=predict(sms_classifier, newdata=sms_test2)
sms_test_pred
