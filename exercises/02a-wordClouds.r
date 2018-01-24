library(tm)		# load all required packages, install if not loaded
library(SnowballC)
library(wordcloud)

################### importing files ##########################

### import a single file
docs=readLines(file.choose())
x1 = Corpus(VectorSource(docs))  	# Constructs a source for a vector as input
x1 = tm_map(x1, stripWhitespace) 	# removes white space
x1 = tm_map(x1, content_transformer(tolower))		# converts to lower case
x1 = tm_map(x1, removePunctuation)	# removes punctuation marks
x1 = tm_map(x1, removeNumbers)		# removes numbers in the documents
x1 = tm_map(x1, removeWords, c(stopwords('english'), "german", "germany","hitler","hitlers"))

## VCorpus *


### create a corpus from many documents
x1=Corpus(DirSource(directory="path/to/your/directory"))
x1=Corpus(DirSource(directory="C://Users//user//Desktop//tertiary//Rprogramming//RtextMining//datasets//corpus"))
x1 = tm_map(x1, stripWhitespace) 	# removes white space
x1 = tm_map(x1, content_transformer(tolower))		# converts to lower case
x1 = tm_map(x1, removePunctuation)	# removes punctuation marks
x1 = tm_map(x1, removeNumbers)		# removes numbers in the documents
x1 = tm_map(x1, removeWords, c(stopwords('english'), "project", "null"))

### VCorpus > convert into another corpus type


### corpus
x1    # at this stage x1 is a corpus
inspect(x1)


### term document Frequency
tdm0 = TermDocumentMatrix(x1)
inspect(tdm0)

### document term frequency
dtm01 = t(tdm0)
inspect(dtm01)


### data frame
tdm = as.matrix(tdm0)
word_freqs = sort(rowSums(tdm), decreasing=TRUE)
dm = data.frame(word=names(word_freqs), freq=word_freqs)
View(dm)

barplot(dm$freq, rownames=dm$word)

########################################### Simple Word cloud

# plot wordcloud 1 (uses corpus)
wordcloud(x1, min.freq=3)
wordcloud(x1, random.order=FALSE, 
          colors=brewer.pal(6, "Dark2"),min.freq=3, scale=c(4,.2),rot.per=.15,max.words=100)


# plot wordcloud 2 (uses data frame)
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(6, "Dark2"),
          min.freq=3, scale=c(4,.2),rot.per=.15,max.words=100)

# plot wordcloud 3 (uses data frame)
library(ggplot2)
library(ggrepel)
library(dplyr)

dm %>% 
  slice(1:50) %>%
ggplot +
  aes(x = 1, y = 1, size = freq, label = word) +
  geom_text_repel(segment.size = 0, force = 100) +
  scale_size(range = c(2, 15), guide = FALSE) +
  scale_y_continuous(breaks = NULL) +
  scale_x_continuous(breaks = NULL) +
  labs(x = '', y = '') +
  theme_classic()

# plot wordcloud 4 (uses data frame)
# devtools::install_github("jbkunst/d3wordcloud")
library(d3wordcloud)

dm2=dm[1:150,]
d3wordcloud(dm2$word,dm2$freq)


# plot wordcloud 5 (uses document term frequency)

makewordc = function(a){	# plot wordcloud func opens
  a.colsum = apply(a, 2, sum);
  min1 = min(150, length(a.colsum))	# no more than 150 terms in wordcloud
  words = colnames(a)[1:min1]
  freq = a.colsum*100
  wordcloud(words, freq, scale=c(8, 0.3), colors=1:10)	} # func ends

makewordc(dtm01)

####################################### sentiment WordCloud

pos=scan(file.choose(), what="character", comment.char=";")	# read-in positive-words.txt
neg=scan(file.choose(), what="character", comment.char=";") 	# read-in negative-words.txt
pos.words=c(pos,"wow", "kudos", "hurray") 			# including our own positive words to the existing list
pos.words
neg.words = c(neg)
neg.words


# positive sentiment wordcloud (uses document term frequency)
makeposwordc = function(a){	# plot wordcloud func opens
  pos.matches = match(colnames(a), pos.words) 		# match() returns the position of the matched term or NA
  pos.matches = !is.na(pos.matches)
  b1 = apply(a, 2, sum)[pos.matches];	 b1 = as.data.frame(b1);
  colnames(b1) = c("freq");
  wordcloud(rownames(b1), b1[,1], scale=c(5, 1), colors=1:10)  	# wordcloud of positive words
}	# function ends for positive wordcloud

# negative sentiment wordcloud (uses document term frequency)
makenegwordc = function(a){	# plot wordcloud func opens
  neg.matches = match(colnames(a), neg.words) 		# match() returns the position of the matched term or NA
  neg.matches = !is.na(neg.matches)
  b1 = apply(a, 2, sum)[neg.matches];	 b1 = as.data.frame(b1);
  colnames(b1) = c("freq");
  wordcloud(rownames(b1), b1[,1], scale=c(5, 1), colors=1:10)  	# wordcloud of negative words
}	# func ends


# positive plot
makeposwordc(dtm01)

# negative plot
makenegwordc(dtm01)


################################ comparison and commonality wordcloud

### for this you must use two documents to compare several documents
# convert to term document frequency

tdm = as.matrix(tdm0)
colnames(tdm)
comparison.cloud(tdm, max.words = 100, random.order = F)
commonality.cloud(tdm, max.words = 100, random.order=F)


################################ nGrams cloud

library(quanteda)

corp2=corpus(x1)

# plot a unigram
dfm0=dfm(corp2, remove=stopwords("english"), remove_punct=TRUE, stem=TRUE)

textplot_wordcloud(dfm0, min.freq=3, random.order=FALSE, rot.per=.25,
                   colors=RColorBrewer::brewer.pal(8,"Dark2"))


# plot a bigram
dfm2=dfm(corp2, remove=stopwords("english"), remove_punct=TRUE, stem=TRUE, ngrams=2)
textplot_wordcloud(dfm2, min.freq=2, random.order=FALSE, rot.per=.25,
                   colors=RColorBrewer::brewer.pal(8,"Dark2"))


# plot a trigram
dfm3=dfm(corp2, remove=stopwords("english"), remove_punct=TRUE, stem=TRUE, ngrams=3)
textplot_wordcloud(dfm3, min.freq=2, random.order=FALSE, rot.per=.25,
                   colors=RColorBrewer::brewer.pal(8,"Dark2"))

