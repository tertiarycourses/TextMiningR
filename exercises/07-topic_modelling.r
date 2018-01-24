
################################ Topic Modelling #################################
library(tm)

file1=readLines(file.choose())

textCorpus=Corpus(VectorSource(file1))  # convert to corpus

textCorpus  # view
inspect(textCorpus[[1]]) # view first line

textCorpus=tm_map(textCorpus, tolower) # convert all to lower
textCorpus=tm_map(textCorpus, removePunctuation)
textCorpus=tm_map(textCorpus, removeNumbers)
textCorpus=tm_map(textCorpus, removeWords, stopwords("english")) # remove common words
textCorpus=tm_map(textCorpus, removeWords,c("eisenhower"))  # remove user defined words
textCorpus=tm_map(textCorpus, stemDocument)
textCorpus=tm_map(textCorpus, stripWhitespace)

dtm=DocumentTermMatrix(textCorpus)

rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
dtm.new   <- dtm[rowTotals> 0, ]

k=4 # number of topics > we set our number of topics


# Latent Dirichlet Allocation
# latent Dirichlet allocation (LDA) is a generative statistical model that allows sets of observations to 
# be explained by unobserved groups that explain why some parts of the data are similar. For example, if 
# observations are words collected into documents, it posits that each document is a mixture of a small 
# number of topics and that each word's creation is attributable to one of the document's topics.


library(topicmodels)
lda=LDA(dtm.new, k=k)

topics(lda)
terms(lda)


term=terms(lda,4)
term
term=apply(term, MARGIN=2, paste, collapse=", ")


gammaDF = as.data.frame(lda@gamma)
names(gammaDF) = c(1:k)
toptopics = as.data.frame(cbind(document = row.names(gammaDF), 
                                 topic = apply(gammaDF,1,function(x) names(gammaDF)[which(x==max(x))])))
toptopics 

barplot(table(toptopics$topic), col="red")


#### another method

SEED = 1234;

my_TM = list(VEM = LDA(dtm.new, k = k, control = list(seed = SEED)),
              VEM_fixed = LDA(dtm.new, k = k,
              control = list(estimate.alpha = FALSE, seed = SEED)),
              Gibbs = LDA(dtm.new, k = k, method = "Gibbs",
              control = list(seed = SEED, burnin = 1000, thin = 100, iter = 1000)),
              CTM = CTM(dtm.new, k = k,
              control = list(seed = SEED,
              var = list(tol = 10^-4), em = list(tol = 10^-3))));
 
Topic = topics(my_TM[["VEM"]], 1);
 
#top 5 terms for each topic in LDA
Terms = terms(my_TM[["VEM"]], 5);
Terms;
 
(my_topics = topics(my_TM[["VEM"]]));
 
most_frequent = which.max(tabulate(my_topics));
 
terms(my_TM[["VEM"]], 10)[, most_frequent];



# Latent Semantic Indexing
# technique in natural language processing, in particular distributional semantics, of analyzing 
# relationships between a set of documents and the terms they contain by producing a set of concepts 
# related to the documents and terms. LSA assumes that words that are close in meaning will occur in 
# similar pieces of text (the distributional hypothesis). A matrix containing word counts per paragraph 
# (rows represent unique words and columns represent each paragraph) is constructed from a large piece of 
# text and a mathematical technique called singular value decomposition (SVD) is used to reduce the number 
# of rows while preserving the similarity structure among columns. Words are then compared by taking the 
# cosine of the angle between the two vectors (or the dot product between the normalizations of the two 
# vectors) formed by any two rows. Values close to 1 represent very similar words while values close to 0 
# represent very dissimilar words.

myTdm=TermDocumentMatrix(textCorpus, control=list(wordLengths=c(2,Inf)))
findFreqTerms(myTdm, lowfreq=2)
dim(myTdm)

tdm.matrix=as.matrix(myTdm)
tdm.matrix


library(lsa)
ptdm=tdm.matrix/rowSums(tdm.matrix)           # tdm is term document matrix
GW=1-entropy(ptdm)/log(ncol(tdm.matrix))
LW=log(1+tdm.matrix)

X=GW * LW


# library(wordcloud)
# termFreq=rowSums(X)
# windows()
# wordcloud(myTdm, termFreq, color=1:length(myTdm))

termSim=cosine(t(X))

myLSA=lsa(X)

dim(X)
head(myLSA$tk)
head(myLSA$dk)
myLSA$sk

## Singular Value Decomposition
termVec=myLSA$tk * myLSA$sk
docVec=myLSA$dk * myLSA$sk

termSimLSA=cosine(t(termVec))

word='general'   # the word we wish to compare

head(termSim[word,])

head(termSimLSA[word,])



################################ Topic Modelling (part 2) #################################


library(topicmodels)

# This is a collection of 2246 news articles 

data("AssociatedPress")
AssociatedPress

# create a two-topic LDA model

ap_lda <- LDA(AssociatedPress, k = 2, control = list(seed = 1234))

ap_lda


########################  Word-topic probabilities

library(tidytext)

ap_topics <- tidy(ap_lda, matrix = "beta")

ap_topics


# use dplyr's top_n() to find the 10 terms that are most common within each topic

library(ggplot2)
library(dplyr)

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()


# we can filter for relatively common words, such as those that have a  ßß  greater than 
# 1/1000 in at least one topic.

library(tidyr)

beta_spread <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

beta_spread


####################### Document-topic probabilities

ap_documents <- tidy(ap_lda, matrix = "gamma")

ap_documents


#We can see that many of these documents were drawn from a mix of the two topics, but that 
#document 6 was drawn almost entirely from topic 2, 

tidy(AssociatedPress) %>%
  filter(document == 6) %>%
  arrange(desc(count))



# topic modeling to discover how chapters cluster into distinct topics, each of them 
# (presumably) representing one of the books.

titles <- c("Twenty Thousand Leagues under the Sea", "The War of the Worlds",
            "Pride and Prejudice", "Great Expectations")



library(gutenbergr)

books <- gutenberg_works(title %in% titles) %>%
  gutenberg_download(meta_fields = "title")


library(stringr)

# divide into documents, each representing one chapter
by_chapter <- books %>%
  group_by(title) %>%
  mutate(chapter = cumsum(str_detect(text, regex("^chapter ", ignore_case = TRUE)))) %>%
  ungroup() %>%
  filter(chapter > 0) %>%
  unite(document, title, chapter)

# split into words
by_chapter_word <- by_chapter %>%
  unnest_tokens(word, text)

# find document-word counts
word_counts <- by_chapter_word %>%
  anti_join(stop_words) %>%
  count(document, word, sort = TRUE) %>%
  ungroup()

word_counts


# LDA on chapters

chapters_dtm <- word_counts %>%
  cast_dtm(document, word, n)

chapters_dtm

chapters_lda <- LDA(chapters_dtm, k = 4, control = list(seed = 1234))

chapters_lda


chapter_topics <- tidy(chapters_lda, matrix = "beta")

chapter_topics


# We could use dplyr's top_n() to find the top 5 terms within each topic.

top_terms <- chapter_topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms



library(ggplot2)

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()



################### Per document classification

# which topics are associated with each document

chapters_gamma <- tidy(chapters_lda, matrix = "gamma")

chapters_gamma

chapters_gamma <- chapters_gamma %>%
  separate(document, c("title", "chapter"), sep = "_", convert = TRUE)

chapters_gamma

chapters_gamma %>%
  mutate(title = reorder(title, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ title)


#First we'd find the topic that was most associated with each chapter using top_n(), which 
#is effectively the 'classification' of that chapter.


chapter_classifications <- chapters_gamma %>%
  group_by(title, chapter) %>%
  top_n(1, gamma) %>%
  ungroup()

chapter_classifications


book_topics <- chapter_classifications %>%
  count(title, topic) %>%
  group_by(title) %>%
  top_n(1, n) %>%
  ungroup() %>%
  transmute(consensus = title, topic)

chapter_classifications %>%
  inner_join(book_topics, by = "topic") %>%
  filter(title != consensus)


# augment() uses a model to add information to each observation in the original data

assignments <- augment(chapters_lda, data = chapters_dtm)

assignments

assignments <- assignments %>%
  separate(document, c("title", "chapter"), sep = "_", convert = TRUE) %>%
  inner_join(book_topics, by = c(".topic" = "topic"))

assignments


# visualize a confusion matrix, showing how often words from one book were assigned to another
library(scales)


assignments %>%
  count(title, consensus, wt = count) %>%
  group_by(title) %>%
  mutate(percent = n / sum(n)) %>%
  ggplot(aes(consensus, title, fill = percent)) +
  geom_tile() +
  scale_fill_gradient2(high = "red", label = percent_format()) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid = element_blank()) +
  labs(x = "Book words were assigned to",
       y = "Book words came from",
       fill = "% of assignments")



# What were the most commonly mistaken words?

wrong_words <- assignments %>%
  filter(title != consensus)

wrong_words


wrong_words %>%
  count(title, consensus, term, wt = count) %>%
  ungroup() %>%
  arrange(desc(n))



# we can confirm 'flopson' appears only in Great Expectations, even though it's assigned to 
# the "Pride and Prejudice" cluster.

word_counts %>%
  filter(word == "flopson")



#################################### Algorithms ##########################################

# Latent Semantic Indexing
# Latent Dirichlet Allocation
# Hierarchical Dirichlet Process

