library(readr)
text <-read_lines(file.choose())   # from readr package
text


# In order to turn it into a tidy text dataset, we first need to put it into a data frame.

ll=length(text)

library(dplyr)
text_df <- data_frame(line = 1:ll, text = text)

text_df

# A token is a meaningful unit of text, most often a word, that we are interested in using 
# for further analysis, and tokenization is the process of splitting text into tokens.

library(tidyverse)
library(tidytext)


text_dfU = text_df %>%
  unnest_tokens(word, text)


####### We can remove stop words 

data(stop_words)


text_df_RM <- text_dfU %>%
  anti_join(stop_words)


text_df_RM %>%
  count(word, sort = TRUE)

##################### creating a wordcloud 
library(wordcloud)


text_df_RM %>%
  count(word, sort=TRUE) %>%
  with(wordcloud(word, n, max.words=100))


###################### frequency analysis 

textCorpus=text_df %>%
    unnest_tokens(word,text)%>%
    count(line,word, sort = TRUE) %>%
    cast_tdm(line,word,n)                      # convert to term document matrix
                                               # cast_dtm > document term matrix


textCorpus=text_df %>%
  unnest_tokens(word,text)%>%
  count(line,word, sort = TRUE) %>%
  cast_tdm(line,Corword,n) 


#frequency =TermDocumentMatrix(textCorpus)  # generate a matrix of the words

frequency=textCorpus
rownames(frequency)=textCorpus$dimnames$Docs[as.numeric(rownames(frequency))]

# find frequent terms
findFreqTerms(frequency, lowfreq=20)  # see words that appear at least 20 times

# find associative terms
findAssocs(frequency, c("war","german"),c(.5,.6))



myTdm <- as.matrix(frequency)
FreqMat <- data.frame(ST = rownames(myTdm), 
                      Freq = rowSums(myTdm), 
                      row.names = NULL)
head(FreqMat, 10)

data=subset(FreqMat, Freq>100)

library(ggplot2)   
plot = ggplot(data,aes(ST, Freq))  # these are the column headers    
plot = plot + geom_bar(stat="identity")   
plot = plot + theme(axis.text.x=element_text(angle=45, hjust=1))   
plot

################################################################################################################
#						Documents
################################################################################################################


##################### Term frequency in Jane Austen,s novels

# What are the most commonly used words in Jane Austen,s novels

library(dplyr)
library(janeaustenr)
library(tidytext)

book_words <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word, sort = TRUE) %>%
  ungroup()

total_words <- book_words %>% 
  group_by(book) %>% 
  summarize(total = sum(n))

book_words <- left_join(book_words, total_words)

book_words


library(ggplot2)

ggplot(book_words, aes(n/total, fill = book)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~book, ncol = 2, scales = "free_y")



###################### The bind_tf_idf function

# function in the tidytext package takes a tidy text dataset as input with one row per token (term), per document

#TF-IDF > is a statistic to measure about word in corpus to identify 
#words which are common and important. Words important to one document 
#in a collection of document

book_words <- book_words %>%
  bind_tf_idf(word, book, n)

book_words

# Let's look at terms with high tf-idf in Jane Austen’s works.

book_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))


plot_austen <- book_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word))))

plot_austen %>% 
  top_n(20) %>%
  ggplot(aes(word, tf_idf, fill = book)) +
  geom_col() +
  labs(x = NULL, y = "tf-idf") +
  coord_flip()


# Let's look at the novels individually.

plot_austen %>% 
  group_by(book) %>% 
  top_n(15) %>% 
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = book)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~book, ncol = 2, scales = "free") +
  coord_flip()



### corpus of physics text

library(gutenbergr)
physics <- gutenberg_download(c(37729, 14725, 13476, 5001), 
                              meta_fields = "author")

physics_words <- physics %>%
  unnest_tokens(word, text) %>%
  count(author, word, sort = TRUE) %>%
  ungroup()

physics_words


# calculate tf-idf, then visualize the high tf-id words

physics_words <- physics_words %>%
  bind_tf_idf(word, author, n) 

plot_physics <- physics_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  mutate(author = factor(author, levels = c("Galilei, Galileo",
                                            "Huygens, Christiaan", 
                                            "Tesla, Nikola",
                                            "Einstein, Albert")))

plot_physics %>%
  top_n(20) %>%
  ggplot(aes(word, tf_idf, fill = author)) +
  geom_col() +
  labs(x = NULL, y = "tf-idf") +
  coord_flip()


# Let's look at each text individually 

plot_physics %>% 
  group_by(author) %>% 
  top_n(15, tf_idf) %>% 
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = author)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~author, ncol = 2, scales = "free") +
  coord_flip()



# Let's remove some of these less meaningful words to make a better, more meaningful plot.

mystopwords <- data_frame(word = c("eq", "co", "rc", "ac", "ak", "bn", 
                                   "the","fig", "file", "cg", "cb", "cm"))


physics_words <- anti_join(physics_words, mystopwords, by = "word")

plot_physics <- physics_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(author) %>% 
  top_n(15, tf_idf) %>%
  ungroup %>%
  mutate(author = factor(author, levels = c("Galilei, Galileo",
                                            "Huygens, Christiaan",
                                            "Tesla, Nikola",
                                            "Einstein, Albert")))

ggplot(plot_physics, aes(word, tf_idf, fill = author)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~author, ncol = 2, scales = "free") +
  coord_flip()
