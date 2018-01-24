
############################################################################
#					nGrams
############################################################################

# By seeing how often word X is followed by word Y, we can then build a model of the # relationships between them.

library(dplyr)
library(tidytext)
library(janeaustenr)

austen_bigrams <- austen_books() %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

austen_bigrams


austen_bigrams %>%
  count(bigram, sort = TRUE)


# lets us separate it into two columns, "word1' and 'word2', at which point we can remove 
# cases where either is a stop-word.

library(tidyr)

bigrams_separated <- austen_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_counts

# let us find the most common bigrams not containing stop-words.

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigrams_united


# the most common trigrams, which are consecutive sequences of 3 words.

austen_books() %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)


########################## analyzing bigrams

# we might be interested in the most common "streets" mentioned in each book:

bigrams_filtered %>%
  filter(word2 == "street") %>%
  count(book, word1, sort = TRUE)


# we can look at the tf-idf  of bigrams across Austen novels
# within each book, just as we did for words

bigram_tf_idf <- bigrams_united %>%
  count(book, bigram) %>%
  bind_tf_idf(bigram, book, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf


######################### bigrams for sentiment analysis

# Now that we have the data organized into bigrams, it's easy to tell how often words are 
# preceded by a word like "not" :

bigrams_separated %>%
  filter(word1 == "not") %>%
  count(word1, word2, sort = TRUE)

AFINN <- get_sentiments("afinn")

AFINN

# We can then examine the most frequent words that were preceded by "not" and were 
# associated with a sentiment.

not_words <- bigrams_separated %>%
  filter(word1 == "not") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, score, sort = TRUE) %>%
  ungroup()

not_words


not_words %>%
  mutate(contribution = n * score) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * score, fill = n * score > 0)) +
  geom_col(show.legend = FALSE) +
  xlab("Words preceded by \"not\"") +
  ylab("Sentiment score * number of occurrences") +
  coord_flip()



# "Not" isn't the only term that provides some context for the following word. We could 
# pick four common words (or more) that negate the subsequent term

negation_words <- c("not", "no", "never", "without")

negated_words <- bigrams_separated %>%
  filter(word1 %in% negation_words) %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word1, word2, score, sort = TRUE) %>%
  ungroup()


######################## Visualizing a network of bigrams


library(igraph)

# original counts
bigram_counts

bigram_graph <- bigram_counts %>%
  filter(n > 20) %>%
  graph_from_data_frame()

bigram_graph


library(ggraph)
set.seed(2017)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

#### Challenge
# create a bigram graph of the hitler text


set.seed(2016)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()


# Visualizing bigrams in other texts



library(dplyr)
library(tidyr)
library(tidytext)
library(ggplot2)
library(igraph)
library(ggraph)

count_bigrams <- function(dataset) {
  dataset %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>%
    count(word1, word2, sort = TRUE)
}


visualize_bigrams <- function(bigrams) {
  set.seed(2016)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
}



### try with our dataset
library(readr)
text <-read_lines(file.choose())
ll=length(text)
library(dplyr)
text_df <- data_frame(line = 1:ll, text = text)


myBigram= count_bigrams(text_df[1:10,])   # choose first 10 lines > file too big

visualize_bigrams(myBigram)

################################################################################
#                      Counting and correlating pairs of words
################################################################################

##### textassociations 

# changing to tdm > term document matrix
# it structures the text in a matrix where each term is organized in a column
# each row is a document and the number represents the count in the terms

data=readLines(file.choose())

textCorpus=Corpus(VectorSource(data))

textTdm=TermDocumentMatrix(textCorpus)
textTdm

# frequent terms
findFreqTerms(textTdm, lowfreq=11) # term at least 11 times

# associations
findAssocs(textTdm, 'german', 0.30)
# search term 'german' from our above list with correlation of 0.3

plot(textTdm,
     terms=findFreqTerms(textTdm, lowfreq=11)[1:50],
     corThreshold=0.3)


#"Pride and Prejudice" divided into 10-line sections, as we did (with larger sections) for 
#sentiment analysis in Chapter 2. We may be interested in what words tend to appear within 
#the same section.


austen_section_words <- austen_books() %>%
  filter(book == "Pride & Prejudice") %>%
  mutate(section = row_number() %/% 10) %>%
  filter(section > 0) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word)

austen_section_words

# count words co-occuring within sections

library(widyr)

word_pairs <- austen_section_words %>%
  pairwise_count(word, section, sort = TRUE)

word_pairs


#We can easily find the words that most often occur with Darcy:

word_pairs %>%
  filter(item1 == "darcy")


#The pairwise_cor() function in widyr lets us find the phi coefficient between words based 
#on how often they appear in the same section

word_cors <- austen_section_words %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, section, sort = TRUE)

word_cors


# we could find the words most correlated with a word like "pounds" using a filter

word_cors %>%
  filter(item1 == "pounds")


word_cors %>%
  filter(item1 %in% c("elizabeth", "pounds", "married", "pride")) %>%
  group_by(item1) %>%
  top_n(6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()


set.seed(2016)

word_cors %>%
  filter(correlation > .15) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

### Challenge
# try to form word correlations with hitler txt
