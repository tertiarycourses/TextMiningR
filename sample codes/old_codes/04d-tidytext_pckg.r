############# tidying text objects

library(tidytext)

text <- readLines(file.choose())
text


# In order to turn it into a tidy text dataset, 
# we first need to put it into a data frame.

ll=length(text)

library(dplyr)
text_df <- data_frame(line = 1:ll, text = text)

text_df
text_df$text


############# Tidying DocumentTermMatrix objects

#  collection of Associated Press newspaper articles included in the topicmodels package.

library(tm)

data("AssociatedPress", package = "topicmodels")
AssociatedPress

#access the terms in the document 

terms <- Terms(AssociatedPress)
head(terms)

#  we would first need to turn it into a data frame with one-token-per-document-per-row

library(dplyr)
library(tidytext)

ap_td <- tidy(AssociatedPress)
ap_td


################## Tidying dfm objects

library(methods)

data("data_corpus_inaugural", package = "quanteda")
inaug_dfm <- quanteda::dfm(data_corpus_inaugural, verbose = FALSE)

inaug_dfm

# turning document-feature matrices into a one-token-per-document-per-row table:

inaug_td <- tidy(inaug_dfm)

inaug_td


inaug_tf_idf <- inaug_td %>%
  bind_tf_idf(term, document, count) %>%
  arrange(desc(tf_idf))

inaug_tf_idf


################## tidy text data into a matrix

# tidied AP dataset and cast it back into a document-term matrix 

ap_td %>%
  cast_dtm(document, term, count)


# cast the table into a dfm object from quanteda’s dfm with cast_dfm().


ap_td %>%
  cast_dfm(term, document, count)



################# tidying corpus objects with metadata

data("acq")
acq
acq[[1]]

acq_td <- tidy(acq)
acq_td


acq_tokens <- acq_td %>%
  select(-places) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word")

# most common words
acq_tokens %>%
  count(word, sort = TRUE)


acq_tokens %>%
  count(id, word) %>%
  bind_tf_idf(word, id, n) %>%
  arrange(desc(tf_idf))

############ mining literacy works


# What are the most commonly used words in Jane Austen's novels

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



#Let's get The Time Machine, The War of the Worlds, The Invisible Man, and The Island of 
#Doctor Moreau. We can access these works using gutenberg_download() and the 
#Gutenberg ID numbers for each novel.

library(gutenbergr)

hgwells <- gutenberg_download(c(35, 36, 5230, 159))



tidy_hgwells <- hgwells %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)


# most common words in these novels of H.G. Wells

tidy_hgwells %>%
  count(word, sort = TRUE)


#Let.s get Jane Eyre, Wuthering Heights, The Tenant of Wildfell Hall, Villette, and 
#Agnes Grey

bronte <- gutenberg_download(c(1260, 768, 969, 9182, 767))

tidy_bronte <- bronte %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_bronte %>%
  count(word, sort = TRUE)



################ mining financial articles


#For instance, performing WebCorpus(GoogleFinanceSource("NASDAQ:MSFT"))) allows us to 
#retrieve the 20 most recent articles related to the Microsoft (MSFT) stock.
# 
# library(tm.plugin.webmining)
# library(purrr)
# 
# company <- c("Microsoft", "Apple", "Google", "Amazon", "Facebook",
#              "Twitter", "IBM", "Yahoo", "Netflix")
# symbol <- c("MSFT", "AAPL", "GOOG", "AMZN", "FB", "TWTR", "IBM", "YHOO", "NFLX")
# 
# download_articles <- function(symbol) {
#   WebCorpus(GoogleFinanceSource(paste0("NASDAQ:", symbol)))
# }
# 
# stock_articles <- data_frame(company = company,
#                              symbol = symbol) %>%
#   mutate(corpus = map(symbol, download_articles))
# 
# 
# stock_articles
